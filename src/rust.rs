use super::*;
use std::borrow::Cow;

pub fn append_rust_type(rust: &mut String, ast: &AST, ty: &RustType) {
    use RustType::*;
    match ty {
        Pair { rust: inner, .. } => append_rust_type(rust, ast, inner),
        Verbatim(text) => rust.push_str(text),
        Bool => rust.push_str("bool"),

        U8 => rust.push_str("u8"),
        U16 => rust.push_str("u16"),
        U32 => rust.push_str("u32"),
        Usize => rust.push_str("usize"),
        U64 => rust.push_str("u64"),

        I8 => rust.push_str("i8"),
        I16 => rust.push_str("i16"),
        I32 => rust.push_str("i32"),
        Isize => rust.push_str("isize"),
        I64 => rust.push_str("i64"),

        F32 => rust.push_str("f32"),
        F64 => rust.push_str("f64"),

        RefStr => rust.push_str("&str"),
        OwnStr => rust.push_str("String"),

        Struct(index) => rust.push_str(&ast.structs[*index].name),
        Enum(index) => rust.push_str(&ast.enums[*index].name),

        DynTrait(index) => {
            rust.push_str("dyn ");
            rust.push_str(&ast.traits[*index].name);
        }

        Ptr(kind, inner) => {
            rust.push_str(kind.path());
            rust.push('<');
            append_rust_type(rust, ast, &inner);
            rust.push('>');
        }

        Vector(inner) => {
            rust.push_str("Vec<");
            append_rust_type(rust, ast, &inner);
            rust.push('>');
        }

        Tuple(types) => {
            rust.push('(');
            for (i, ty) in types.iter().enumerate() {
                append_rust_type(rust, ast, ty);
                if i + 1 < types.len() {
                    rust.push_str(", ");
                } else if types.len() == 1 {
                    rust.push(',');
                }
            }
            rust.push(')');
        }

        Optional(inner) => {
            rust.push_str("Option<");
            append_rust_type(rust, ast, &inner);
            rust.push('>');
        }

        ForeignHandle => rust.push_str("*const u8"),
    }
}

pub fn append_rust_default_val(rust: &mut String, ast: &AST, ty: &RustType) {
    use RustType::*;
    match ty {
        Bool => rust.push_str("false"),

        U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 => rust.push_str("0"),

        F32 | F64 => rust.push_str("0.0"),

        RefStr => rust.push_str("\"\""),
        OwnStr => rust.push_str("String::new()"),

        Struct(index) => {
            let s = &ast.structs[*index];
            rust.push_str(&s.name);
            if s.fields.is_empty() {
                rust.push_str(" {}");
            } else {
                rust.push_str(" { ");
                for (i, f) in s.fields.iter().enumerate() {
                    if i > 0 {
                        rust.push_str(", ");
                    }
                    rust.push_str(&f.name);
                    rust.push_str(": ");
                    append_rust_default_val(rust, ast, &f.ty);
                }
                rust.push_str(" }");
            }
        }

        Tuple(types) => {
            rust.push('(');
            for (i, ty) in types.iter().enumerate() {
                append_rust_default_val(rust, ast, ty);
                if i + 1 < types.len() {
                    rust.push_str(", ");
                } else if types.len() == 1 {
                    rust.push(',');
                }
            }
            rust.push(')');
        }

        ForeignHandle => rust.push_str("std::ptr::null()"),

        _ => rust.push_str("unreachable!()"),
    }
}

impl FnBuilder {
    pub fn write_to_rust(&mut self, ast: &AST, out: &mut String, base_indent: &str) {
        let mut indent = 0;
        let mut callback = |line: &str| {
            if line.starts_with(&['}', ']', ')']) {
                indent -= 1;
            }
            _ = write!(out, "{base_indent}{}{line}\n", "    ".repeat(indent));
            if line.ends_with(&['{', '[', '(']) {
                indent += 1;
            }
        };
        for line in self.take_lines() {
            match line {
                Line::Plain(text) => {
                    text.split('\n').for_each(&mut callback);
                }
                Line::PlainAlt(when_false, when_true, flag) => match flag.get() {
                    false if when_false.is_empty() => continue,
                    false => when_false.split('\n').for_each(&mut callback),
                    true => when_true.split('\n').for_each(&mut callback),
                },
                Line::Decl(name, ty, text) => {
                    let mut decl_ty = String::new();
                    if let Some(ty) = ty {
                        decl_ty.push_str(": ");
                        append_rust_type(&mut decl_ty, ast, &ty);
                    }
                    let text = format!("let {name}{decl_ty} = {text};");
                    text.split('\n').for_each(&mut callback);
                }
            }
        }
    }
}

pub fn is_snake_case(name: &str) -> bool {
    let name = name.trim_matches('_');
    !name.contains("__") && !name.chars().any(char::is_uppercase)
}

pub fn is_camel_case(name: &str) -> bool {
    let name = name.trim_matches('_');
    let mut chars = name.chars();
    !chars.next().map(char::is_lowercase).unwrap_or(false)
        && !name.contains("__")
        && !name.chars().zip(chars).any(|(a, b)| {
            let has_case = |c: char| c.to_uppercase().next() != c.to_lowercase().next();
            (has_case(a) && b == '_') || (a == '_' && has_case(b))
        })
}

pub fn allow_non_snake_case(rust: &mut String, name: &str) {
    if !is_snake_case(name) {
        rust.push_str("\n#[allow(non_snake_case)]");
    }
}

pub fn allow_non_camel_case_types(rust: &mut String, name: &str) {
    if !is_camel_case(name) {
        rust.push_str("\n#[allow(non_camel_case_types)]");
    }
}

pub fn rust_decl_ctor(
    rust: &mut FnBuilder,
    var_name: &str,
    ctor_name: &str,
    fields: &Vec<RustField>,
    item_names: Vec<Cow<'_, str>>,
) {
    if fields.is_empty() {
        rust.decl(var_name, ctor_name.to_string());
    } else if fields.iter().any(|arg| starts_with_digit(&arg.name)) {
        let args = rust.find_args(
            &fields
                .iter()
                .zip(item_names)
                .map(|(f, name)| RustArg {
                    name: name.to_string(),
                    ty: f.ty.clone(),
                })
                .collect(),
            RefInline,
        );
        rust.decl(var_name, format!("{ctor_name}({args})"));
    } else {
        let fields = rust.find_fields(
            fields,
            item_names,
            RefInline,
            &format!("{ctor_name} {{ "),
            " }",
        );
        rust.decl(var_name, fields);
    }
}

pub fn add_common_rust_helpers(helpers: &mut HelperSet<(), String>) {
    helpers.add(
        "_ffi_read",
        r"
fn _ffi_read<T: Copy>(ptr: &mut *const u8) -> T {
    let val = unsafe { (*ptr as *const T).read_unaligned() };
    *ptr = unsafe { ptr.byte_offset(std::mem::size_of::<T>() as isize) };
    val
}
",
    );

    helpers.add(
        "_ffi_write",
        r"
fn _ffi_write<T: Copy>(val: T, buf: &mut Vec<u8>) {
    let ptr = std::ptr::addr_of!(val) as *const u8;
    let len = std::mem::size_of::<T>();
    buf.extend_from_slice(unsafe { std::slice::from_raw_parts(ptr, len) });
}
",
    );

    helpers.add(
        "_ffi_alloc",
        r#"
#[unsafe(no_mangle)]
extern "C" fn _ffi_alloc(len: usize) -> *const u8 {
    Box::into_raw([0 as u8].repeat(len).into_boxed_slice()) as *const u8
}
"#,
    );

    helpers.add(
        "_ffi_dealloc",
        r#"
#[unsafe(no_mangle)]
extern "C" fn _ffi_dealloc(ptr: *mut u8, capacity: usize) {
    drop(unsafe { Vec::from_raw_parts(ptr, 0, capacity) });
}
"#,
    );

    helpers
        .add(
            "_ffi_string_from_host",
            r"
fn _ffi_string_from_host(ptr: *const u8, len: usize) -> String {
    unsafe { String::from_raw_parts(ptr as *mut u8, len, len) }
}
",
        )
        .add_dep("_ffi_alloc");

    helpers
        .add(
            "_ffi_string_to_host",
            r"
fn _ffi_string_to_host(buf: String) -> (*const u8, usize, usize) {
    let buf = std::mem::ManuallyDrop::new(buf.into_bytes());
    (buf.as_ptr(), buf.len(), buf.capacity())
}
",
        )
        .add_dep("_ffi_dealloc");

    helpers
        .add(
            "_ffi_buf_from_host",
            r"
fn _ffi_buf_from_host(ptr: *const u8, end: *const u8) {
    let len = unsafe { end.byte_offset_from(ptr) } as usize;
    drop(unsafe { Vec::from_raw_parts(ptr as *mut u8, 0, len) });
}
",
        )
        .add_dep("_ffi_alloc");

    helpers
        .add(
            "_ffi_buf_to_host",
            r"
fn _ffi_buf_to_host(buf: Vec<u8>) -> (*const u8, usize) {
    let buf = std::mem::ManuallyDrop::new(buf);
    (buf.as_ptr(), buf.capacity())
}
",
        )
        .add_dep("_ffi_dealloc");
}
