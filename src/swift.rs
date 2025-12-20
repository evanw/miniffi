use super::*;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Use this target when the host language is Swift.
///
/// This needs to generate a `.swift` file and a `.h` file. By default, the
/// files will be named `miniffi.swift` and `miniffi.h` and will be written to
/// the directory containing your `Cargo.toml` file. You can customize these
/// paths before calling [`build`](Target::build):
///
/// ```no_run
/// use miniffi::*;
///
/// fn main() {
///     SwiftTarget::new()
///         .write_swift_to("../app/rust.swift")
///         .write_header_to("../app/rust.h")
///         .build();
/// }
/// ```
///
/// If your `src/lib.rs` looks like this:
///
/// ```no_run
/// pub fn add(left: u64, right: u64) -> u64 {
///     left + right
/// }
///
/// # macro_rules! env { ($a:expr) => { $a } }
/// # macro_rules! include { ($a:expr) => { $a } }
/// include!(concat!(env!("OUT_DIR"), "/miniffi.rs"));
/// ```
///
/// You can call that from Swift like this:
///
/// ```text
/// print("1 + 2 =", add(1, 2))
/// ```
///
/// Even though Swift is the host language, the `.h` file is necessary because
/// Swift lacks syntax for specifying certain interactions with the C ABI that
/// Rust uses. Instead of building those features into Swift itself, Apple has
/// decided to make those features work by passing a C header to the Swift
/// compiler instead. This is called using an "Objective-C bridging header"
/// even though it has nothing to do with Objective-C in this case.
///
/// ## Using the command line
///
/// If you are using Swift from the command line, then you can call Rust from
/// Swift by adding the generated `.swift` file, passing the generated `.h`
/// file as the Objective-C bridging header, and also linking to the compiled
/// Rust code. For example:
///
/// ```text
/// # Build the Rust code
/// cargo rustc --crate-type=staticlib
///
/// # Build the Swift code
/// swiftc main.swift miniffi.swift -import-objc-header miniffi.h ./target/debug/libexample.a
/// ```
///
/// ## Using Xcode
///
/// If you're using Swift from Xcode, then the process is more complicated.
/// There are also various ways to do this. Here's the process I use:
///
/// 1. Create a `Makefile` to build your Rust code
///
///     Put the following in a file called `Makefile` in your Rust crate:
///
///     ```makefile
///     Debug:
///     	cargo rustc --crate-type=staticlib
///
///     Release:
///     	cargo rustc --crate-type=staticlib --release
///     ```
///
///     Using an additional script like this makes our life easier later since
///     we can reference it from the `xcodeproj` settings. Note that these
///     indents are intentionally tab characters, not spaces (`make` requires
///     tab characters).
///
/// 2. Tell Xcode to compile the generated code
///
///     Run `make` to cause the build script to run, which will generate
///     `miniffi.swift` and `miniffi.h`. Then in Xcode, add `miniffi.swift` and
///     `miniffi.h` to your project (make sure they are referenced, not copied).
///
///     Include `miniffi.h` in your Objective-C Bridging Header. If you already
///     have a bridging header, use `#include` to include `miniffi.h` there.
///     Otherwise you can go to the **Build Settings** tab in your `xcodeproj`
///     settings and set **Objective-C Bridging Header** directly to `miniffi.h`.
///
/// 3. Tell Xcode to link with the compiled Rust code
///
///     In the **Build Settings** tab of your `xcodeproj` settings, expand
///     **Library Search Paths** into separate **Debug** and **Release** rows.
///     Set the **Debug** path to `./target/debug` and the **Release** path to
///     `./target/release`.
///
///     Go to the **Build Phases** tab of your `xcodeproj` settings. Expand
///     **Link Binary With Libraries**, click the `+` button, click
///     **Add Other...** > **Add Files...**, and select the
///     `./target/debug/libexample.a` file that you built earlier.
///
///     At this point, you should now be able to compile and run your Xcode
///     project and call Rust from Swift! But there's one more step you'll
///     likely want to do...
///
/// 4. Tell Xcode to rebuild your Rust code automatically
///
///     In the **Build Settings** tab of your `xcodeproj` settings, set
///     **User Scripts Sandboxing** to No. We will be building your Rust code
///     which exists on the file system, so we need file system access.
///
///     Go to the **Build Phases** tab of your `xcodeproj` settings. Use the `+`
///     button to add a **New Run Script Phase** and drag it before the
///     **Compile Sources** phase. Expand it and use the following shell script:
///
///     ```sh
///     export PATH="$HOME/.cargo/bin:$PATH"
///     make "$CONFIGURATION"
///     ```
///
///     You can also uncheck the **Based on dependency analysis** checkbox to
///     disable an Xcode warning.
///
///     Using `$CONFIGURATION` in combination with the `Makefile` means the
///     **Debug** build configuration in Xcode will build your Rust code in debug
///     mode (without the `--release` flag), and the **Release** build
///     configuration in Xcode will build your rust code in release mode (with
///     the `--release` flag). The `export PATH` is necessary because Xcode
///     doesn't use your `$PATH` and won't be able to find the `cargo` binary
///     otherwise.
pub struct SwiftTarget {
    common_options: CommonOptions,
    swift_path: PathBuf,
    header_path: PathBuf,
}

impl SwiftTarget {
    pub fn new() -> SwiftTarget {
        SwiftTarget {
            common_options: CommonOptions::default(),
            swift_path: "miniffi.swift".into(),
            header_path: "miniffi.h".into(),
        }
    }

    pub fn write_swift_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.swift_path = path.into();
        self
    }

    pub fn write_header_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.header_path = path.into();
        self
    }
}

pub const SWIFT_KEYWORDS: &[&str] = &[
    "Any",
    "as",
    "associatedtype",
    "associativity",
    "async",
    "await",
    "borrowing",
    "break",
    "case",
    "catch",
    "catch",
    "class",
    "consuming",
    "continue",
    "convenience",
    "default",
    "defer",
    "deinit",
    "didSet",
    "do",
    "dynamic",
    "else",
    "enum",
    "extension",
    "fallthrough",
    "false",
    "fileprivate",
    "final",
    "for",
    "func",
    "get",
    "guard",
    "if",
    "import",
    "in",
    "indirect",
    "infix",
    "init",
    "inout",
    "internal",
    "is",
    "lazy",
    "left",
    "let",
    "mutating",
    "nil",
    "none",
    "nonisolated",
    "nonmutating",
    "open",
    "operator",
    "optional",
    "override",
    "package",
    "postfix",
    "precedence",
    "precedencegroup",
    "prefix",
    "private",
    "protocol",
    "Protocol",
    "public",
    "repeat",
    "required",
    "rethrows",
    "rethrows",
    "return",
    "right",
    "self",
    "Self",
    "set",
    "some",
    "static",
    "struct",
    "subscript",
    "super",
    "switch",
    "throw",
    "throw",
    "throws",
    "true",
    "try",
    "Type",
    "typealias",
    "unowned",
    "var",
    "weak",
    "where",
    "while",
    "willSet",
];

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum HeaderGroup {
    Include,
    Other,
}

impl Compile for SwiftTarget {
    fn common_options(&mut self) -> &mut CommonOptions {
        &mut self.common_options
    }

    fn compile(&self, mut ast: AST, rust_path: PathBuf) -> Vec<FileData> {
        let mut rust_helpers = HelperSet::<(), String>::default();
        let mut swift_helpers = HelperSet::<(), String>::default();
        let mut header_helpers = HelperSet::<HeaderGroup, String>::default();

        // Also need to escape C++ keywords for the Objective-C bridging header
        let all_keywords: Vec<_> = SWIFT_KEYWORDS
            .iter()
            .chain(cpp::CPP_KEYWORDS.iter())
            .map(|x| *x)
            .collect();

        add_common_rust_helpers(&mut rust_helpers);
        ast.rename_keywords(&all_keywords);

        header_helpers.add_group(
            HeaderGroup::Include,
            "<stdint.h>",
            "\n#include <stdint.h>\n",
        );

        header_helpers
            .add_group(
                HeaderGroup::Other,
                "_ffi_alloc",
                "\nvoid* _ffi_alloc(intptr_t len);\n",
            )
            .add_dep_group(HeaderGroup::Include, "<stdint.h>");

        header_helpers
            .add_group(
                HeaderGroup::Other,
                "_ffi_dealloc",
                "\nvoid _ffi_dealloc(const void* ptr, uintptr_t capacity);\n",
            )
            .add_dep_group(HeaderGroup::Include, "<stdint.h>");

        swift_helpers.add(
            "_ffi_read",
            r#"
private func _ffi_read<T>(_ ptr: inout UnsafeRawPointer) -> T {
    let val = ptr.loadUnaligned(fromByteOffset: 0, as: T.self)
    ptr = ptr.advanced(by: MemoryLayout<T>.size)
    return val
}
"#,
        );

        swift_helpers.add(
            "_ffi_write",
            r#"
private func _ffi_write<T>(_ val: T, _ buf: inout ContiguousArray<UInt8>) {
    var val = val
    withUnsafeBytes(of: &val) { val in
        buf.append(contentsOf: val)
    }
}
"#,
        );

        swift_helpers.add(
            "_ffi_swift_drop",
            r#"
@_cdecl("_ffi_swift_drop")
func _ffi_swift_drop(ptr: UnsafeRawPointer?) {
    _ = Unmanaged<AnyObject>.fromOpaque(ptr!).takeRetainedValue()
}
"#,
        );

        swift_helpers.add(
            "_ffi_vec_to_rust",
            r"
private func _ffi_vec_to_rust(_ vec: ContiguousArray<UInt8>) -> UnsafeRawPointer? {
    vec.withUnsafeBytes { vec in
        let buf = UnsafeMutableRawBufferPointer(start: _ffi_alloc(vec.count), count: vec.count)
        buf.copyMemory(from: UnsafeRawBufferPointer(start: vec.baseAddress, count: vec.count))
        return UnsafeRawPointer(buf.baseAddress)
    }
}
",
        );

        swift_helpers.add(
            "_ffi_string_to_rust",
            r"
private func _ffi_string_to_rust(_ str: String) -> (UnsafeRawPointer?, UInt) {
    var str = str
    return str.withUTF8 { str in
        let buf = UnsafeMutableRawBufferPointer(start: _ffi_alloc(str.count), count: str.count)
        buf.copyMemory(from: UnsafeRawBufferPointer(start: str.baseAddress, count: str.count))
        return (UnsafeRawPointer(buf.baseAddress), UInt(buf.count))
    }
}
",
        );

        swift_helpers.add(
            "_ffi_string_from_rust",
            r"
private func _ffi_string_from_rust(_ ptr: UnsafeRawPointer?, _ len: Int, _ cap: UInt) -> String {
    let buf = UnsafeBufferPointer(start: ptr!.assumingMemoryBound(to: UInt8.self), count: len)
    let str = String(decoding: buf, as: UTF8.self)
    _ffi_dealloc(ptr, cap)
    return str
}
",
        );

        let mut rust = format!("// {DO_NOT_EDIT_COMMENT}\n");
        let mut swift = format!("// {DO_NOT_EDIT_COMMENT}\n");

        // Traits
        for t in &ast.traits {
            _ = write!(swift, "\nprotocol {}: AnyObject {{\n", t.name);
            for f in &t.fns {
                _ = write!(swift, "    func {}(", f.name);
                for (i, arg) in f.args.iter().enumerate() {
                    if i > 0 {
                        swift.push_str(", ");
                    }
                    _ = write!(swift, "_ {}: ", arg.name);
                    append_swift_type(&mut swift, &ast, &arg.ty);
                }
                swift.push(')');
                if let Some(returns) = &f.returns {
                    swift.push_str(" -> ");
                    append_swift_type(&mut swift, &ast, &returns.ty);
                }
                swift.push('\n');
            }
            swift.push_str("}\n");
        }

        // Enums
        for (i, e) in ast.enums.iter().enumerate() {
            if !e.has_fields() {
                // Enums without fields are just integers
                _ = write!(swift, "\nenum {}: Int32", e.name);
                swift.push_str(" {\n");
                for v in &e.variants {
                    _ = write!(swift, "    case {} = {}\n", v.name, v.discriminant);
                }
                swift.push_str("}\n");
            } else {
                // Enums with fields map directly to Swift enums
                _ = write!(swift, "\nenum {}", e.name);
                if e.derives_partial_eq {
                    swift.push_str(" : Equatable");
                }
                swift.push_str(" {\n");
                for v in &e.variants {
                    let indirect = match swift_variant_needs_indirect(&ast, i, v) {
                        false => "",
                        true => "indirect ",
                    };
                    _ = write!(swift, "    {indirect}case {}", v.name);
                    if !v.fields.is_empty() {
                        swift.push('(');
                        for (j, f) in v.fields.iter().enumerate() {
                            if j > 0 {
                                swift.push_str(", ");
                            }
                            if !starts_with_digit(&f.name) {
                                _ = write!(swift, "{}: ", f.name);
                            }
                            append_swift_type(&mut swift, &ast, &f.ty);
                        }
                        swift.push(')');
                    }
                    swift.push('\n');
                }
                if e.derives_partial_eq
                    && e.variants
                        .iter()
                        .any(|v| v.fields.iter().any(|f| swift_type_contains_tuple(&f.ty)))
                {
                    swift.push_str(&generate_operator_eq_enum(e));
                }
                swift.push_str("}\n");
            }
        }

        // Structs
        for (i, s) in ast.structs.iter().enumerate() {
            let use_class = swift_type_is_infinite_size(&ast, &RustType::Struct(i));
            let keyword = if use_class { "class" } else { "struct" };
            _ = write!(swift, "\n{keyword} {}", s.name);
            if s.derives_partial_eq {
                swift.push_str(" : Equatable");
            }
            swift.push_str(" {\n");
            for f in &s.fields {
                _ = write!(swift, "    var {}: ", with_digit_prefix(&f.name));
                append_swift_type(&mut swift, &ast, &f.ty);
                swift.push('\n');
            }
            if use_class {
                swift.push_str(&generate_init(&ast, s));
            }
            if s.derives_partial_eq
                && (use_class || s.fields.iter().any(|f| swift_type_contains_tuple(&f.ty)))
            {
                swift.push_str(&generate_operator_eq_struct(s));
            }
            swift.push_str("}\n");
        }

        // Constants
        if !ast.consts.is_empty() {
            swift.push('\n');
            for c in &ast.consts {
                _ = write!(swift, "let {}: ", c.name);
                append_swift_type(&mut swift, &ast, &c.ty);
                swift.push_str(" = ");
                append_swift_val(&mut swift, &c.val);
                swift.push('\n');
            }
        }

        let mut ctx = SwiftCtx {
            rust_helpers,
            swift_helpers,
            header_helpers,
            ..SwiftCtx::default()
        };

        // Functions
        for f in &ast.fns {
            generate_swift_to_rust_fn(&ast, &mut ctx, f, &mut swift, None);
        }

        for it in ctx.rust_helpers.code_in_order() {
            rust.push_str(it);
        }

        for it in ctx.swift_helpers.code_in_order() {
            swift.push_str(it);
        }

        let header_code = ctx.header_helpers.code_by_group_in_order();
        let mut header = format!("// {DO_NOT_EDIT_COMMENT}\n");
        header.push_str("\n#pragma once\n");

        if let Some(code) = header_code.get(&HeaderGroup::Include) {
            for it in code {
                header.push_str(it);
            }
        }

        header.push_str("\n#ifdef __cplusplus\n");
        header.push_str("extern \"C\" {\n");
        header.push_str("#endif\n");

        if let Some(code) = header_code.get(&HeaderGroup::Other) {
            for it in code {
                header.push_str(it);
            }
        }

        header.push_str("\n#ifdef __cplusplus\n");
        header.push_str("}\n");
        header.push_str("#endif\n");

        vec![
            FileData {
                path: rust_path,
                contents: rust,
            },
            FileData {
                path: self.swift_path.clone(),
                contents: swift,
            },
            FileData {
                path: self.header_path.clone(),
                contents: header,
            },
        ]
    }
}

fn swift_type_contains_tuple(ty: &RustType) -> bool {
    use RustType::*;
    match ty {
        Pair { other, .. } => swift_type_contains_tuple(other),
        Vector(inner_ty) => swift_type_contains_tuple(inner_ty),
        Optional(inner_ty) => swift_type_contains_tuple(inner_ty),
        Ptr(_, inner_ty) => swift_type_contains_tuple(inner_ty),
        Tuple(types) => types.len() != 1,
        _ => false,
    }
}

fn generate_init(ast: &AST, s: &RustStruct) -> String {
    let mut swift = "\n    init(".to_string();
    for (i, f) in s.fields.iter().enumerate() {
        if i > 0 {
            swift.push_str(", ");
        }
        swift.push_str(&f.name);
        swift.push_str(": ");
        append_swift_type(&mut swift, ast, &f.ty);
    }
    swift.push_str(") {\n");
    for f in &s.fields {
        _ = write!(swift, "        self.{} = {}\n", f.name, f.name);
    }
    swift.push_str("    }\n");
    swift
}

fn emit_eq(parts: &mut Vec<String>, ty: &RustType, a: &str, b: &str) {
    use RustType::*;

    if !swift_type_contains_tuple(ty) {
        parts.push(format!("{a} == {b}"));
        return;
    }

    match ty {
        Pair { other, .. } => emit_eq(parts, other, a, b),
        Ptr(_, inner_ty) => emit_eq(parts, inner_ty, a, b),

        // Unfortunately tuples in Swift don't currently implement
        // "Equatable", so structs containing tuples cannot automatically
        // derive "Equatable". There have been many attempts to fix this
        // but somehow it's still broken? Works fine in other languages.
        // Gotta work around it ourselves...
        Tuple(types) if types.len() != 1 => {
            for (i, ty) in types.iter().enumerate() {
                let a = format!("{a}.{i}");
                let b = format!("{b}.{i}");
                emit_eq(parts, ty, &a, &b);
            }
        }

        Optional(inner_ty) => {
            let mut inner_parts = Vec::new();
            emit_eq(&mut inner_parts, inner_ty, "a", "b");
            parts.push(if inner_parts.is_empty() {
                    format!("({a} != nil) == ({b} != nil)")
                } else {
                    format!(
                        "{{ if let a = {a}, let b = {b} {{ {} }} else {{ {a} == nil && {b} == nil }} }}()",
                        inner_parts.join(" && ")
                    )
                });
        }

        Vector(inner_ty) => {
            let mut inner_parts = Vec::new();
            emit_eq(&mut inner_parts, inner_ty, "a", "b");
            parts.push(if inner_parts.is_empty() {
                format!("{a}.count == {b}.count")
            } else {
                format!(
                    "{a}.elementsEqual({b}, by: {{ a, b in {} }})",
                    inner_parts.join(" && ")
                )
            });
        }

        _ => parts.push(format!("{a} == {b}")),
    }
}

fn emit_eq_return(parts: &[String], base_indent: &str) -> String {
    if parts.is_empty() {
        "return true".to_string()
    } else if parts.len() == 1 || parts.iter().map(|x| x.len()).sum::<usize>() < 100 {
        format!("return {}", parts.join(" && "))
    } else {
        let mut swift = "return (".to_string();
        for (i, part) in parts.iter().enumerate() {
            if i > 0 {
                swift.push_str(" &&");
            }
            _ = write!(swift, "\n{base_indent}    {part}");
        }
        _ = write!(swift, "\n{base_indent})");
        swift
    }
}

fn generate_operator_eq_struct(s: &RustStruct) -> String {
    let mut swift = String::new();
    let mut parts = Vec::new();

    for f in &s.fields {
        let name = with_digit_prefix(&f.name);
        let a = format!("a.{}", name);
        let b = format!("b.{}", name);
        emit_eq(&mut parts, &f.ty, &a, &b);
    }

    _ = write!(
        swift,
        "\n    static func == (a: {}, b: {}) -> Bool {{\n",
        s.name, s.name
    );
    _ = write!(swift, "        {}\n", emit_eq_return(&parts, "        "));

    swift.push_str("    }\n");
    swift
}

fn generate_operator_eq_enum(e: &RustEnum) -> String {
    let mut locals = NameSet::default();
    let mut swift = String::new();

    locals.add("a".to_string());
    locals.add("b".to_string());
    _ = write!(
        swift,
        "\n    static func == (a: {}, b: {}) -> Bool {{\n",
        e.name, e.name
    );
    swift.push_str("        switch (a, b) {\n");

    for v in &e.variants {
        if v.fields.is_empty() {
            _ = write!(swift, "        case (.{}, .{}):\n", v.name, v.name);
            swift.push_str("            return true\n");
            continue;
        }

        let mut branch_locals = locals.clone();
        let mut a_names = Vec::new();
        let mut b_names = Vec::new();
        let mut parts = Vec::new();

        _ = write!(swift, "        case let (.{}(", v.name);
        for (i, f) in v.fields.iter().enumerate() {
            if i > 0 {
                swift.push_str(", ");
            }
            if !starts_with_digit(&f.name) {
                _ = write!(swift, "{}: ", f.name);
            }
            let name = branch_locals.create(&format!("a{}", f.name));
            swift.push_str(&name);
            a_names.push(name);
        }

        _ = write!(swift, "), .{}(", v.name);
        for (i, f) in v.fields.iter().enumerate() {
            if i > 0 {
                swift.push_str(", ");
            }
            if !starts_with_digit(&f.name) {
                _ = write!(swift, "{}: ", f.name);
            }
            let name = branch_locals.create(&format!("b{}", f.name));
            swift.push_str(&name);
            b_names.push(name);
        }

        for ((a, b), f) in a_names.iter().zip(&b_names).zip(&v.fields) {
            emit_eq(&mut parts, &f.ty, a, b);
        }
        swift.push_str(")):\n");
        _ = write!(
            swift,
            "            {}\n",
            emit_eq_return(&parts, "            ")
        );
    }

    swift.push_str("        default:\n");
    swift.push_str("            return false\n");
    swift.push_str("        }\n");
    swift.push_str("    }\n");
    swift
}

#[derive(Default)]
struct SwiftCtx {
    helper_names: NameSet,
    rust_helpers: HelperSet<(), String>,
    swift_helpers: HelperSet<(), String>,
    header_helpers: HelperSet<HeaderGroup, String>,
    multi_ret_helpers: HashMap<Vec<RustType>, String>,
    trait_to_rust_helpers: HashMap<usize, String>,
    trait_to_swift_helpers: HashMap<(RustPtr, usize), String>,
    vec_to_rust_helpers: HashMap<RustType, (String, String)>,
    vec_to_swift_helpers: HashMap<RustType, (String, String)>,
    box_to_rust_helpers: HashMap<RustType, (String, String)>,
    box_to_swift_helpers: HashMap<RustType, (String, String)>,
    enum_to_rust_helpers: HashMap<usize, (String, String)>,
    enum_to_swift_helpers: HashMap<usize, (String, String)>,
}

#[derive(Default)]
struct Transform {
    swift: FnBuilder,
    rust: FnBuilder,
    ffi_args: Vec<RustArg>,
    buf: Option<Rc<SharedBuf>>,
    buf_status: BufStatus,
    buf_ref: &'static str,
}

struct TraitInfo<'a> {
    t: &'a RustTrait,
    kind: RustPtr,
}

fn generate_swift_to_rust_fn(
    ast: &AST,
    ctx: &mut SwiftCtx,
    f: &RustFn,
    swift: &mut String,
    trait_info: Option<TraitInfo>,
) {
    let ffi_name = ctx.helper_names.create(&match &trait_info {
        None => format!("_ffi_fn_{}", f.name),
        Some(info) => format!("_ffi_{:?}_{}__{}", info.kind, info.t.name, f.name),
    });
    let mut names = NameSet::default();
    for arg in &f.args {
        names.add(arg.name.clone());
    }
    if let Some(ret) = &f.returns {
        names.add(ret.name.clone());
    }

    // Transform the arguments
    let mut arg_tfm = Transform::default();
    if let Some(info) = &trait_info {
        arg_tfm.rust.line(format!(
            "let _self = unsafe {{ &*(_self as *const {}<dyn {}>) }};",
            info.kind.path(),
            info.t.name
        ));
    }
    for arg in &f.args {
        arg_tfm.swift.mark_pure(&arg.name);
        transform_to_rust(ast, ctx, &mut names, &mut arg_tfm, &arg.name, &arg.ty);
    }
    arg_tfm.swift.insert_deferred_lines_here();

    // Generate the Rust call to the FFI function
    let mut rust_call = String::new();
    if trait_info.is_some() {
        rust_call.push_str("_self.");
    }
    _ = write!(rust_call, "{}(", f.name);
    rust_call.push_str(&arg_tfm.rust.find_args(&f.args, RefInline));
    rust_call.push(')');

    // Transform the result
    let mut ret_tfm = Transform::default();
    if let Some(ret) = &f.returns {
        ret_tfm.rust.decl(&ret.name, rust_call);
        transform_to_swift(ast, ctx, &mut names, &mut ret_tfm, &ret.name, &ret.ty);
    } else {
        rust_call.push(';');
        ret_tfm.rust.line(rust_call);
    }

    // Generate the Swift call to the FFI function
    let mut swift_call = format!("{ffi_name}(");
    if trait_info.is_some() {
        swift_call.push_str("_ffi");
        if !arg_tfm.ffi_args.is_empty() {
            swift_call.push_str(", ");
        }
    }
    swift_call.push_str(&arg_tfm.swift.find_args(&arg_tfm.ffi_args, RefInline));
    swift_call.push(')');

    // Header
    {
        let mut header = String::new();
        let mut header_deps = HashSet::new();
        let return_ty = match &ret_tfm.ffi_args[..] {
            [] => None,
            [value] => Some(Cow::Borrowed(&value.ty)),
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                header_deps.insert((HeaderGroup::Other, ty_name.clone()));
                Some(Cow::Owned(RustType::Verbatim(ty_name)))
            }
        };
        header.push('\n');
        append_c_signature(
            &mut header,
            &mut header_deps,
            return_ty.as_deref(),
            &ffi_name,
            trait_info.as_ref().map(|_| &RustType::ForeignHandle),
            &arg_tfm.ffi_args,
        );
        header.push_str(";\n");
        ctx.header_helpers
            .add_group(HeaderGroup::Other, &ffi_name, header)
            .add_deps_group(header_deps)
            .mark_used();
    }

    // Swift
    {
        // Handle the return values
        let mut fb = arg_tfm.swift;
        match &ret_tfm.ffi_args[..] {
            [] => fb.line(swift_call),
            [arg] => fb.decl(&arg.name, swift_call),
            _ => {
                let ret = names.create("multi_ret");
                fb.decl(&ret, swift_call);
                for (i, arg) in ret_tfm.ffi_args.iter().enumerate() {
                    fb.decl(&arg.name, format!("{ret}._{i}"));
                }
            }
        };

        // Return from the function
        fb.extend(ret_tfm.swift);
        fb.insert_deferred_lines_here();
        if let Some(ret) = &f.returns {
            let code = fb.find(&ret.name, &ret.ty, RefInline).code;
            fb.line(format!("return {code}"));
        }

        // Write out the final function
        let indent = match &trait_info {
            None => "    ",
            Some(_) => "        ",
        };
        _ = write!(swift, "\n{}func {}(", &indent[4..], f.name);
        for (i, arg) in f.args.iter().enumerate() {
            if i > 0 {
                swift.push_str(", ");
            }
            _ = write!(swift, "_ {}: ", arg.name);
            append_swift_type(swift, ast, &arg.ty);
        }
        swift.push(')');
        if let Some(returns) = &f.returns {
            swift.push_str(" -> ");
            append_swift_type(swift, ast, &returns.ty);
        }
        swift.push_str(" {\n");
        fb.write_to_swift(ast, swift, indent);
        _ = write!(swift, "{}}}\n", &indent[4..]);
    }

    // Rust
    {
        // Return from the function
        let mut fb = arg_tfm.rust;
        fb.extend(ret_tfm.rust);
        fb.insert_deferred_lines_here();
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                let code = fb.find(&arg.name, &arg.ty, RefInline).code;
                fb.line(code);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                let args = fb.find_args(&ret_tfm.ffi_args, RefInline);
                fb.line(format!("{ty_name}({args})"));
            }
        }

        // Write out the final function
        let mut rust = String::new();
        rust.push_str("\n#[unsafe(no_mangle)]\n");
        _ = write!(rust, "extern \"C\" fn {ffi_name}(");
        if trait_info.is_some() {
            rust.push_str("_self: *const u8");
        }
        for (i, arg) in arg_tfm.ffi_args.iter().enumerate() {
            if trait_info.is_some() || i > 0 {
                rust.push_str(", ");
            }
            _ = write!(rust, "{}: ", arg.name);
            append_rust_type(&mut rust, ast, &arg.ty);
        }
        rust.push(')');
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                rust.push_str(" -> ");
                append_rust_type(&mut rust, ast, &arg.ty);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(rust, " -> {ty_name}");
            }
        }
        rust.push_str(" {\n");
        fb.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&ffi_name, rust).mark_used();
    }
}

fn generate_rust_to_swift_fn(
    ast: &AST,
    ctx: &mut SwiftCtx,
    t: &RustTrait,
    f: &RustFn,
    rust: &mut String,
) {
    let ffi_name = ctx
        .helper_names
        .create(&format!("_ffi_swift_{}__{}", t.name, f.name));
    let mut names = NameSet::default();
    for arg in &f.args {
        names.add(arg.name.clone());
    }
    if let Some(ret) = &f.returns {
        names.add(ret.name.clone());
    }

    // Transform the arguments
    let mut arg_tfm = Transform::default();
    let self_code = format!(
        "Unmanaged<AnyObject>.fromOpaque(_self!).takeUnretainedValue() as! {}",
        t.name
    );
    arg_tfm.swift.line(format!("let _self = {self_code}"));
    for arg in &f.args {
        arg_tfm.rust.mark_pure(&arg.name);
        transform_to_swift(ast, ctx, &mut names, &mut arg_tfm, &arg.name, &arg.ty);
    }
    arg_tfm.rust.insert_deferred_lines_here();

    // Generate the Swift call to the FFI function
    let mut swift_call = format!("_self.{}(", f.name);
    swift_call.push_str(&arg_tfm.swift.find_args(&f.args, RefInline));
    swift_call.push(')');

    // Transform the result
    let mut ret_tfm = Transform::default();
    if let Some(ret) = &f.returns {
        ret_tfm.swift.decl(&ret.name, swift_call);
        transform_to_rust(ast, ctx, &mut names, &mut ret_tfm, &ret.name, &ret.ty);
    } else {
        ret_tfm.swift.line(swift_call);
    }

    // Generate the Rust call to the FFI function
    let mut rust_call = format!("unsafe {{ {ffi_name}(self.0");
    if !arg_tfm.ffi_args.is_empty() {
        rust_call.push_str(", ");
    }
    rust_call.push_str(&arg_tfm.rust.find_args(&arg_tfm.ffi_args, RefInline));
    rust_call.push_str(") }");

    // Rust
    {
        // Handle the return values
        let mut fb = arg_tfm.rust;
        match &ret_tfm.ffi_args[..] {
            [] => {
                rust_call.push(';');
                fb.line(rust_call);
            }
            [arg] => fb.decl(&arg.name, rust_call),
            _ => {
                let ret = names.create("multi_ret");
                fb.decl(&ret, rust_call);
                for (i, arg) in ret_tfm.ffi_args.iter().enumerate() {
                    fb.decl(&arg.name, format!("{ret}.{i}"));
                }
            }
        };

        // Return from the function
        fb.extend(ret_tfm.rust);
        fb.insert_deferred_lines_here();
        if let Some(ret) = &f.returns {
            let code = fb.find(&ret.name, &ret.ty, RefInline).code;
            fb.line(code);
        }

        // Write out the final function
        _ = write!(rust, "\n    fn {}(&self", f.name);
        for arg in &f.args {
            _ = write!(rust, ", {}: ", arg.name);
            append_rust_type(rust, ast, &arg.ty);
        }
        rust.push(')');
        if let Some(returns) = &f.returns {
            rust.push_str(" -> ");
            append_rust_type(rust, ast, &returns.ty);
        }
        rust.push_str(" {\n");
        _ = write!(
            rust,
            "        unsafe extern \"C\" {{ fn {ffi_name}(_: *const u8"
        );
        for arg in &arg_tfm.ffi_args {
            _ = write!(rust, ", {}: ", arg.name);
            append_rust_type(rust, ast, &arg.ty);
        }
        match &ret_tfm.ffi_args[..] {
            [] => rust.push(')'),
            [arg] => {
                rust.push_str(") -> ");
                append_rust_type(rust, ast, &arg.ty);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(rust, ") -> {ty_name}");
            }
        }
        rust.push_str("; }\n");
        fb.write_to_rust(ast, rust, "        ");
        rust.push_str("    }\n");
    }

    // Swift
    {
        // Return from the function
        let mut fb = arg_tfm.swift;
        fb.extend(ret_tfm.swift);
        fb.insert_deferred_lines_here();
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                let code = fb.find(&arg.name, &arg.ty, RefInline).code;
                fb.line(format!("return {code}"));
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                let args = fb.find_fields(
                    &ret_tfm
                        .ffi_args
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| RustField {
                            name: format!("_{i}"),
                            ty: arg.ty.clone(),
                        })
                        .collect(),
                    ret_tfm
                        .ffi_args
                        .iter()
                        .map(|arg| arg.name.as_str().into())
                        .collect(),
                    RefInline,
                    &format!("return {ty_name}("),
                    ")",
                );
                fb.line(args);
            }
        }

        // Write out the final function
        let mut swift = "\n@_cdecl(".to_string();
        append_swift_quoted(&mut swift, &ffi_name);
        _ = write!(swift, ")\nfunc {ffi_name}(_self: UnsafeRawPointer?");
        for arg in &arg_tfm.ffi_args {
            _ = write!(swift, ", {}: ", arg.name);
            append_swift_type(&mut swift, ast, &arg.ty);
        }
        swift.push(')');
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                swift.push_str(" -> ");
                append_swift_type(&mut swift, ast, &arg.ty);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(swift, " -> {ty_name}");
            }
        }
        swift.push_str(" {\n");
        fb.write_to_swift(ast, &mut swift, "    ");
        swift.push_str("}\n");
        ctx.swift_helpers.add(&ffi_name, swift).mark_used();
    }
}

fn transform_to_rust(
    ast: &AST,
    ctx: &mut SwiftCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    name: &str,
    ty: &RustType,
) {
    use RustType::*;

    fn add_ffi_arg(ast: &AST, ctx: &mut SwiftCtx, tfm: &mut Transform, name: &str, ty: &RustType) {
        match tfm.buf_status {
            BufStatus::Outside => tfm.ffi_args.push(RustArg {
                name: name.to_string(),
                ty: ty.clone(),
            }),
            BufStatus::Inside => {
                let buf = tfm.buf.as_ref().unwrap();

                // Swift (write)
                let code = tfm.swift.find(name, ty, RefInline).code;
                tfm.swift
                    .line(format!("_ffi_write({code}, &{})", buf.buf_name()));
                ctx.swift_helpers.mark_used("_ffi_write");

                // Rust (read)
                let mut rust = "_ffi_read::<".to_string();
                append_rust_type(&mut rust, ast, ty);
                _ = write!(rust, ">({}{})", tfm.buf_ref, buf.end_name());
                tfm.rust.decl(name, rust);
                ctx.rust_helpers.mark_used("_ffi_read");
            }
        }
    }

    match ty {
        Bool | U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 | F32 | F64
        | ForeignHandle => add_ffi_arg(ast, ctx, tfm, name, ty),

        RefStr | OwnStr => {
            let swift_code = tfm.swift.find(name, ty, RefInline).code;
            let ptr_name = names.create(&format!("{name}_ptr"));
            let len_name = names.create(&format!("{name}_len"));
            let opt_ref = match ty {
                RefStr => "&",
                _ => "",
            };
            tfm.swift.line(format!(
                "let ({ptr_name}, {len_name}) = _ffi_string_to_rust({swift_code});"
            ));
            ctx.header_helpers
                .mark_used_group(HeaderGroup::Other, "_ffi_alloc");
            ctx.swift_helpers.mark_used("_ffi_string_to_rust");
            add_ffi_arg(ast, ctx, tfm, &ptr_name, &ForeignHandle);
            add_ffi_arg(ast, ctx, tfm, &len_name, &Usize);
            let len_code = tfm.rust.find(&len_name, &Usize, RefInline).code;
            let ptr_code = tfm.rust.find(&ptr_name, &ForeignHandle, RefInline).code;
            tfm.rust.decl(
                name,
                format!("{opt_ref}_ffi_string_from_host({ptr_code}, {len_code})"),
            );
            ctx.rust_helpers.mark_used("_ffi_string_from_host");
        }

        Enum(enum_index) => {
            let swift = tfm.swift.find(name, ty, RefInline);
            let e = &ast.enums[*enum_index];
            let (swift_helper, rust_helper) = enum_to_rust_helper(ast, ctx, *enum_index);
            if !e.has_fields() {
                let raw_name = names.create(&format!("{name}_raw"));
                tfm.swift.maybe_pure_decl(
                    swift.pure,
                    &raw_name,
                    format!("{}.rawValue", swift.code),
                );
                add_ffi_arg(ast, ctx, tfm, &raw_name, &I32);
                tfm.rust.decl(name, format!("{rust_helper}({raw_name})"));
            } else {
                let buf = ensure_swift_buf(ctx, names, tfm);
                tfm.swift.line(format!(
                    "{swift_helper}({}, &{})",
                    swift.code,
                    buf.buf_name()
                ));
                tfm.rust.decl(
                    name,
                    format!("{rust_helper}({}{})", tfm.buf_ref, buf.end_name()),
                );
            }
        }

        Struct(struct_index) => {
            let swift = tfm.swift.find(name, ty, RefMany);
            let s = &ast.structs[*struct_index];
            let mut item_names = Vec::new();
            for f in &s.fields {
                let item_name = names.create(&format!("{name}_{}", f.name));
                tfm.swift.maybe_pure_decl(
                    swift.pure,
                    &item_name,
                    format!("{}.{}", swift.code, with_digit_prefix(&f.name)),
                );
                transform_to_rust(ast, ctx, names, tfm, &item_name, &f.ty);
                item_names.push(item_name.into());
            }
            rust_decl_ctor(&mut tfm.rust, name, &s.name, &s.fields, item_names);
        }

        Tuple(types) => {
            let swift = tfm.swift.find(name, ty, RefMany);
            let mut item_args = Vec::new();
            for (i, item_ty) in types.iter().enumerate() {
                let item_name = names.create(&format!("{name}_{i}"));
                let swift_code = match types.len() {
                    1 => tfm.swift.find(name, item_ty, RefInline).code,
                    _ => format!("{}.{i}", swift.code),
                };
                tfm.swift
                    .maybe_pure_decl(swift.pure, &item_name, swift_code);
                transform_to_rust(ast, ctx, names, tfm, &item_name, &item_ty);
                item_args.push(RustArg {
                    name: item_name,
                    ty: item_ty.clone(),
                });
            }
            let mut rust_code = tfm.rust.find_args(&item_args, RefInline);
            if types.len() == 1 {
                rust_code.push(',');
            }
            tfm.rust.decl(name, format!("({rust_code})"));
        }

        Ptr(kind, inner_ty) => {
            if let DynTrait(trait_index) = &**inner_ty {
                let swift_code = tfm.swift.find(name, ty, RefInline).code;
                let ptr_name = names.create(&format!("{name}_ptr"));
                let rust_helper = trait_to_rust_helper(ast, ctx, *trait_index);
                let swift_code = format!(
                    "UnsafeRawPointer(Unmanaged.passRetained({swift_code} as AnyObject).toOpaque())"
                );
                tfm.swift.decl(&ptr_name, swift_code);
                add_ffi_arg(ast, ctx, tfm, &ptr_name, &ForeignHandle);
                let ptr_code = tfm.rust.find(&ptr_name, &ForeignHandle, RefInline).code;
                tfm.rust.decl(
                    name,
                    format!("{}::new({rust_helper}({ptr_code}))", kind.path()),
                );
            } else if *kind == RustPtr::Box {
                let swift_code = tfm.swift.find(name, ty, RefMany).code;
                let (swift_helper, rust_helper) = box_to_rust_helper(ast, ctx, inner_ty);
                let buf = ensure_swift_buf(ctx, names, tfm);
                tfm.swift
                    .line(format!("{swift_helper}({swift_code}, &{})", buf.buf_name()));
                tfm.rust.decl(
                    name,
                    format!("{rust_helper}({}{})", tfm.buf_ref, buf.end_name()),
                );
            } else {
                unreachable!()
            }
        }

        Vector(inner_ty) => {
            let swift_code = tfm.swift.find(name, ty, RefMany).code;
            let len_name = names.create(&format!("{name}_len"));
            let (swift_helper, rust_helper) = vec_to_rust_helper(ast, ctx, inner_ty);
            let buf = ensure_swift_buf(ctx, names, tfm);
            tfm.swift
                .decl(&len_name, format!("UInt({swift_code}.count)"));
            add_ffi_arg(ast, ctx, tfm, &len_name, &Usize);
            tfm.swift
                .line(format!("{swift_helper}({swift_code}, &{})", buf.buf_name()));
            let len_code = tfm.rust.find(&len_name, ty, RefInline).code;
            tfm.rust.decl(
                name,
                format!(
                    "{rust_helper}({len_code}, {}{})",
                    tfm.buf_ref,
                    buf.end_name()
                ),
            );
        }

        Optional(inner_ty) => {
            let swift_code = tfm.swift.find(name, ty, RefMany).code;
            let has_name = names.create(&format!("has_{name}"));
            let val_name = names.create(&format!("{name}_val"));
            ensure_swift_buf(ctx, names, tfm);

            tfm.swift.decl(&has_name, format!("{swift_code} != nil"));
            add_ffi_arg(ast, ctx, tfm, &has_name, &Bool);

            let mut rust = FnBuilder::default();
            let branch = format!("if let {val_name} = {swift_code} {{");
            tfm.swift.line(branch.clone());
            {
                let old = tfm.buf_status;
                tfm.buf_status = BufStatus::Inside;
                std::mem::swap(&mut tfm.rust, &mut rust);
                transform_to_rust(ast, ctx, names, tfm, &val_name, inner_ty);
                std::mem::swap(&mut tfm.rust, &mut rust);
                tfm.buf_status = old;
            }
            match tfm.swift.pop_line_if(|x| x == &Line::Plain(branch)) {
                None => tfm.swift.line("}".into()),
                Some(_) => {} // Avoid an unused variable warning in Swift
            }

            let has_code = tfm.rust.find(&has_name, ty, RefInline).code;
            let val_code = rust.find(&val_name, ty, RefInline).code;
            if rust.is_empty() {
                tfm.rust
                    .decl(name, format!("{has_code}.then(|| {val_code})"));
            } else {
                rust.insert_deferred_lines_here();
                rust.line(val_code);
                tfm.rust.line(format!("let {name} = {has_code}.then(|| {{"));
                tfm.rust.extend(rust);
                tfm.rust.line("});".to_string());
            }
        }

        Pair { .. } | Verbatim(_) | DynTrait(_) => unreachable!(),
    }
}

fn transform_to_swift(
    ast: &AST,
    ctx: &mut SwiftCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    name: &str,
    ty: &RustType,
) {
    use RustType::*;

    fn add_ffi_arg(ast: &AST, ctx: &mut SwiftCtx, tfm: &mut Transform, name: &str, ty: &RustType) {
        match tfm.buf_status {
            BufStatus::Outside => tfm.ffi_args.push(RustArg {
                name: name.to_string(),
                ty: ty.clone(),
            }),
            BufStatus::Inside => {
                let buf = tfm.buf.as_ref().unwrap();

                // Rust (write)
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!(
                    "_ffi_write({code}, {}{});",
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                ctx.rust_helpers.mark_used("_ffi_write");

                // Swift (read)
                let mut swift = String::new();
                _ = write!(swift, "_ffi_read(&{}) as ", buf.end_name());
                append_swift_type(&mut swift, ast, ty);
                tfm.swift.decl(name, swift);
                ctx.swift_helpers.mark_used("_ffi_read");
            }
        }
    }

    match ty {
        Bool | U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 | F32 | F64
        | ForeignHandle => add_ffi_arg(ast, ctx, tfm, name, ty),

        RefStr | OwnStr => {
            let mut rust_code = tfm.rust.find(name, ty, RefInline).code;
            let ptr_name = names.create(&format!("{name}_ptr"));
            let len_name = names.create(&format!("{name}_len"));
            let cap_name = names.create(&format!("{name}_cap"));
            if let RefStr = ty {
                rust_code.push_str(".into()");
            }
            tfm.rust.line(format!(
                "let ({ptr_name}, {len_name}, {cap_name}) = _ffi_string_to_host({rust_code});"
            ));
            ctx.rust_helpers.mark_used("_ffi_string_to_host");
            add_ffi_arg(ast, ctx, tfm, &ptr_name, &ForeignHandle);
            add_ffi_arg(ast, ctx, tfm, &len_name, &Usize);
            add_ffi_arg(ast, ctx, tfm, &cap_name, &Usize);
            let cap_code = tfm.swift.find(&cap_name, &Usize, RefInline).code;
            let len_code = tfm.swift.find(&len_name, &Usize, RefInline).code;
            let ptr_code = tfm.swift.find(&ptr_name, &ForeignHandle, RefInline).code;
            tfm.swift.decl(
                name,
                format!("_ffi_string_from_rust({ptr_code}, Int({len_code}), {cap_code})"),
            );
            ctx.swift_helpers.mark_used("_ffi_string_from_rust");
            ctx.header_helpers
                .mark_used_group(HeaderGroup::Other, "_ffi_dealloc");
        }

        Enum(enum_index) => {
            let rust = tfm.rust.find(name, ty, RefInline);
            let e = &ast.enums[*enum_index];
            if !e.has_fields() {
                let raw_name = names.create(&format!("{name}_raw"));
                tfm.rust
                    .maybe_pure_decl(rust.pure, &raw_name, format!("{} as i32", rust.code));
                add_ffi_arg(ast, ctx, tfm, &raw_name, &I32);
                tfm.swift
                    .decl(name, format!("{}(rawValue: {raw_name})!", e.name));
            } else {
                let (swift_helper, rust_helper) = enum_to_swift_helper(ast, ctx, *enum_index);
                let buf = ensure_rust_buf(ctx, names, tfm);
                tfm.rust.line(format!(
                    "{rust_helper}({}, {}{});",
                    rust.code,
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                tfm.swift
                    .decl(name, format!("{swift_helper}(&{})", buf.end_name()));
            }
        }

        Struct(struct_index) => {
            let rust = tfm.rust.find(name, ty, RefMany);
            let s = &ast.structs[*struct_index];
            let mut item_names = Vec::new();
            for f in &s.fields {
                let item_name = names.create(&format!("{name}_{}", f.name));
                tfm.rust.maybe_pure_decl(
                    rust.pure,
                    &item_name,
                    format!("{}.{}", rust.code, f.name),
                );
                transform_to_swift(ast, ctx, names, tfm, &item_name, &f.ty);
                item_names.push(item_name.into());
            }
            if s.fields.is_empty() {
                // Avoid an unused variable warning in Rust
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!("_ = {code};"));
            }
            let fields = tfm.swift.find_fields(
                &s.fields
                    .iter()
                    .map(|f| RustField {
                        name: with_digit_prefix(&f.name).into_owned(),
                        ty: f.ty.clone(),
                    })
                    .collect(),
                item_names,
                RefInline,
                &format!("{}(", s.name),
                ")",
            );
            tfm.swift.decl(name, fields);
        }

        Tuple(types) => {
            let rust = tfm.rust.find(name, ty, RefMany);
            let mut item_args = Vec::new();
            for (i, item_ty) in types.iter().enumerate() {
                let item_name = names.create(&format!("{name}_{i}"));
                tfm.rust
                    .maybe_pure_decl(rust.pure, &item_name, format!("{}.{i}", rust.code));
                transform_to_swift(ast, ctx, names, tfm, &item_name, &item_ty);
                item_args.push(RustArg {
                    name: item_name,
                    ty: item_ty.clone(),
                });
            }
            if types.is_empty() {
                tfm.swift.pure_decl(name, "()".to_string());

                // Avoid an unused variable warning in Rust
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!("_ = {code};"));
            } else {
                let swift_code = tfm.swift.find_args(&item_args, RefInline);
                tfm.swift.decl(
                    name,
                    match types.len() {
                        1 => swift_code,
                        _ => format!("({swift_code})"),
                    },
                );
            }
        }

        Ptr(kind, inner_ty) => {
            if let DynTrait(trait_index) = &**inner_ty {
                let rust_code = tfm.rust.find(name, ty, RefInline).code;
                let ptr_name = names.create(&format!("{name}_ptr"));
                let swift_helper = trait_to_swift_helper(ast, ctx, *trait_index, *kind);
                tfm.rust.decl(
                    &ptr_name,
                    format!("Box::into_raw(Box::new({rust_code})) as *const u8"),
                );
                add_ffi_arg(ast, ctx, tfm, &ptr_name, &ForeignHandle);
                let ptr_code = tfm.swift.find(&ptr_name, &ForeignHandle, RefInline).code;
                tfm.swift.decl(name, format!("{swift_helper}({ptr_code})"));
            } else if *kind == RustPtr::Box {
                let rust_code = tfm.rust.find(name, ty, RefMany).code;
                let (swift_helper, rust_helper) = box_to_swift_helper(ast, ctx, inner_ty);
                let buf = ensure_rust_buf(ctx, names, tfm);
                tfm.rust.line(format!(
                    "{rust_helper}(*{rust_code}, {}{});",
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                tfm.swift
                    .decl(name, format!("{swift_helper}(&{})", buf.end_name()));
            } else {
                unreachable!()
            }
        }

        Vector(inner_ty) => {
            let rust_code = tfm.rust.find(name, ty, RefMany).code;
            let len_name = names.create(&format!("{name}_len"));
            let (swift_helper, rust_helper) = vec_to_swift_helper(ast, ctx, inner_ty);
            let buf = ensure_rust_buf(ctx, names, tfm);
            tfm.rust.decl(&len_name, format!("{rust_code}.len()"));
            add_ffi_arg(ast, ctx, tfm, &len_name, &Usize);
            tfm.rust.line(format!(
                "{rust_helper}({rust_code}, {}{});",
                tfm.buf_ref,
                buf.buf_name()
            ));
            let len_code = tfm.swift.find(&len_name, ty, RefInline).code;
            tfm.swift.decl(
                name,
                format!("{swift_helper}(Int({len_code}), &{})", buf.end_name()),
            );
        }

        Optional(inner_ty) => {
            let rust_code = tfm.rust.find(name, ty, RefMany).code;
            let has_name = names.create(&format!("has_{name}"));
            let val_name = names.create(&format!("{name}_val"));
            ensure_rust_buf(ctx, names, tfm);

            tfm.rust.decl(&has_name, format!("{rust_code}.is_some()"));
            add_ffi_arg(ast, ctx, tfm, &has_name, &Bool);

            let mut swift = FnBuilder::default();
            tfm.rust
                .line(format!("if let Some({val_name}) = {rust_code} {{"));
            {
                let old = tfm.buf_status;
                tfm.buf_status = BufStatus::Inside;
                std::mem::swap(&mut tfm.swift, &mut swift);
                transform_to_swift(ast, ctx, names, tfm, &val_name, inner_ty);
                std::mem::swap(&mut tfm.swift, &mut swift);
                tfm.buf_status = old;
            }
            tfm.rust.line("}".into());

            let has_code = tfm.swift.find(&has_name, ty, RefInline).code;
            let mut val_code = swift.find(&val_name, ty, RefInline).code;
            if let Optional(_) = &**inner_ty {
                val_code = format!("Optional.some({val_code})");
            }
            if swift.is_empty() {
                tfm.swift
                    .decl(name, format!("{has_code} ? {val_code} : nil"));
            } else {
                let mut swift_ty = String::new();
                append_swift_type(&mut swift_ty, ast, ty);
                swift.insert_deferred_lines_here();
                swift.line(format!("{name} = {val_code}"));
                tfm.swift.line(format!("var {name}: {swift_ty} = nil"));
                tfm.swift.line(format!("if {has_code} {{"));
                tfm.swift.extend(swift);
                tfm.swift.line("}".to_string());
            }
        }

        Pair { .. } | Verbatim(_) | DynTrait(_) => unreachable!(),
    }
}

fn ensure_swift_buf(ctx: &mut SwiftCtx, names: &mut NameSet, tfm: &mut Transform) -> Rc<SharedBuf> {
    if let Some(buf) = &tfm.buf {
        return buf.clone();
    }

    let buf_name = names.create("buf");
    let ptr_name = names.create("buf_ptr");
    let end_name = names.create("buf_end");
    let buf = SharedBuf::new(&buf_name, &end_name);

    // Swift (write)
    tfm.swift.line_alt(
        format!("let {buf_name} = ContiguousArray<UInt8>()"),
        format!("var {buf_name} = ContiguousArray<UInt8>()"),
        buf.is_buf_name_used_flag(),
    );
    tfm.swift
        .defer_decl(&ptr_name, format!("_ffi_vec_to_rust({buf_name})"));
    ctx.swift_helpers.mark_used("_ffi_vec_to_rust");
    ctx.header_helpers
        .mark_used_group(HeaderGroup::Other, "_ffi_alloc");

    // Rust (read)
    tfm.rust.line_alt(
        // Avoid a warning about an unnecessarily mutable variable
        format!("let {end_name} = {ptr_name};"),
        format!("let mut {end_name} = {ptr_name};"),
        buf.is_buf_name_used_flag(),
    );
    tfm.rust
        .defer_line(format!("_ffi_buf_from_host({ptr_name}, {end_name});"));
    ctx.rust_helpers.mark_used("_ffi_buf_from_host");

    // FFI
    tfm.buf = Some(buf.clone());
    tfm.buf_ref = "&mut ";
    tfm.ffi_args.push(RustArg {
        name: ptr_name,
        ty: RustType::ForeignHandle,
    });
    buf
}

fn ensure_rust_buf(ctx: &mut SwiftCtx, names: &mut NameSet, tfm: &mut Transform) -> Rc<SharedBuf> {
    if let Some(buf) = &tfm.buf {
        return buf.clone();
    }

    let buf_name = names.create("buf");
    let ptr_name = names.create("buf_ptr");
    let end_name = names.create("buf_end");
    let cap_name = names.create("buf_cap");
    let buf = SharedBuf::new(&buf_name, &end_name);

    // Rust (write)
    tfm.rust.line_alt(
        // Avoid a warning about an unnecessarily mutable variable
        format!("let {buf_name} = Vec::<u8>::new();"),
        format!("let mut {buf_name} = Vec::<u8>::new();"),
        buf.is_buf_name_used_flag(),
    );
    tfm.rust.defer_line(format!(
        "let ({ptr_name}, {cap_name}) = _ffi_buf_to_host({buf_name});"
    ));
    ctx.rust_helpers.mark_used("_ffi_buf_to_host");

    // Swift (read)
    tfm.swift.line_alt(
        "".to_string(),
        format!("var {end_name} = {ptr_name}!"),
        buf.is_end_name_used_flag(),
    );
    tfm.swift
        .defer_line(format!("_ffi_dealloc({ptr_name}, {cap_name})"));
    ctx.header_helpers
        .mark_used_group(HeaderGroup::Other, "_ffi_dealloc");

    // FFI
    tfm.buf = Some(buf.clone());
    tfm.buf_ref = "&mut ";
    tfm.ffi_args.push(RustArg {
        name: ptr_name,
        ty: RustType::ForeignHandle,
    });
    tfm.ffi_args.push(RustArg {
        name: cap_name,
        ty: RustType::Usize,
    });
    buf
}

fn vec_to_rust_helper(ast: &AST, ctx: &mut SwiftCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.vec_to_rust_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_vec_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let swift_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_swift"));

    // This must be done first to avoid a stack overflow
    ctx.vec_to_rust_helpers
        .insert(inner_ty.clone(), (swift_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let vec_name = locals.create("items");
    let item_name = locals.create("item");
    let end_name = locals.create("end");
    let len_name = locals.create("len");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.swift.mark_pure(&item_name);
    transform_to_rust(ast, ctx, &mut locals, &mut tfm, &item_name, &inner_ty);
    let item_code = tfm.rust.find(&item_name, inner_ty, RefInline).code;
    tfm.rust.line(format!("{vec_name}.push({item_code});"));

    // Swift
    {
        let mut swift = String::new();
        _ = write!(swift, "\nprivate func {swift_name}(_ {vec_name}: [");
        append_swift_type(&mut swift, ast, inner_ty);
        _ = write!(swift, "], _ {buf_name}: inout ContiguousArray<UInt8>) {{\n");
        if !tfm.swift.is_empty() {
            _ = write!(swift, "    for {item_name} in {vec_name} {{\n");
            tfm.swift.write_to_swift(ast, &mut swift, "        ");
            swift.push_str("    }\n");
        }
        swift.push_str("}\n");
        ctx.swift_helpers.add(&swift_name, swift).mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        let mut item_ty = String::new();
        let end_name = buf.final_end_name_for_rust();
        append_rust_type(&mut item_ty, ast, inner_ty);
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({len_name}: usize, {end_name}: &mut *const u8) -> Vec<{item_ty}> {{\n"
        );
        _ = write!(
            rust,
            "    let mut {vec_name} = Vec::<{item_ty}>::with_capacity({len_name});\n"
        );
        _ = write!(rust, "    for _ in 0..{len_name} {{\n");
        tfm.rust.write_to_rust(ast, &mut rust, "        ");
        rust.push_str("    }\n");
        _ = write!(rust, "    {vec_name}\n");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    (swift_name, rust_name)
}

fn vec_to_swift_helper(ast: &AST, ctx: &mut SwiftCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.vec_to_swift_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_vec_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let swift_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_swift"));

    // This must be done first to avoid a stack overflow
    ctx.vec_to_swift_helpers
        .insert(inner_ty.clone(), (swift_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let vec_name = locals.create("items");
    let item_name = locals.create("item");
    let end_name = locals.create("end");
    let len_name = locals.create("len");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.rust.mark_pure(&item_name);
    transform_to_swift(ast, ctx, &mut locals, &mut tfm, &item_name, &inner_ty);
    let item_code = tfm.swift.find(&item_name, inner_ty, RefInline).code;
    tfm.swift.line(format!("{vec_name}.append({item_code})"));

    // Rust
    {
        let mut rust = String::new();
        let buf_name = buf.final_buf_name_for_rust();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(rust, "\nfn {rust_name}({vec_name}: Vec<");
        append_rust_type(&mut rust, ast, inner_ty);
        _ = write!(rust, ">, {buf_name}: &mut Vec<u8>) {{\n");
        if !tfm.rust.is_empty() {
            _ = write!(rust, "    for {item_name} in {vec_name} {{\n");
            tfm.rust.write_to_rust(ast, &mut rust, "        ");
            rust.push_str("    }\n");
        }
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    // Swift
    {
        let mut swift = String::new();
        _ = write!(
            swift,
            "\nprivate func {swift_name}(_ {len_name}: Int, _ {end_name}: inout UnsafeRawPointer) -> ["
        );
        append_swift_type(&mut swift, ast, inner_ty);
        swift.push_str("] {\n");
        _ = write!(swift, "    var {vec_name}: [");
        append_swift_type(&mut swift, ast, inner_ty);
        swift.push_str("] = []\n");
        _ = write!(swift, "    {vec_name}.reserveCapacity({len_name})\n");
        _ = write!(swift, "    while {vec_name}.count < {len_name} {{\n");
        tfm.swift.write_to_swift(ast, &mut swift, "        ");
        swift.push_str("    }\n");
        _ = write!(swift, "    return {vec_name}\n");
        swift.push_str("}\n");
        ctx.swift_helpers.add(&swift_name, swift).mark_used();
    }

    (swift_name, rust_name)
}

fn trait_to_rust_helper(ast: &AST, ctx: &mut SwiftCtx, trait_index: usize) -> String {
    if let Some(result) = ctx.trait_to_rust_helpers.get(&trait_index) {
        return result.clone();
    }

    let t = &ast.traits[trait_index];
    let rust_name = format!("_ffi_rs_{}", t.name);

    // This must be done first to avoid a stack overflow
    ctx.trait_to_rust_helpers
        .insert(trait_index, rust_name.clone());

    // Rust
    {
        let mut rust = String::new();
        allow_non_camel_case_types(&mut rust, &rust_name);
        _ = write!(rust, "\nstruct {rust_name}(*const u8);\n");
        _ = write!(rust, "\nimpl Drop for {rust_name} {{\n");
        rust.push_str("    fn drop(&mut self) {\n");
        _ = write!(
            rust,
            "        unsafe extern \"C\" {{ fn _ffi_swift_drop(_: *const u8); }}\n"
        );
        _ = write!(rust, "        unsafe {{ _ffi_swift_drop(self.0) }};\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        _ = write!(rust, "\nimpl {} for {rust_name} {{", t.name);
        for f in &t.fns {
            generate_rust_to_swift_fn(ast, ctx, t, f, &mut rust);
        }
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
        ctx.swift_helpers.mark_used("_ffi_swift_drop");
    }

    rust_name
}

fn trait_to_swift_helper(
    ast: &AST,
    ctx: &mut SwiftCtx,
    trait_index: usize,
    kind: RustPtr,
) -> String {
    if let Some(result) = ctx.trait_to_swift_helpers.get(&(kind, trait_index)) {
        return result.clone();
    }

    let t = &ast.traits[trait_index];
    let drop_name = format!("_ffi_rs_drop_{kind:?}_{}", t.name);
    let swift_name = format!("_ffi_{kind:?}_{}", t.name);

    // This must be done first to avoid a stack overflow
    ctx.trait_to_swift_helpers
        .insert((kind, trait_index), swift_name.clone());

    // Swift
    {
        let mut swift = String::new();
        _ = write!(swift, "\nprivate class {swift_name} : {} {{\n", t.name);
        swift.push_str("    private var _ffi: UnsafeRawPointer?\n");
        swift.push_str("\n    init(_ ptr: UnsafeRawPointer?) {\n");
        swift.push_str("        _ffi = ptr\n");
        swift.push_str("    }\n");
        swift.push_str("\n    deinit {\n");
        _ = write!(swift, "        {drop_name}(_ffi)\n");
        swift.push_str("    }\n");
        for f in &t.fns {
            let info = Some(TraitInfo { t, kind });
            generate_swift_to_rust_fn(ast, ctx, f, &mut swift, info);
        }
        swift.push_str("}\n");
        ctx.swift_helpers.add(&swift_name, swift).mark_used();
    }

    // Header
    {
        let mut header = String::new();
        let mut header_deps = HashSet::new();
        header.push('\n');
        append_c_signature(
            &mut header,
            &mut header_deps,
            None,
            &drop_name,
            None,
            &[RustArg {
                name: "ptr".to_string(),
                ty: RustType::ForeignHandle,
            }],
        );
        header.push_str(";\n");
        ctx.header_helpers
            .add_group(HeaderGroup::Other, &drop_name, header)
            .add_deps_group(header_deps)
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        rust.push_str("\n#[unsafe(no_mangle)]\n");
        _ = write!(rust, "extern \"C\" fn {drop_name}(ptr: *const u8) {{\n");
        _ = write!(
            rust,
            "    drop(unsafe {{ Box::from_raw(ptr as *mut {}<dyn {}>) }});\n",
            kind.path(),
            t.name
        );
        rust.push_str("}\n");
        ctx.rust_helpers.add(&drop_name, rust).mark_used();
    }

    swift_name
}

fn box_to_rust_helper(ast: &AST, ctx: &mut SwiftCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.box_to_rust_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_box_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let swift_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_swift"));

    // This must be done first to avoid a stack overflow
    ctx.box_to_rust_helpers
        .insert(inner_ty.clone(), (swift_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let end_name = locals.create("end");
    let buf_name = locals.create("buf");

    // Transform the value
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.swift.mark_pure(&val_name);
    transform_to_rust(ast, ctx, &mut locals, &mut tfm, &val_name, &inner_ty);
    let val_code = tfm.rust.find(&val_name, inner_ty, RefInline).code;
    tfm.rust.line(format!("Box::new({val_code})"));

    // Swift
    {
        let mut swift = String::new();
        _ = write!(swift, "\nprivate func {swift_name}(_ {val_name}: ");
        append_swift_type(&mut swift, ast, inner_ty);
        _ = write!(swift, ", _ {buf_name}: inout ContiguousArray<UInt8>) {{\n");
        tfm.swift.write_to_swift(ast, &mut swift, "    ");
        swift.push_str("}\n");
        ctx.swift_helpers.add(&swift_name, swift).mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        let mut item_ty = String::new();
        let end_name = buf.final_end_name_for_rust();
        append_rust_type(&mut item_ty, ast, inner_ty);
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({end_name}: &mut *const u8) -> Box<{item_ty}> {{\n"
        );
        tfm.rust.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    (swift_name, rust_name)
}

fn box_to_swift_helper(ast: &AST, ctx: &mut SwiftCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.box_to_swift_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_box_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let swift_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_swift"));

    // This must be done first to avoid a stack overflow
    ctx.box_to_swift_helpers
        .insert(inner_ty.clone(), (swift_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let end_name = locals.create("end");
    let buf_name = locals.create("buf");

    // Transform the value
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.rust.mark_pure(&val_name);
    transform_to_swift(ast, ctx, &mut locals, &mut tfm, &val_name, &inner_ty);
    let val_code = tfm.swift.find(&val_name, inner_ty, RefInline).code;
    tfm.swift.line(format!("return {val_code}"));

    // Rust
    {
        let mut rust = String::new();
        let buf_name = buf.final_buf_name_for_rust();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(rust, "\nfn {rust_name}({val_name}: ");
        append_rust_type(&mut rust, ast, inner_ty);
        _ = write!(rust, ", {buf_name}: &mut Vec<u8>) {{\n");
        tfm.rust.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    // Swift
    {
        let mut swift = String::new();
        _ = write!(
            swift,
            "\nprivate func {swift_name}(_ {end_name}: inout UnsafeRawPointer) -> "
        );
        append_swift_type(&mut swift, ast, inner_ty);
        swift.push_str(" {\n");
        tfm.swift.write_to_swift(ast, &mut swift, "    ");
        swift.push_str("}\n");
        ctx.swift_helpers.add(&swift_name, swift).mark_used();
    }

    (swift_name, rust_name)
}

fn enum_to_rust_helper(ast: &AST, ctx: &mut SwiftCtx, enum_index: usize) -> (String, String) {
    if let Some(result) = ctx.enum_to_rust_helpers.get(&enum_index) {
        return result.clone();
    }

    let e = &ast.enums[enum_index];
    let base_name = format!("_ffi_enum_{}", e.name);
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_swift"));
    let swift_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));

    // This must be done first to avoid a stack overflow
    ctx.enum_to_rust_helpers
        .insert(enum_index, (swift_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");
    let end_name = locals.create("end");
    let mut branches = Vec::new();

    // Enums without fields are just integers
    if !e.has_fields() {
        let mut rust = String::new();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(rust, "\nfn {rust_name}({val_name}: i32) -> {} {{\n", e.name);
        _ = write!(rust, "    match {val_name} {{\n");
        for v in &e.variants {
            _ = write!(
                rust,
                "        {} => {}::{},\n",
                v.discriminant, e.name, v.name
            );
        }
        rust.push_str("        _ => panic!(),\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
        return (swift_name, rust_name);
    }

    struct Branch {
        tfm: Transform,
        fields: Vec<String>,
    }

    // Transform all fields for each variant in a separate branch
    for v in &e.variants {
        let mut branch_locals = locals.clone();
        let mut fields = Vec::new();
        let mut tfm = Transform::default();
        let buf = SharedBuf::new(&buf_name, &end_name);
        tfm.buf = Some(buf.clone());
        tfm.buf_status = BufStatus::Inside;
        for f in &v.fields {
            let field_name = branch_locals.create(&name_for_match(&f.name, v.fields.len()));
            tfm.swift.mark_pure(&field_name);
            transform_to_rust(ast, ctx, &mut branch_locals, &mut tfm, &field_name, &f.ty);
            fields.push(field_name);
        }
        rust_decl_ctor(
            &mut tfm.rust,
            &val_name,
            &format!("{}::{}", e.name, v.name),
            &v.fields,
            fields.iter().map(|x| Cow::Borrowed(x.as_str())).collect(),
        );
        branches.push(Branch { tfm, fields });
    }

    // Swift
    {
        let mut swift = String::new();
        _ = write!(
            swift,
            "\nprivate func {swift_name}(_ {val_name}: {}, _ {buf_name}: inout ContiguousArray<UInt8>) {{\n",
            e.name
        );
        _ = write!(swift, "    switch {val_name} {{\n");
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            if branch.fields.is_empty() {
                _ = write!(swift, "    case .{}:\n", v.name);
            } else {
                _ = write!(swift, "    case let .{}(", v.name);
                for (i, f) in branch.fields.iter().enumerate() {
                    if i > 0 {
                        swift.push_str(", ");
                    }
                    swift.push_str(&f);
                }
                swift.push_str("):\n");
            }
            _ = write!(
                swift,
                "        _ffi_write({} as Int32, &{buf_name})\n",
                v.discriminant
            );
            branch.tfm.swift.write_to_swift(ast, &mut swift, "        ");
        }
        swift.push_str("    }\n");
        swift.push_str("}\n");
        ctx.swift_helpers.add(&swift_name, swift).mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({end_name}: &mut *const u8) -> {} {{\n",
            e.name
        );
        _ = write!(rust, "    match _ffi_read::<i32>({end_name}) {{\n");
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            let val_code = branch
                .tfm
                .rust
                .find(&val_name, &RustType::Enum(enum_index), RefInline)
                .code;
            if branch.tfm.rust.is_empty() {
                let val_code = format!("{} => {val_code},", v.discriminant);
                branch.tfm.rust.line(val_code);
                branch.tfm.rust.write_to_rust(ast, &mut rust, "        ");
            } else {
                _ = write!(rust, "        {} => {{", v.discriminant);
                branch.tfm.rust.line(val_code);
                branch
                    .tfm
                    .rust
                    .write_to_rust(ast, &mut rust, "            ");
                rust.push_str("        }\n");
            }
        }
        rust.push_str("        _ => panic!(),\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers
            .add(&rust_name, rust)
            .add_dep("_ffi_read")
            .mark_used();
    }

    (swift_name, rust_name)
}

fn enum_to_swift_helper(ast: &AST, ctx: &mut SwiftCtx, enum_index: usize) -> (String, String) {
    if let Some(result) = ctx.enum_to_swift_helpers.get(&enum_index) {
        return result.clone();
    }

    let e = &ast.enums[enum_index];
    let base_name = format!("_ffi_enum_{}", e.name);
    let swift_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_swift"));

    // This must be done first to avoid a stack overflow
    ctx.enum_to_swift_helpers
        .insert(enum_index, (swift_name.clone(), rust_name.clone()));

    struct Branch {
        tfm: Transform,
        fields: Vec<String>,
    }

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");
    let end_name = locals.create("end");
    let mut branches = Vec::new();

    // Transform all fields for each variant in a separate branch
    for v in &e.variants {
        let mut branch_locals = locals.clone();
        let mut fields = Vec::new();
        let mut tfm = Transform::default();
        let buf = SharedBuf::new(&buf_name, &end_name);
        tfm.buf = Some(buf.clone());
        tfm.buf_status = BufStatus::Inside;
        for f in &v.fields {
            let field_name = branch_locals.create(&name_for_match(&f.name, v.fields.len()));
            tfm.rust.mark_pure(&field_name);
            transform_to_swift(ast, ctx, &mut branch_locals, &mut tfm, &field_name, &f.ty);
            fields.push(field_name);
        }
        let val_code = tfm.swift.find_fields(
            &v.fields
                .iter()
                .map(|f| RustField {
                    name: match starts_with_digit(&f.name) {
                        false => f.name.clone(),
                        true => "".to_string(),
                    },
                    ty: f.ty.clone(),
                })
                .collect(),
            fields.iter().map(|x| Cow::Borrowed(x.as_str())).collect(),
            RefInline,
            "(",
            ")",
        );
        tfm.swift.decl(
            &val_name,
            match v.fields.len() {
                0 => format!(".{}", v.name),
                _ => format!(".{}{}", v.name, val_code),
            },
        );
        branches.push(Branch { tfm, fields });
    }

    // Rust
    {
        let mut rust = String::new();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({val_name}: {}, {buf_name}: &mut Vec<u8>) {{\n",
            e.name
        );
        _ = write!(rust, "    match {val_name} {{\n");
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            _ = write!(rust, "        {}::{}", e.name, v.name);
            if v.fields.iter().any(|f| starts_with_digit(&f.name)) {
                rust.push_str("(");
                for (i, name) in branch.fields.iter().enumerate() {
                    rust.push_str(if i > 0 { ", " } else { "" });
                    rust.push_str(name);
                }
                rust.push_str(")");
            } else if !v.fields.is_empty() {
                rust.push_str(" {");
                for (i, (f, name)) in v.fields.iter().zip(&branch.fields).enumerate() {
                    rust.push_str(if i > 0 { ", " } else { " " });
                    if f.name != *name {
                        _ = write!(rust, "{}: ", f.name);
                    }
                    rust.push_str(name);
                }
                rust.push_str(" }");
            }
            if branch.tfm.rust.is_empty() {
                _ = write!(
                    rust,
                    " => _ffi_write({} as i32, {buf_name}),\n",
                    v.discriminant
                );
            } else {
                rust.push_str(" => {\n");
                _ = write!(
                    rust,
                    "            _ffi_write({} as i32, {buf_name});\n",
                    v.discriminant
                );
                branch
                    .tfm
                    .rust
                    .write_to_rust(ast, &mut rust, "            ");
                rust.push_str("        }\n");
            }
        }
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers
            .add(&rust_name, rust)
            .add_dep("_ffi_write")
            .mark_used();
    }

    // Swift
    {
        let mut swift = String::new();
        _ = write!(
            swift,
            "\nprivate func {swift_name}(_ {end_name}: inout UnsafeRawPointer) -> {} {{\n",
            e.name
        );
        _ = write!(swift, "    switch _ffi_read(&{end_name}) as Int32 {{\n",);
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            let val_code = branch
                .tfm
                .swift
                .find(&val_name, &RustType::Enum(enum_index), RefInline)
                .code;
            if branch.tfm.swift.is_empty() {
                let val_code = format!("case {}: return {val_code}", v.discriminant);
                branch.tfm.swift.line(val_code);
                branch.tfm.swift.write_to_swift(ast, &mut swift, "        ");
            } else {
                _ = write!(swift, "        case {}:\n", v.discriminant);
                branch.tfm.swift.line(format!("return {val_code}"));
                branch
                    .tfm
                    .swift
                    .write_to_swift(ast, &mut swift, "            ");
            }
        }
        swift.push_str("        default: fatalError()\n");
        swift.push_str("    }\n");
        swift.push_str("}\n");
        ctx.swift_helpers
            .add(&swift_name, swift)
            .add_dep("_ffi_read")
            .mark_used();
    }

    (swift_name, rust_name)
}

fn multi_ret_helper(ast: &AST, ctx: &mut SwiftCtx, args: &[RustArg]) -> String {
    let types: Vec<_> = args.iter().map(|arg| arg.ty.clone()).collect();
    if let Some(result) = ctx.multi_ret_helpers.get(&types) {
        return result.clone();
    }

    // Construct the name for the new type
    let mut ty_name = "_ffi_ret_".to_string();
    append_type_name_hints(&mut ty_name, ast, &types);
    let ty_name = ctx.helper_names.create(&ty_name);

    // Header
    {
        let mut header = String::new();
        let mut header_deps = HashSet::new();
        header.push_str("\ntypedef struct {\n");
        for (i, ty) in types.iter().enumerate() {
            header.push_str("    ");
            append_c_type(&mut header, &mut header_deps, ty);
            _ = write!(header, " _{i};\n");
        }
        _ = write!(header, "}} {ty_name};\n");
        ctx.header_helpers
            .add_group(HeaderGroup::Other, &ty_name, header)
            .add_deps_group(header_deps)
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        _ = write!(rust, "\n#[repr(C)]\nstruct {ty_name}(");
        for (i, ty) in types.iter().enumerate() {
            if i > 0 {
                rust.push_str(", ");
            }
            append_rust_type(&mut rust, ast, ty);
        }
        rust.push_str(");\n");
        ctx.rust_helpers.add(&ty_name, rust).mark_used();
    }

    ctx.multi_ret_helpers.insert(types, ty_name.clone());
    ty_name
}

fn append_swift_type(swift: &mut String, ast: &AST, ty: &RustType) {
    use RustType::*;
    match ty {
        Pair { other, .. } => append_swift_type(swift, ast, other),
        Verbatim(text) => swift.push_str(text),
        Bool => swift.push_str("Bool"),

        U8 => swift.push_str("UInt8"),
        U16 => swift.push_str("UInt16"),
        U32 => swift.push_str("UInt32"),
        Usize => swift.push_str("UInt"),
        U64 => swift.push_str("UInt64"),

        I8 => swift.push_str("Int8"),
        I16 => swift.push_str("Int16"),
        I32 => swift.push_str("Int32"),
        Isize => swift.push_str("Int"),
        I64 => swift.push_str("Int64"),

        F32 => swift.push_str("Float32"),
        F64 => swift.push_str("Float64"),
        RefStr | OwnStr => swift.push_str("String"),

        Struct(index) => swift.push_str(&ast.structs[*index].name),
        Enum(index) => swift.push_str(&ast.enums[*index].name),
        DynTrait(index) => swift.push_str(&ast.traits[*index].name),
        Ptr(_, inner) => append_swift_type(swift, ast, inner),

        Tuple(types) if types.len() == 1 => append_swift_type(swift, ast, &types[0]),
        Tuple(types) => {
            swift.push('(');
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    swift.push_str(", ");
                }
                append_swift_type(swift, ast, ty);
            }
            swift.push(')');
        }

        Vector(inner) => {
            swift.push('[');
            append_swift_type(swift, ast, inner);
            swift.push(']');
        }

        ForeignHandle => swift.push_str("UnsafeRawPointer?"),

        Optional(inner_ty) => {
            append_swift_type(swift, ast, &inner_ty);
            swift.push('?');
        }
    }
}

fn append_swift_val(swift: &mut String, val: &RustVal) {
    use RustVal::*;
    match val {
        Bool(x) => _ = write!(swift, "{x}"),

        U8(x) => _ = write!(swift, "{x}"),
        U16(x) => _ = write!(swift, "{x}"),
        U32(x) => _ = write!(swift, "{x}"),
        U64(x) => _ = write!(swift, "{x}"),

        I8(x) => _ = write!(swift, "{x}"),
        I16(x) => _ = write!(swift, "{x}"),
        I32(x) => _ = write!(swift, "{x}"),
        I64(x) => _ = write!(swift, "{x}"),

        F32(x) => _ = write!(swift, "{x:?}"),
        F64(x) => _ = write!(swift, "{x:?}"),

        Str(x) => append_swift_quoted(swift, x),
    }
}

fn append_swift_quoted(swift: &mut String, text: &str) {
    swift.push('"');
    for c in text.chars() {
        match c {
            '\t' => swift.push_str("\\t"),
            '\n' => swift.push_str("\\n"),
            '\r' => swift.push_str("\\r"),
            '\\' => swift.push_str("\\\\"),
            '\"' => swift.push_str("\\\""),
            c if (c as u32) < 0x20 => _ = write!(swift, "\\u{{{:x}}}", c as u32),
            _ => swift.push(c),
        }
    }
    swift.push('"');
}

fn append_c_signature(
    c: &mut String,
    deps: &mut HashSet<(HeaderGroup, String)>,
    returns: Option<&RustType>,
    name: &str,
    receiver: Option<&RustType>,
    args: &[RustArg],
) {
    // Emit the return type
    if let Some(returns) = returns {
        append_c_type(c, deps, returns);
        c.push(' ');
    } else {
        c.push_str("void ");
    }

    // The function name goes in the middle in C
    c.push_str(name);

    // Emit the arguments
    c.push('(');
    if let Some(receiver) = receiver {
        append_c_type(c, deps, receiver);
    }
    for (i, arg) in args.iter().enumerate() {
        if receiver.is_some() || i > 0 {
            c.push_str(", ");
        }
        append_c_type(c, deps, &arg.ty);
        c.push(' ');
        c.push_str(&arg.name);
    }
    c.push(')');
}

fn append_c_type(c: &mut String, deps: &mut HashSet<(HeaderGroup, String)>, ty: &RustType) {
    use RustType::*;
    let text = match ty {
        Pair { other, .. } => return append_c_type(c, deps, &other),
        Verbatim(text) => text,
        Bool => "_Bool",

        U8 => "uint8_t",
        U16 => "uint16_t",
        U32 => "uint32_t",
        Usize => "uintptr_t",
        U64 => "uint64_t",

        I8 => "int8_t",
        I16 => "int16_t",
        I32 => "int32_t",
        Isize => "intptr_t",
        I64 => "int64_t",

        F32 => "float",
        F64 => "double",
        ForeignHandle => "const void*",
        _ => unreachable!(),
    };
    if text.ends_with("_t") {
        deps.insert((HeaderGroup::Include, "<stdint.h>".into()));
    }
    c.push_str(text);
}

// We represent "Box<T>" as "T" in Swift, so a recursive struct that uses Box
// to refer to itself in Rust will need to be a class in Swift to avoid having
// infinite size.
fn swift_type_is_infinite_size(ast: &AST, ty: &RustType) -> bool {
    fn visit(ast: &AST, ty: &RustType, visited: &mut HashSet<RustType>) -> bool {
        use RustType::*;
        match ty {
            Ptr(kind, inner_ty) if *kind == RustPtr::Box => visit(ast, &inner_ty, visited),
            Struct(struct_index) => {
                if visited.contains(ty) {
                    return true;
                }
                visited.insert(ty.clone());
                ast.structs[*struct_index]
                    .fields
                    .iter()
                    .any(|f| visit(ast, &f.ty, visited))
            }
            Tuple(types) => types.iter().any(|ty| visit(ast, ty, visited)),
            Optional(inner) => visit(ast, inner, visited),
            _ => false,
        }
    }
    visit(ast, ty, &mut HashSet::new())
}

// We represent "Box<T>" as "T" in Swift, so a recursive enum that uses Box to
// refer to itself in Rust will need to use an "indirect case" in Swift to
// avoid having infinite size.
fn swift_variant_needs_indirect(ast: &AST, query_index: usize, v: &RustVariant) -> bool {
    use RustType::*;
    fn visit(
        ast: &AST,
        ty: &RustType,
        query_index: usize,
        visited: &mut HashSet<RustType>,
    ) -> bool {
        match ty {
            Enum(enum_index) if *enum_index == query_index => true,
            Enum(enum_index) => {
                if visited.contains(ty) {
                    return true;
                }
                visited.insert(ty.clone());
                ast.enums[*enum_index].variants.iter().any(|v| {
                    v.fields
                        .iter()
                        .any(|f| visit(ast, &f.ty, query_index, visited))
                })
            }
            Ptr(kind, inner_ty) if *kind == RustPtr::Box => {
                visit(ast, inner_ty, query_index, visited)
            }
            Struct(struct_index) => {
                if visited.contains(ty) {
                    return true;
                }
                visited.insert(ty.clone());
                ast.structs[*struct_index]
                    .fields
                    .iter()
                    .any(|f| visit(ast, &f.ty, query_index, visited))
            }
            Tuple(types) => types.iter().any(|ty| visit(ast, ty, query_index, visited)),
            Optional(inner) => visit(ast, inner, query_index, visited),
            _ => false,
        }
    }
    let mut visited = HashSet::new();
    v.fields
        .iter()
        .any(|f| visit(ast, &f.ty, query_index, &mut visited))
}

impl FnBuilder {
    fn write_to_swift(&mut self, ast: &AST, out: &mut String, base_indent: &str) {
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
                        append_swift_type(&mut decl_ty, ast, &ty);
                    }
                    let text = format!("let {name}{decl_ty} = {text}");
                    text.split('\n').for_each(&mut callback);
                }
            }
        }
    }
}
