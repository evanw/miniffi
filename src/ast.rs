use super::*;
use std::collections::HashSet;
use std::fmt::Write;

pub struct AST {
    pub structs: Vec<RustStruct>,
    pub enums: Vec<RustEnum>,
    pub traits: Vec<RustTrait>,
    pub consts: Vec<RustConst>,
    pub fns: Vec<RustFn>,
}

impl AST {
    pub fn rename_keywords(&mut self, keywords: &[&str]) {
        let keywords: HashSet<_> = keywords.iter().map(|x| *x).collect();
        let mut names = NameSet::default();
        for k in &keywords {
            names.add(k.to_string());
        }

        // Avoid the "_" symbol in Rust
        names.add("_".to_string());

        let rename_keywords_in_fn = move |f: &mut RustFn| {
            // Avoid cloning the set in the common case
            if !f
                .args
                .iter()
                .chain(f.returns.iter())
                .any(|x| names.contains(x.name.as_str()))
            {
                return;
            }

            // Rename all relevant local identifiers
            let mut names = names.clone();
            for arg in &mut f.args {
                arg.name = names.create(&arg.name);
            }
            if let Some(ret) = &mut f.returns {
                ret.name = names.create(&ret.name);
            }
        };

        for f in &mut self.fns {
            rename_keywords_in_fn(f);
        }

        for t in &mut self.traits {
            for f in &mut t.fns {
                rename_keywords_in_fn(f);
            }
        }
    }
}

pub struct RustStruct {
    pub name: String,
    pub fields: Vec<RustField>,
    pub derives_partial_eq: bool,
}

pub struct RustEnum {
    pub name: String,
    pub variants: Vec<RustVariant>,
    pub derives_partial_eq: bool,
}

impl RustEnum {
    pub fn has_fields(&self) -> bool {
        self.variants.iter().any(|v| !v.fields.is_empty())
    }
}

pub struct RustVariant {
    pub name: String,
    pub discriminant: i32,
    pub fields: Vec<RustField>,
}

pub struct RustTrait {
    pub name: String,
    pub fns: Vec<RustFn>,
}

#[derive(Clone)]
pub struct RustField {
    pub name: String,
    pub ty: RustType,
}

pub struct RustConst {
    pub name: String,
    pub ty: RustType,
    pub val: RustVal,
}

pub struct RustFn {
    pub name: String,
    pub args: Vec<RustArg>,
    pub returns: Option<RustArg>,
}

#[derive(Clone)]
pub struct RustArg {
    pub name: String,
    pub ty: RustType,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RustPtr {
    Box,
    Rc,
}

impl RustPtr {
    pub fn path(&self) -> &'static str {
        match self {
            RustPtr::Box => "Box",
            RustPtr::Rc => "std::rc::Rc",
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RustType {
    Pair {
        rust: Box<RustType>,
        other: Box<RustType>,
    },
    Verbatim(String),
    Bool,

    U8,
    U16,
    U32,
    Usize,
    U64,

    I8,
    I16,
    I32,
    Isize,
    I64,

    F32,
    F64,

    RefStr,
    OwnStr,

    Struct(usize),
    Enum(usize),
    DynTrait(usize),
    Ptr(RustPtr, Box<RustType>),
    Vector(Box<RustType>),
    Tuple(Vec<RustType>),
    Optional(Box<RustType>),

    ForeignHandle,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RustVal {
    Bool(bool),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),

    F32(f32),
    F64(f64),

    Str(String),
}

pub fn append_type_name_hint(out: &mut String, ast: &AST, ty: &RustType) {
    use RustType::*;
    match ty {
        Pair { rust, .. } => append_type_name_hint(out, ast, rust),
        Verbatim(text) => out.push_str(text),
        Bool => out.push_str("bool"),

        U8 => out.push_str("u8"),
        U16 => out.push_str("u16"),
        U32 => out.push_str("u32"),
        Usize => out.push_str("usize"),
        U64 => out.push_str("u64"),

        I8 => out.push_str("i8"),
        I16 => out.push_str("i16"),
        I32 => out.push_str("i32"),
        Isize => out.push_str("isize"),
        I64 => out.push_str("i64"),

        F32 => out.push_str("f32"),
        F64 => out.push_str("f64"),

        RefStr => out.push_str("str"),
        OwnStr => out.push_str("string"),

        Struct(index) => out.push_str(&ast.structs[*index].name),
        Enum(index) => out.push_str(&ast.enums[*index].name),

        DynTrait(index) => {
            out.push_str("dyn_");
            out.push_str(&ast.traits[*index].name);
        }

        Ptr(kind, inner) => {
            out.push_str(match kind {
                RustPtr::Box => "box_",
                RustPtr::Rc => "rc_",
            });
            append_type_name_hint(out, ast, &inner);
        }

        Vector(inner) => {
            out.push_str("vec_");
            append_type_name_hint(out, ast, &inner);
        }

        Tuple(types) => {
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    out.push_str("_");
                }
                append_type_name_hint(out, ast, ty);
            }
        }

        Optional(inner) => {
            out.push_str("option_");
            append_type_name_hint(out, ast, &inner);
        }

        ForeignHandle => out.push_str("ptr"),
    }
}

pub fn append_type_name_hints(out: &mut String, ast: &AST, types: &[RustType]) {
    let mut i = 0;
    while i < types.len() {
        if i > 0 {
            out.push('_');
        }
        let ty = &types[i];
        let mut counter = 1;
        i += 1;
        while i < types.len() && ty == &types[i] {
            counter += 1;
            i += 1;
        }
        if counter > 1 {
            _ = write!(out, "{counter}_");
        }
        append_type_name_hint(out, ast, ty);
    }
}
