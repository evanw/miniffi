use super::*;
use std::collections::HashSet;
use syn::{Attribute, Fields, ext::IdentExt};

pub fn scan_ast(file: &FileData, parsed: &syn::File, warnings: &mut Vec<Warning>) -> AST {
    use syn::{Item, Visibility::Public};

    let mut ast = AST {
        structs: Vec::new(),
        enums: Vec::new(),
        traits: Vec::new(),
        consts: Vec::new(),
        fns: Vec::new(),
    };

    // Pass 1: Scan for type names
    for item in &parsed.items {
        match item {
            Item::Struct(item) => {
                let Public(_) = item.vis else { continue };
                ast.structs.push(RustStruct {
                    name: item.ident.unraw().to_string(),
                    fields: Vec::new(),
                    derives_partial_eq: derives_partial_eq(&item.attrs),
                });
            }

            Item::Enum(item) => {
                let Public(_) = item.vis else { continue };
                ast.enums.push(RustEnum {
                    name: item.ident.unraw().to_string(),
                    variants: Vec::new(),
                    derives_partial_eq: derives_partial_eq(&item.attrs),
                });
            }

            Item::Trait(item) => {
                let Public(_) = item.vis else { continue };
                ast.traits.push(RustTrait {
                    name: item.ident.unraw().to_string(),
                    fns: Vec::new(),
                });
            }

            _ => continue,
        }
    }

    // Pass 2: Scan for type contents
    'next_item: for item in &parsed.items {
        match item {
            Item::Struct(item) => {
                let Public(_) = item.vis else { continue };
                let name = item.ident.unraw().to_string();
                let fields = match parse_fields(file, &ast, &item.fields, ("struct", &name)) {
                    Ok(fields) => fields,
                    Err(warning) => {
                        warnings.push(warning);
                        continue;
                    }
                };
                for s in &mut ast.structs {
                    if s.name == name {
                        s.fields = fields;
                        break;
                    }
                }
            }

            Item::Enum(item) => {
                let Public(_) = item.vis else { continue };
                let enum_name = item.ident.unraw().to_string();
                let mut variants = Vec::new();
                let mut discriminant = 0;

                for v in &item.variants {
                    let name = v.ident.unraw().to_string();
                    let full_name = format!("{enum_name}::{name}");
                    let fields = match parse_fields(file, &ast, &v.fields, ("variant", &full_name))
                    {
                        Ok(fields) => fields,
                        Err(warning) => {
                            warnings.push(warning);
                            continue 'next_item;
                        }
                    };
                    if let Some((_, expr)) = &v.discriminant {
                        if let Some(value) = expr_to_numeric_literal(expr) {
                            discriminant = value;
                        } else {
                            warnings.push(unsupported(
                                file,
                                "discriminant",
                                expr.span(),
                                ("variant", &name),
                                Some(("enum", &enum_name)),
                                "must be an integer literal",
                            ));
                        }
                    }
                    variants.push(RustVariant {
                        name,
                        discriminant,
                        fields,
                    });
                    discriminant = discriminant.wrapping_add(1);
                }

                for e in &mut ast.enums {
                    if e.name == enum_name {
                        e.variants = variants;
                        break;
                    }
                }
            }

            _ => continue,
        }
    }

    // Prune infinitely-sized types so the code generators don't stack overflow
    for i in 0..ast.structs.len() {
        for j in 0..ast.structs[i].fields.len() {
            if is_infinite_size(&ast, &ast.structs[i].fields[j].ty) {
                ast.structs[i].fields[j].ty = RustType::Tuple(Vec::new());
            }
        }
    }

    // Pass 3: Scan for code
    'next_item: for item in &parsed.items {
        match item {
            Item::Const(item) => {
                let Public(_) = item.vis else { continue };
                let name = item.ident.unraw().to_string();
                let Some(ty) = to_rust_type(&ast, &item.ty) else {
                    warnings.push(unsupported(
                        file,
                        "type",
                        item.ty.span(),
                        ("constant", &name),
                        None,
                        "",
                    ));
                    continue;
                };
                let Some(val) = to_rust_val(&item.expr, &ty) else {
                    warnings.push(unsupported(
                        file,
                        "value",
                        item.expr.span(),
                        ("constant", &name),
                        None,
                        "only literals are supported",
                    ));
                    continue;
                };
                ast.consts.push(RustConst { name, ty, val });
            }

            Item::Fn(item) => {
                let Public(_) = item.vis else { continue };
                let f = match to_rust_fn(file, &ast, &item.sig, None) {
                    Ok(f) => f,
                    Err(warning) => {
                        warnings.push(warning);
                        continue 'next_item;
                    }
                };
                ast.fns.push(f);
            }

            Item::Trait(item) => {
                let Public(_) = item.vis else { continue };
                let name = item.ident.unraw().to_string();
                let mut fns = Vec::new();

                for item in &item.items {
                    let syn::TraitItem::Fn(item) = item else {
                        warnings.push(unsupported(
                            file,
                            "item",
                            item.span(),
                            ("trait", &name),
                            None,
                            "only functions are supported",
                        ));
                        continue 'next_item;
                    };
                    let f = match to_rust_fn(file, &ast, &item.sig, Some(&name)) {
                        Ok(f) => f,
                        Err(warning) => {
                            warnings.push(warning);
                            continue 'next_item;
                        }
                    };
                    fns.push(f);
                }

                for t in &mut ast.traits {
                    if t.name == name {
                        t.fns = fns;
                        break;
                    }
                }
            }

            _ => continue,
        }
    }

    ast
}

// Detect "#[derive(PartialEq)]"
fn derives_partial_eq(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if let syn::Meta::List(syn::MetaList { path, .. }) = &attr.meta {
            if path.is_ident("derive") {
                let mut found = false;
                _ = attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("PartialEq") {
                        found = true;
                    }
                    Ok(())
                });
                if found {
                    return true;
                }
            }
        }
    }
    false
}

fn parse_fields(
    file: &FileData,
    ast: &AST,
    fields: &Fields,
    extra: (&str, &str),
) -> Result<Vec<RustField>, Warning> {
    let mut result = Vec::new();

    match fields {
        syn::Fields::Unit => {}

        syn::Fields::Unnamed(item) => {
            for (i, field) in item.unnamed.iter().enumerate() {
                let name = format!("{i}");
                let Some(ty) = to_rust_type(&ast, &field.ty) else {
                    return Err(unsupported(
                        file,
                        "type",
                        field.ty.span(),
                        ("field", &name),
                        Some(extra),
                        "",
                    ));
                };
                result.push(RustField { name, ty });
            }
        }

        syn::Fields::Named(item) => {
            for field in &item.named {
                let name = field.ident.as_ref().unwrap().to_string();
                let Some(ty) = to_rust_type(&ast, &field.ty) else {
                    return Err(unsupported(
                        file,
                        "type",
                        field.ty.span(),
                        ("field", &name),
                        Some(extra),
                        "",
                    ));
                };
                result.push(RustField { name, ty });
            }
        }
    }

    Ok(result)
}

fn to_rust_fn(
    file: &FileData,
    ast: &AST,
    sig: &syn::Signature,
    trait_name: Option<&str>,
) -> Result<RustFn, Warning> {
    let name = sig.ident.unraw().to_string();
    let mut args = Vec::new();
    let mut self_span = None;
    let extra = trait_name.map(|name| ("trait", name));

    for arg in &sig.inputs {
        match arg {
            syn::FnArg::Typed(arg) => {
                let Some(name) = pat_to_single_ident(&arg.pat) else {
                    return Err(unsupported(
                        file,
                        "pattern",
                        arg.pat.span(),
                        ("function", &name),
                        None,
                        "only identifiers are supported",
                    ));
                };
                let Some(ty) = to_rust_type(ast, &arg.ty) else {
                    return Err(unsupported(
                        file,
                        "type",
                        arg.ty.span(),
                        ("argument", &name),
                        extra,
                        "",
                    ));
                };
                args.push(RustArg { name, ty });
            }

            syn::FnArg::Receiver(arg) => {
                if arg.reference.is_none() || arg.colon_token.is_some() || arg.mutability.is_some()
                {
                    return Err(unsupported(
                        file,
                        "receiver",
                        arg.span(),
                        ("argument", &name),
                        extra,
                        "use `&self` instead",
                    ));
                }
                self_span = Some(arg.self_token.span());
            }
        }
    }

    // All trait methods must have a "self" receiver
    match (trait_name, self_span) {
        (Some(trait_name), None) => {
            return Err(unsupported(
                file,
                "function",
                sig.ident.span(),
                ("trait", trait_name),
                None,
                "methods on public traits must use `&self`",
            ));
        }
        (None, Some(self_span)) => {
            return Err(unsupported(
                file,
                "argument",
                self_span,
                ("function", &name),
                extra,
                "",
            ));
        }
        _ => {}
    }

    let returns = match &sig.output {
        syn::ReturnType::Default => None,
        syn::ReturnType::Type(_, ty) => {
            let Some(ty) = to_rust_type(ast, &ty) else {
                return Err(unsupported(
                    file,
                    "return type",
                    ty.span(),
                    ("function", &name),
                    extra,
                    "",
                ));
            };
            let mut names = NameSet::default();
            for arg in &args {
                names.add(arg.name.clone());
            }
            let name = names.create("ret");
            Some(RustArg { name, ty })
        }
    };

    Ok(RustFn {
        name,
        args,
        returns,
    })
}

fn to_rust_type(ast: &AST, ty: &syn::Type) -> Option<RustType> {
    if let Some(ident) = type_to_single_ident(ty) {
        return match () {
            () if ident == "bool" => Some(RustType::Bool),

            () if ident == "u8" => Some(RustType::U8),
            () if ident == "u16" => Some(RustType::U16),
            () if ident == "u32" => Some(RustType::U32),
            () if ident == "usize" => Some(RustType::Usize),
            () if ident == "u64" => Some(RustType::U64),

            () if ident == "i8" => Some(RustType::I8),
            () if ident == "i16" => Some(RustType::I16),
            () if ident == "i32" => Some(RustType::I32),
            () if ident == "isize" => Some(RustType::Isize),
            () if ident == "i64" => Some(RustType::I64),

            () if ident == "f32" => Some(RustType::F32),
            () if ident == "f64" => Some(RustType::F64),

            () if ident == "String" => Some(RustType::OwnStr),

            _ => {
                if let Some((i, _)) = ast
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| ident == &s.name)
                {
                    Some(RustType::Struct(i))
                } else if let Some((i, _)) =
                    ast.enums.iter().enumerate().find(|(_, e)| ident == &e.name)
                {
                    Some(RustType::Enum(i))
                } else {
                    None
                }
            }
        };
    }

    if let Some((name, inner)) = type_to_single_ident_with_argument(ast, ty) {
        return if name == "Rc" || name == "rc::Rc" || name == "std::rc::Rc" {
            match &inner {
                RustType::DynTrait(_) => Some(RustType::Ptr(RustPtr::Rc, inner.into())),
                _ => None,
            }
        } else if name == "Box" || name == "boxed::Box" || name == "std::boxed::Box" {
            Some(RustType::Ptr(RustPtr::Box, inner.into()))
        } else if name == "Vec" || name == "vec::Vec" || name == "std::vec::Vec" {
            Some(RustType::Vector(inner.into()))
        } else if name == "Option" || name == "option::Option" || name == "std::option::Option" {
            Some(RustType::Optional(inner.into()))
        } else {
            None
        };
    }

    match ty {
        syn::Type::Tuple(syn::TypeTuple { elems, .. }) => {
            let mut types = Vec::new();
            for elem in elems {
                types.push(to_rust_type(ast, elem)?);
            }
            Some(RustType::Tuple(types))
        }

        syn::Type::Reference(syn::TypeReference {
            mutability: None,
            elem,
            ..
        }) => {
            if let Some(ident) = type_to_single_ident(elem) {
                match () {
                    () if ident == "str" => Some(RustType::RefStr),
                    _ => None,
                }
            } else {
                None
            }
        }

        syn::Type::TraitObject(syn::TypeTraitObject { bounds, .. }) if bounds.len() == 1 => {
            if let syn::TypeParamBound::Trait(syn::TraitBound { path, .. }) = &bounds[0] {
                if let Some(ident) = path.get_ident() {
                    if let Some((i, _)) = ast
                        .traits
                        .iter()
                        .enumerate()
                        .find(|(_, t)| ident == &t.name)
                    {
                        Some(RustType::DynTrait(i))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }

        _ => None,
    }
}

fn to_rust_val(expr: &syn::Expr, ty: &RustType) -> Option<RustVal> {
    match expr {
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Bool(lit),
            ..
        }) => match ty {
            RustType::Bool => Some(RustVal::Bool(lit.value())),
            _ => None,
        },

        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(lit),
            ..
        }) => match ty {
            RustType::U8 => Some(RustVal::U8(lit.base10_parse().ok()?)),
            RustType::U16 => Some(RustVal::U16(lit.base10_parse().ok()?)),
            RustType::U32 => Some(RustVal::U32(lit.base10_parse().ok()?)),
            RustType::U64 => Some(RustVal::U64(lit.base10_parse().ok()?)),

            RustType::I8 => Some(RustVal::I8(lit.base10_parse().ok()?)),
            RustType::I16 => Some(RustVal::I16(lit.base10_parse().ok()?)),
            RustType::I32 => Some(RustVal::I32(lit.base10_parse().ok()?)),
            RustType::I64 => Some(RustVal::I64(lit.base10_parse().ok()?)),

            _ => None,
        },

        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Float(lit),
            ..
        }) => match ty {
            RustType::F32 => Some(RustVal::F32(lit.base10_parse().ok()?)),
            RustType::F64 => Some(RustVal::F64(lit.base10_parse().ok()?)),
            _ => None,
        },

        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit),
            ..
        }) => match ty {
            RustType::RefStr => Some(RustVal::Str(lit.value())),
            _ => None,
        },

        syn::Expr::Unary(syn::ExprUnary {
            op: syn::UnOp::Neg(_),
            expr,
            ..
        }) => match &**expr {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Int(lit),
                ..
            }) => match ty {
                RustType::I8 => Some(RustVal::I8(
                    format!("-{}", lit.base10_digits()).parse().ok()?,
                )),
                RustType::I16 => Some(RustVal::I16(
                    format!("-{}", lit.base10_digits()).parse().ok()?,
                )),
                RustType::I32 => Some(RustVal::I32(
                    format!("-{}", lit.base10_digits()).parse().ok()?,
                )),
                RustType::I64 => Some(RustVal::I64(
                    format!("-{}", lit.base10_digits()).parse().ok()?,
                )),
                _ => None,
            },

            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Float(lit),
                ..
            }) => match ty {
                RustType::F32 => Some(RustVal::F32(
                    format!("-{}", lit.base10_digits()).parse().ok()?,
                )),
                RustType::F64 => Some(RustVal::F64(
                    format!("-{}", lit.base10_digits()).parse().ok()?,
                )),
                _ => None,
            },

            _ => None,
        },

        _ => None,
    }
}

fn expr_to_numeric_literal<T>(expr: &syn::Expr) -> Option<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Display,
{
    match expr {
        // Positive
        syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Int(lit),
            ..
        }) => lit.base10_parse().ok(),

        // Negative
        syn::Expr::Unary(syn::ExprUnary {
            op: syn::UnOp::Neg(_),
            expr,
            ..
        }) => match &**expr {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Int(lit),
                ..
            }) => format!("-{}", lit.base10_digits()).parse().ok(),
            _ => None,
        },

        _ => None,
    }
}

fn path_to_string_with_argument(ast: &AST, path: &syn::Path) -> Option<(String, RustType)> {
    let last = path.segments.last()?;
    let syn::PathArguments::AngleBracketed(inner) = &last.arguments else {
        return None;
    };
    if inner.args.len() == 1 {
        if let syn::GenericArgument::Type(ty) = &inner.args[0] {
            if let Some(inner) = to_rust_type(ast, ty) {
                let mut name = String::new();
                for (i, segment) in path.segments.iter().enumerate() {
                    if i + 1 < path.segments.len() {
                        let syn::PathArguments::None = segment.arguments else {
                            return None;
                        };
                    }
                    if i > 0 {
                        name.push_str("::");
                    }
                    name.push_str(&segment.ident.unraw().to_string());
                }
                return Some((name, inner));
            }
        }
    }
    None
}

fn type_to_single_ident(ty: &syn::Type) -> Option<&syn::Ident> {
    if let syn::Type::Path(syn::TypePath { qself: None, path }) = ty {
        return path.get_ident();
    }
    None
}

fn type_to_single_ident_with_argument(ast: &AST, ty: &syn::Type) -> Option<(String, RustType)> {
    if let syn::Type::Path(syn::TypePath { qself: None, path }) = ty {
        return path_to_string_with_argument(ast, path);
    }
    None
}

fn pat_to_single_ident(pat: &syn::Pat) -> Option<String> {
    match pat {
        syn::Pat::Ident(syn::PatIdent { ident, .. }) => Some(ident.unraw().to_string()),
        syn::Pat::Wild(_) => Some("_".to_string()),
        _ => None,
    }
}

fn unsupported(
    file: &FileData,
    what: &str,
    span: proc_macro2::Span,
    outer: (&str, &str),
    extra: Option<(&str, &str)>,
    note: &str,
) -> Warning {
    let text = match span.source_text() {
        None => what.into(),
        Some(text) => format!("{what} `{text}`"),
    };
    let mut message = format!("unsupported {text} for {} `{}`", outer.0, outer.1);
    if let Some((what, name)) = extra {
        _ = write!(message, " in {what} `{name}`");
    }
    make_warning(file, message, note, span)
}

pub fn make_warning(
    file: &FileData,
    message: String,
    note: &str,
    span: proc_macro2::Span,
) -> Warning {
    let start = span.start();
    let end = span.end();
    let line = start.line;
    let column = start.column + 1;
    let code = file
        .contents
        .lines()
        .skip(line.saturating_sub(1))
        .next()
        .unwrap_or("")
        .to_string();
    let len = if end.line == start.line {
        code.len().min(end.column)
    } else {
        code.len()
    }
    .saturating_sub(start.column);
    Warning {
        path: file.path.clone(),
        line,
        column,
        len,
        message,
        code,
        note: note.to_string(),
    }
}

fn is_infinite_size(ast: &AST, ty: &RustType) -> bool {
    fn visit(ast: &AST, ty: &RustType, visited: &mut HashSet<RustType>) -> bool {
        use RustType::*;
        match ty {
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
