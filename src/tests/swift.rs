use super::*;
use std::borrow::Cow;

pub fn run_test(name: &str, case: &TestCase, edition: usize) {
    let name = Path::new(name).file_stem().unwrap();
    let name = &format!("swift_{}_{edition}", name.to_string_lossy());
    let test_dir = &Path::new(".temp").join(name);
    let src_dir = &test_dir.join("src");
    std::fs::create_dir_all(src_dir).expect("failed to create directory `src`");

    let TestCase {
        rust,
        swift_pre,
        swift_post,
        checks,
        ..
    } = case;
    let checks = checks.iter().map(to_str).collect::<Vec<_>>().join("\n");

    std::fs::write(
        src_dir.join("lib.rs"),
        format!(
            r#"
            #![deny(warnings)]
            {}
            {rust}
            include!(concat!(env!("OUT_DIR"), "/miniffi.rs"));
            "#,
            rust_leak_check()
        ),
    )
    .expect("failed to write file `src/lib.rs`");

    std::fs::write(
        test_dir.join("build.rs"),
        format!(
            r#"
            fn main() {{
                use miniffi::*;
                let mut result = SwiftTarget::new()
                    .rust_edition({edition})
                    .write_swift_to("ffi.swift")
                    .write_header_to("ffi.h")
                    .build()
                    .convert_warnings_to_errors();
                result.finish();
                for output in &result.output_files {{
                    if output.path.extension().unwrap() == "rs" {{
                        std::fs::copy(&output.path, "miniffi.rs").unwrap();
                    }}
                }}
            }}
            "#
        ),
    )
    .expect("failed to write file `build.rs`");

    std::fs::write(
        test_dir.join("Cargo.toml"),
        format!(
            r#"
            [package]
            name = "{name}"
            version = "0.1.0"
            edition = "{edition}"
            [lib]
            crate-type = ["staticlib"]
            [build-dependencies]
            miniffi = {{ path = "../.." }}
            "#
        ),
    )
    .expect("failed to write file `Cargo.toml`");

    std::fs::write(
        test_dir.join("main.swift"),
        format!(
            r#"
            extension DefaultStringInterpolation {{
                mutating func appendInterpolation<T>(_ x: T?) {{
                    appendInterpolation(String(describing: x))
                }}
            }}
            {swift_pre}
            do {{
                {checks}
            }}
            {swift_post}
            assert(rust_mem_leaked() == 0, "\(rust_mem_leaked()) bytes leaked")
            "#
        ),
    )
    .expect("failed to write file `main.swift`");

    cargo_build(test_dir, None);

    copy_snapshot(name, test_dir.join("miniffi.rs"));
    copy_snapshot(name, test_dir.join("ffi.swift"));
    copy_snapshot(name, test_dir.join("ffi.h"));

    check_output(
        Command::new("swiftc")
            .current_dir(test_dir)
            .arg("main.swift")
            .arg("ffi.swift")
            .args(["-import-objc-header", "ffi.h"])
            .arg(format!("../target/debug/lib{name}.a"))
            .arg("-warnings-as-errors")
            .output()
            .expect("failed to run command `swiftc`"),
    );

    check_output(
        Command::new("./main")
            .current_dir(test_dir)
            .env("RUST_BACKTRACE", "1")
            .output()
            .expect("failed to run command `./main`"),
    );

    _ = std::fs::remove_dir_all(test_dir);
}

fn ty_to_str(ty: &ExprTy) -> String {
    match ty {
        TyBool => "Bool".to_string(),
        TyU8 => "UInt8".to_string(),
        TyU16 => "UInt16".to_string(),
        TyU32 => "UInt32".to_string(),
        TyUsize => "UInt".to_string(),
        TyU64 => "UInt64".to_string(),
        TyI8 => "Int8".to_string(),
        TyI16 => "Int16".to_string(),
        TyI32 => "Int32".to_string(),
        TyIsize => "Int".to_string(),
        TyI64 => "Int64".to_string(),
        TyF32 => "Float32".to_string(),
        TyF64 => "Float64".to_string(),
        TyString => "String".to_string(),
        TyTuple(x) => format!(
            "({})",
            x.iter().map(ty_to_str).collect::<Vec<_>>().join(", ")
        ),
        TyOption(x) => format!("{}?", ty_to_str(x)),
        TyVec(x) => format!("[{}]", ty_to_str(x)),
        TyBox(x) => ty_to_str(x),
        TyName(x) => format!("{x}"),
    }
}

fn key_to_str(x: &'static str) -> Cow<'static, str> {
    match starts_with_digit(x) {
        false => Cow::Borrowed(x),
        true => Cow::Owned(format!("_{x}")),
    }
}

fn many_to_str(x: &[Expr]) -> String {
    x.iter().map(to_str).collect::<Vec<_>>().join(", ")
}

fn to_str(expr: &Expr) -> String {
    match expr {
        Bool(x) => format!("{x}"),
        U8(x) => format!("UInt8({x})"),
        U16(x) => format!("UInt16({x})"),
        U32(x) => format!("UInt32({x})"),
        Usize(x) => format!("UInt({x})"),
        U64(x) => format!("UInt64({x})"),
        I8(x) => format!("Int8({x})"),
        I16(x) => format!("Int16({x})"),
        I32(x) => format!("Int32({x})"),
        Isize(x) => format!("Int({x})"),
        I64(x) => format!("Int64({x})"),
        F32(x) => format!("Float32({x:?})"),
        F64(x) => format!("Float64({x:?})"),
        Str(x) => format!("{x:?}"),
        Export(x) => format!("{x}"),
        Enum(x, y) => format!("{x}.{y}"),
        EnumPayload(x, y, z) if z.is_empty() => format!("{x}.{y}"),
        EnumPayload(x, y, z) => format!(
            "{x}.{y}({})",
            z.iter()
                .map(|(k, v)| match starts_with_digit(k) {
                    false => format!("{}: {}", key_to_str(k), to_str(v)),
                    true => to_str(v),
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
        DeclareMutableLocal(x, y) => format!("var {x} = {}", to_str(y)),
        DeclareImmutableLocal(x, y) => format!("let {x} = {}", to_str(y)),
        LoadLocal(x) => format!("{x}"),
        StoreLocal(x, y) => format!("{x} = {}", to_str(y)),
        NewBox(_, x, y) | NewRc(_, x, y) => format!("{x}({})", many_to_str(y)),
        Tuple(x) => format!("({})", many_to_str(x)),
        Vector(x) => format!("[{}]", many_to_str(x)),
        EmptyVector(_) => "[]".to_string(),
        Struct(x, y) => format!(
            "{x}({})",
            y.iter()
                .map(|(k, v)| format!("{}: {}", key_to_str(k), to_str(v)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        AssertEqual(x, y) => match &**y {
            F32(y) if *y == 0.0 => {
                let sign = if y.is_sign_negative() {
                    "minus"
                } else {
                    "plus"
                };
                format!(
                    r#"do {{
                        let left = {}
                        assert(left == 0.0 && left.sign == .{sign}, "\(left) == {y:?}")
                    }}"#,
                    to_str(x)
                )
            }
            F64(y) if *y == 0.0 => {
                let sign = if y.is_sign_negative() {
                    "minus"
                } else {
                    "plus"
                };
                format!(
                    r#"do {{
                        let left = {}
                        assert(left == 0.0 && left.sign == .{sign}, "\(left) == {y:?}")
                    }}"#,
                    to_str(x)
                )
            }
            _ => {
                let ty = match (&**x, &**y) {
                    // Avoid "constant inferred to have type '()', which may be unexpected"
                    (Tuple(z), _) | (_, Tuple(z)) if z.is_empty() => ": ()",
                    _ => "",
                };
                format!(
                    r#"do {{
                        let left{ty} = {}
                        let right{ty} = {}
                        assert(left == right, "\(left) == \(right)")
                    }}"#,
                    to_str(x),
                    to_str(y)
                )
            }
        },
        AssertNotEqual(x, y) => match &**y {
            F32(y) if *y == 0.0 => {
                let sign = if y.is_sign_negative() {
                    "minus"
                } else {
                    "plus"
                };
                format!(
                    r#"do {{
                        let left = {}
                        assert(left != 0.0 || left.sign != .{sign}, "\(left) != {y:?}")
                    }}"#,
                    to_str(x)
                )
            }
            F64(y) if *y == 0.0 => {
                let sign = if y.is_sign_negative() {
                    "minus"
                } else {
                    "plus"
                };
                format!(
                    r#"do {{
                        let left = {}
                        assert(left != 0.0 || left.sign == .{sign}, "\(left) != {y:?}")
                    }}"#,
                    to_str(x)
                )
            }
            _ => {
                let ty = match (&**x, &**y) {
                    // Avoid "constant inferred to have type '()', which may be unexpected"
                    (Tuple(z), _) | (_, Tuple(z)) if z.is_empty() => ": ()",
                    _ => "",
                };
                format!(
                    r#"do {{
                        let left{ty} = {}
                        let right{ty} = {}
                        assert(left != right, "\(left) != \(right)")
                    }}"#,
                    to_str(x),
                    to_str(y)
                )
            }
        },
        StrConcat(x, y) => format!("({}) + ({})", to_str(x), to_str(y)),
        Call(x, y) => format!("{}({})", to_str(x), many_to_str(y)),
        CallMethod(x, y, z) => format!("({}).{y}({})", to_str(x), many_to_str(z)),
        TupleMember(x, y) => format!("({}).{y}", to_str(x)),
        StructMember(x, y) => format!("({}).{y}", to_str(x)),
        VectorMember(x, y) => format!("({})[{y}]", to_str(x)),
        VectorLen(x) => format!("UInt(({}).count)", to_str(x)),
        AfterGC(x) => to_str(x),
        OptNone(x) => format!("Optional<{}>.none", ty_to_str(x)),
        OptSome(x) => format!("Optional.some({})", to_str(x)),
        Boxed(_, x) => to_str(x),
    }
}
