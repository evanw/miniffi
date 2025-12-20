use super::*;

pub fn run_test(name: &str, case: &TestCase, edition: usize) {
    let name = Path::new(name).file_stem().unwrap();
    let name = &format!("wasm_{}_{edition}", name.to_string_lossy());
    let test_dir = &std::env::current_dir().unwrap().join(".temp").join(name);
    let src_dir = &test_dir.join("src");
    let pkg_json_dir = &test_dir.join("..");
    let tsc_path = &pkg_json_dir
        .join("node_modules")
        .join("typescript")
        .join("lib")
        .join("tsc.js");
    std::fs::create_dir_all(src_dir).expect("failed to create directory `src`");

    let TestCase {
        rust,
        js_pre,
        js_post,
        checks,
        ..
    } = case;
    let checks = checks.iter().map(to_str).collect::<Vec<_>>().join(";\n");

    // Install TypeScript for type checking
    if !tsc_path.exists() {
        std::fs::write(pkg_json_dir.join("package.json"), "{}")
            .expect("failed to write file `package.json`");
        check_output(
            Command::new("npm")
                .current_dir(pkg_json_dir)
                .arg("i")
                .arg("typescript@5.9.2")
                .output()
                .expect("failed to run command `npm`"),
        );
    }

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
    .expect("failed to create file `src/lib.rs`");

    std::fs::write(
        test_dir.join("build.rs"),
        r#"
        fn main() {
            use miniffi::*;
            let mut result = WasmTarget::new()
                .write_js_to("ffi.mjs")
                .write_ts_to("ffi.mts")
                .write_d_ts_to("ffi.d.mts")
                .build()
                .convert_warnings_to_errors();
            result.finish();
            for output in &result.output_files {
                if output.path.extension().unwrap() == "rs" {
                    std::fs::copy(&output.path, "miniffi.rs").unwrap();
                }
            }
        }
        "#,
    )
    .expect("failed to create file `build.rs`");

    std::fs::write(
        test_dir.join("Cargo.toml"),
        format!(
            r#"
            [package]
            name = "{name}"
            version = "0.1.0"
            edition = "{edition}"
            [lib]
            crate-type = ["cdylib"]
            [build-dependencies]
            miniffi = {{ path = "../.." }}
            "#
        ),
    )
    .expect("failed to create file `Cargo.toml`");

    std::fs::write(
        test_dir.join("test.mjs"),
        format!(
            r#"
            import * as assert from "assert";
            import * as fs from "fs";
            import * as ffi from "./ffi.mjs";

            Error.stackTraceLimit = Infinity;
            await ffi.instantiate(fs.readFileSync(
                "../target/wasm32-unknown-unknown/debug/{name}.wasm"));

            {js_pre}

            // Use a function wrapper to allow for garbage collection
            async function __TEST_MAIN__() {{
                {checks}
            }}
            await __TEST_MAIN__();

            {js_post}

            if (globalThis.gc) {{
                globalThis.gc()
                await new Promise(r => setTimeout(r, 100)) // Try to run finalizers
                assert.deepStrictEqual(ffi.rust_mem_leaked(), 0, ffi.rust_mem_leaked() + " bytes leaked");
            }}
            "#
        ),
    )
    .expect("failed to create file `test.mjs`");

    std::fs::write(
        test_dir.join("tsconfig.json"),
        r#"{
            "compilerOptions": {
                "target": "ESNext",
                "module": "ESNext",
                "strict": true,
            },
        }"#,
    )
    .expect("failed to create file `tsconfig.json`");

    cargo_build(test_dir, Some("wasm32-unknown-unknown"));

    copy_snapshot(name, test_dir.join("miniffi.rs"));
    copy_snapshot(name, test_dir.join("ffi.mjs"));
    copy_snapshot(name, test_dir.join("ffi.mts"));
    copy_snapshot(name, test_dir.join("ffi.d.mts"));

    // Run the JavaScript code
    check_output(
        Command::new("node")
            .current_dir(test_dir)
            .env("FORCE_COLOR", "1")
            .arg("--expose-gc")
            .arg("test.mjs")
            .output()
            .unwrap(),
    );

    // Check for TypeScript errors
    check_output(
        Command::new("node")
            .current_dir(test_dir)
            .arg(tsc_path)
            .arg("-noEmit")
            .args(["-p", "tsconfig.json"])
            .output()
            .unwrap(),
    );

    _ = std::fs::remove_dir_all(test_dir);
}

fn to_str(expr: &Expr) -> String {
    fn many_to_str(x: &[Expr]) -> String {
        x.iter().map(to_str).collect::<Vec<_>>().join(", ")
    }
    match expr {
        Bool(x) => format!("{x}"),
        U8(x) => format!("{x}"),
        U16(x) => format!("{x}"),
        U32(x) => format!("{x}"),
        Usize(x) => format!("{x}"),
        U64(x) => format!("{x}n"),
        I8(x) => format!("{x}"),
        I16(x) => format!("{x}"),
        I32(x) => format!("{x}"),
        Isize(x) => format!("{x}"),
        I64(x) => format!("{x}n"),
        F32(x) => format!("{}", *x as f64),
        F64(x) => format!("{x}"),
        Str(x) => format!("{x:?}"),
        Export(x) => format!("ffi.{x}"),
        Enum(x, y) => format!("ffi.{x}.{y}"),
        EnumPayload(_, x, y) => format!(
            "{{ $: {x:?}{} }}",
            y.iter()
                .map(|(k, v)| format!(", {k}: {}", to_str(v)))
                .collect::<Vec<_>>()
                .join("")
        ),
        DeclareMutableLocal(x, y) => format!("let {x} = {}", to_str(y)),
        DeclareImmutableLocal(x, y) => format!("const {x} = {}", to_str(y)),
        LoadLocal(x) => format!("{x}"),
        StoreLocal(x, y) => format!("{x} = {}", to_str(y)),
        NewBox(_, x, y) | NewRc(_, x, y) => format!("new {x}({})", many_to_str(y)),
        Tuple(x) if x.is_empty() => "undefined".to_string(),
        Tuple(x) | Vector(x) => format!("[{}]", many_to_str(x)),
        EmptyVector(_) => "[]".to_string(),
        Struct(_, x) => format!(
            "{{{}}}",
            x.iter()
                .map(|(k, v)| format!("{k}: {}", to_str(v)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        AssertEqual(x, y) => match &**y {
            F32(y) if *y == 0.0 => {
                format!("assert.default(Object.is({}, {y}))", to_str(x))
            }
            F64(y) if *y == 0.0 => {
                format!("assert.default(Object.is({}, {y}))", to_str(x))
            }
            _ => {
                format!("assert.deepStrictEqual({}, {})", to_str(x), to_str(y))
            }
        },
        AssertNotEqual(x, y) => match &**y {
            F32(y) if *y == 0.0 => {
                format!("assert.default(!Object.is({}, {y}))", to_str(x))
            }
            F64(y) if *y == 0.0 => {
                format!("assert.default(!Object.is({}, {y}))", to_str(x))
            }
            _ => {
                format!("assert.notDeepStrictEqual({}, {})", to_str(x), to_str(y))
            }
        },
        StrConcat(x, y) => format!("({}) + ({})", to_str(x), to_str(y)),
        Call(x, y) => format!("{}({})", to_str(x), many_to_str(y)),
        CallMethod(x, y, z) => {
            format!("({}).{y}({})", to_str(x), many_to_str(z))
        }
        TupleMember(x, y) => format!("({})[{y}]", to_str(x)),
        StructMember(x, y) => format!("({}).{y}", to_str(x)),
        VectorMember(x, y) => format!("({})[{y}]", to_str(x)),
        VectorLen(x) => format!("({}).length", to_str(x)),
        AfterGC(x) => format!(
            "
            if (globalThis.gc) {{
                globalThis.gc()
                await new Promise(r => setTimeout(r, 100)) // Try to run finalizers
                {}
            }}
            ",
            to_str(x)
        ),
        OptNone(_) => "null".into(),
        OptSome(x) => to_str(x),
        Boxed(_, x) => to_str(x),
    }
}
