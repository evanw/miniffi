use super::*;

pub fn run_test(name: &str, case: &TestCase, edition: usize) {
    let name = Path::new(name).file_stem().unwrap();
    let name = &format!("cpp_{}_{edition}", name.to_string_lossy());
    let test_dir = &Path::new(".temp").join(name);
    let src_dir = &test_dir.join("src");
    std::fs::create_dir_all(src_dir).expect("failed to create directory `src`");

    let TestCase {
        rust,
        cpp_pre,
        cpp_post,
        checks,
        ..
    } = case;
    let checks = checks
        .iter()
        .map(|x| format!("{};", to_str(x)))
        .collect::<Vec<_>>()
        .join("\n");

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
                let mut result = CppTarget::new()
                    .rust_edition({edition})
                    .write_source_to("ffi.cpp")
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
        test_dir.join("main.cpp"),
        format!(
            r#"
            #include "ffi.h"
            #include <assert.h>
            #include <math.h>
            #include <stdlib.h>
            #include <string>
            #include <tuple>
            #include <vector>

            // "std::vector" stupidly copies the "std::initializer_list",
            // which breaks with uncopyable types such as "std::unique_ptr"
            template<typename T>
            std::vector<T> make_vector(T&& arg) {{
                std::vector<T> vec;
                vec.emplace_back(std::forward<T>(arg));
                return vec;
            }}
            template<typename T, typename... Args>
            std::vector<T> make_vector(T&& arg, Args&&... args) {{
                auto vec = make_vector(std::forward<Args>(args)...);
                vec.emplace(vec.begin(), std::forward<T>(arg));
                return vec;
            }}

            size_t cpp_mem_leaked = 0;
            size_t cpp_mem_total = 0;

            void* operator new(size_t n) {{
                cpp_mem_leaked += n;
                cpp_mem_total += n;
                size_t total = n;
                total = (n + 15) & ~15; // Align to a multiple of 16
                total += 16; // Bump up to the next multiple of 16
                uint8_t* ptr = (uint8_t*)malloc(total);
                memcpy(ptr, &n, sizeof(n));
                return ptr + 16;
            }}

            void operator delete(void* p) throw() {{
                uint8_t* ptr = (uint8_t*)p;
                ptr -= 16;
                size_t n;
                memcpy(&n, ptr, sizeof(n));
                free(ptr);
                cpp_mem_leaked -= n;
            }}

            {cpp_pre}
            int main() {{
                {{
                    {{
                        {checks}
                    }}
                    {cpp_post}
                }}

                // Make sure there are no memory leaks
                assert(cpp_mem_leaked == 0);
                assert(rust::rust_mem_leaked() == 0);
            }}
            "#
        ),
    )
    .expect("failed to write file `main.cpp`");

    cargo_build(test_dir, None);

    copy_snapshot(name, test_dir.join("miniffi.rs"));
    copy_snapshot(name, test_dir.join("ffi.cpp"));
    copy_snapshot(name, test_dir.join("ffi.h"));

    if cfg!(windows) {
        check_output(
            Command::new("cl.exe")
                .current_dir(test_dir)
                .arg("main.cpp")
                .arg("ffi.cpp")
                .arg("/std:c++17")
                .arg("/W3")
                .arg("/WX")
                .arg(format!("../target/debug/{name}.lib"))
                .arg("/EHs")
                .arg("/link")
                .arg("ntdll.lib")
                .arg("userenv.lib")
                .arg("ws2_32.lib")
                .arg("/out:main.exe")
                .output()
                .expect("failed to run command `cl.exe`"),
        );

        check_output(
            Command::new(test_dir.join("main.exe"))
                .current_dir(test_dir)
                .env("RUST_BACKTRACE", "1")
                .output()
                .expect("failed to run command `./main.exe`"),
        );
    } else {
        check_output(
            Command::new("c++")
                .current_dir(test_dir)
                .arg("main.cpp")
                .arg("ffi.cpp")
                .arg("-std=c++17")
                .arg("-Wall")
                .arg("-Werror")
                .arg(format!("../target/debug/lib{name}.a"))
                .args(["-o", "main"])
                .output()
                .expect("failed to run command `c++`"),
        );

        check_output(
            Command::new("./main")
                .current_dir(test_dir)
                .env("RUST_BACKTRACE", "1")
                .output()
                .expect("failed to run command `./main`"),
        );
    }

    _ = std::fs::remove_dir_all(test_dir);
}

fn ty_to_str(ty: &ExprTy) -> String {
    match ty {
        TyBool => "bool".to_string(),
        TyU8 => "uint8_t".to_string(),
        TyU16 => "uint16_t".to_string(),
        TyU32 => "uint32_t".to_string(),
        TyUsize => "uintptr_t".to_string(),
        TyU64 => "uint64_t".to_string(),
        TyI8 => "int8_t".to_string(),
        TyI16 => "int16_t".to_string(),
        TyI32 => "int32_t".to_string(),
        TyIsize => "intptr_t".to_string(),
        TyI64 => "int64_t".to_string(),
        TyF32 => "float".to_string(),
        TyF64 => "double".to_string(),
        TyString => "std::string".to_string(),
        TyTuple(x) => format!(
            "std::tuple<{}>",
            x.iter().map(ty_to_str).collect::<Vec<_>>().join(", ")
        ),
        TyOption(x) => format!("std::optional<{}>", ty_to_str(x)),
        TyVec(x) => format!("std::vector<{}>", ty_to_str(x)),
        TyBox(x) => format!("std::unique_ptr<{}>", ty_to_str(x)),
        TyName(x) => format!("rust::{x}"),
    }
}

fn many_to_str(x: &[Expr]) -> String {
    x.iter().map(to_str).collect::<Vec<_>>().join(", ")
}

fn to_str(expr: &Expr) -> String {
    match expr {
        Bool(x) => format!("{x}"),
        U8(x) => format!("uint8_t({x})"),
        U16(x) => format!("uint16_t({x})"),
        U32(x) => format!("uint32_t({x})"),
        Usize(x) => format!("uintptr_t({x})"),
        U64(x) => format!("uint64_t({x}ull)"),
        I8(x) => format!("int8_t({x})"),
        I16(x) => format!("int16_t({x})"),
        I32(x) => {
            if *x == std::i32::MIN {
                // Avoid a Visual C++ warning
                format!("int32_t({} - 1)", x + 1)
            } else {
                format!("int32_t({x})")
            }
        }
        Isize(x) => format!("intptr_t({x})"),
        I64(x) => {
            if *x == std::i64::MIN {
                // Avoid a clang warning
                format!("int64_t({}ll - 1)", x + 1)
            } else {
                format!("int64_t({x}ll)")
            }
        }
        F32(x) => format!("float({x})"),
        F64(x) => format!("double({x})"),
        Str(x) => format!("std::string({x:?}, {})", x.len()),
        Export(x) => format!("rust::{x}"),
        Enum(x, y) => format!("rust::{x}::{y}"),
        EnumPayload(x, y, z) => format!(
            "rust::{x}{{rust::{x}::{y}{{{}}}}}",
            z.iter()
                .map(|(_, v)| to_str(v))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        DeclareMutableLocal(x, y) => format!("auto {x} = {}", to_str(y)),
        DeclareImmutableLocal(x, y) => format!("const auto {x} = {}", to_str(y)),
        LoadLocal(x) => format!("{x}"),
        StoreLocal(x, y) => format!("{x} = {}", to_str(y)),
        NewBox(ty, x, y) => format!(
            "std::unique_ptr<{}>(new {x}({}))",
            ty_to_str(ty),
            many_to_str(y)
        ),
        NewRc(ty, x, y) => format!(
            "std::shared_ptr<{}>(new {x}({}))",
            ty_to_str(ty),
            many_to_str(y)
        ),
        Tuple(x) => format!("std::make_tuple({})", many_to_str(x)),
        Vector(x) => format!("make_vector({})", many_to_str(x)),
        EmptyVector(x) => format!("std::vector<{}>()", ty_to_str(x)),
        Struct(x, y) => format!(
            "rust::{x}{{{}}}",
            y.iter()
                .map(|(_, v)| to_str(v))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        AssertEqual(x, y) => match &**y {
            F32(y) if *y == 0.0 => {
                format!(
                    r#"{{
                        auto left = {};
                        assert(left == 0.0 && signbit(left) == {});
                    }}"#,
                    to_str(x),
                    y.is_sign_negative()
                )
            }
            F64(y) if *y == 0.0 => {
                format!(
                    r#"{{
                        auto left = {};
                        assert(left == 0.0 && signbit(left) == {});
                    }}"#,
                    to_str(x),
                    y.is_sign_negative()
                )
            }
            Str(_) => {
                // "x" might be a "std::string_view" constant
                format!("assert(std::string({}) == ({}))", to_str(x), to_str(y))
            }
            _ => format!("assert(({}) == ({}))", to_str(x), to_str(y)),
        },
        AssertNotEqual(x, y) => match &**y {
            F32(y) if *y == 0.0 => {
                format!(
                    r#"{{
                        auto left = {};
                        assert(left != 0.0 || signbit(left) != {});
                    }}"#,
                    to_str(x),
                    y.is_sign_negative()
                )
            }
            F64(y) if *y == 0.0 => {
                format!(
                    r#"{{
                        auto left = {};
                        assert(left != 0.0 || signbit(left) != {});
                    }}"#,
                    to_str(x),
                    y.is_sign_negative()
                )
            }
            Str(_) => {
                // "x" might be a "std::string_view" constant
                format!("assert(std::string({}) != ({}))", to_str(x), to_str(y))
            }
            _ => format!("assert(({}) != ({}))", to_str(x), to_str(y)),
        },
        StrConcat(x, y) => format!("({}) + ({})", to_str(x), to_str(y)),
        Call(x, y) => format!("{}({})", to_str(x), many_to_str(y)),
        CallMethod(x, y, z) => format!("({})->{y}({})", to_str(x), many_to_str(z)),
        TupleMember(x, y) => format!("std::get<{y}>({})", to_str(x)),
        StructMember(x, y) => format!("({}).{y}", to_str(x)),
        VectorMember(x, y) => format!("({}).at({y})", to_str(x)),
        VectorLen(x) => format!("({}).size()", to_str(x)),
        AfterGC(x) => to_str(x),
        OptNone(x) => format!("std::optional<{}>()", ty_to_str(x)),
        OptSome(x) => format!("std::make_optional({})", to_str(x)),
        Boxed(x, y) => format!("std::make_unique<{}>({})", ty_to_str(x), to_str(y)),
    }
}
