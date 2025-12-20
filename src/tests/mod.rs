use super::*;
use std::process::{Command, Output};
use std::sync::{Mutex, OnceLock};

mod cases;
mod cpp;
mod swift;
mod warnings;
mod wasm;

macro_rules! test_all {
    () => {
        #[test]
        fn wasm() {
            begin_test();
            wasm::run_test(file!(), &test_case());
            end_test();
        }

        #[test]
        fn swift() {
            begin_test();
            swift::run_test(file!(), &test_case());
            end_test();
        }

        #[test]
        fn cpp() {
            begin_test();
            cpp::run_test(file!(), &test_case());
            end_test();
        }
    };
}
use test_all;

#[derive(Default)]
struct TestCase {
    rust: &'static str,
    checks: Vec<Expr>,

    js_pre: &'static str,
    js_post: &'static str,

    swift_pre: &'static str,
    swift_post: &'static str,

    cpp_pre: &'static str,
    cpp_post: &'static str,
}

#[derive(Clone)]
enum ExprTy {
    TyBool,
    TyU8,
    TyU16,
    TyU32,
    TyUsize,
    TyU64,
    TyI8,
    TyI16,
    TyI32,
    TyIsize,
    TyI64,
    TyF32,
    TyF64,
    TyString,
    TyTuple(Box<[ExprTy]>),
    TyOption(Box<ExprTy>),
    TyBox(Box<ExprTy>),
    TyVec(Box<ExprTy>),
    TyName(&'static str),
}
use ExprTy::*;

#[derive(Clone)]
enum Expr {
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    Usize(usize),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    Isize(isize),
    I64(i64),
    F32(f32),
    F64(f64),
    Str(&'static str),
    Export(&'static str),
    Enum(&'static str, &'static str),
    EnumPayload(&'static str, &'static str, Box<[(&'static str, Expr)]>),
    DeclareMutableLocal(&'static str, Box<Expr>),
    DeclareImmutableLocal(&'static str, Box<Expr>),
    LoadLocal(&'static str),
    StoreLocal(&'static str, Box<Expr>),
    NewBox(ExprTy, &'static str, Box<[Expr]>),
    NewRc(ExprTy, &'static str, Box<[Expr]>),
    Tuple(Box<[Expr]>),
    Vector(Box<[Expr]>),
    EmptyVector(ExprTy), // This needs a type to help type inference
    Struct(&'static str, Box<[(&'static str, Expr)]>),
    AssertEqual(Box<Expr>, Box<Expr>),
    AssertNotEqual(Box<Expr>, Box<Expr>),
    StrConcat(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Box<[Expr]>),
    CallMethod(Box<Expr>, &'static str, Box<[Expr]>),
    TupleMember(Box<Expr>, u32),
    StructMember(Box<Expr>, &'static str),
    VectorMember(Box<Expr>, usize),
    VectorLen(Box<Expr>),
    AfterGC(Box<Expr>),
    OptNone(ExprTy),
    OptSome(Box<Expr>),
    Boxed(ExprTy, Box<Expr>),
}
use Expr::*;

static COUNTER: OnceLock<Mutex<usize>> = OnceLock::new();

fn counter() -> &'static Mutex<usize> {
    COUNTER.get_or_init(|| Mutex::new(0))
}

fn begin_test() {
    let mut counter = counter().lock().unwrap();
    *counter += 1;
}

fn end_test() {
    let mut counter = counter().lock().unwrap();
    *counter -= 1;

    // Clean up after the last test (intentionally done while the lock is held)
    if *counter == 0 {
        let test_dir = &PathBuf::from(".temp");
        if let Ok(entries) = std::fs::read_dir(test_dir) {
            for entry in entries {
                let name = entry.unwrap().file_name();
                if name != "target"
                    && name != "node_modules"
                    && name != "package.json"
                    && name != "package-lock.json"
                {
                    // Each test that passes deletes its test directory. So if
                    // we encounter anything here that's not in the allow-list,
                    // then it's likely a test failure. Don't remove the target
                    // directory if there's a test that failed.
                    return;
                }
            }

            // If all tests passed, automatically delete the target directory.
            // This prevents the target directory from taking up multiple
            // gigabytes of file system space when it's not needed.
            _ = std::fs::remove_dir_all(test_dir.join("target"));
        }
    }

    drop(counter);
}

fn check_output(output: Output) {
    if !output.status.success() {
        if output.stdout.is_empty() && output.stderr.is_empty() {
            panic!("(empty output)");
        }
        panic!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

fn cargo_build(test_dir: &Path, target: Option<&str>) {
    // Try offline-only first, otherwise cargo breaks when the internet is down
    for offline in [true, false] {
        let mut cmd = Command::new("cargo");
        cmd.current_dir(test_dir)
            .env("RUST_BACKTRACE", "1")
            .arg("build")
            .arg("--quiet")
            .arg("--color=always")
            .arg("--message-format=short")
            .arg(format!("--target-dir=../target"));
        if offline {
            cmd.arg("--offline");
        }
        if let Some(target) = target {
            cmd.arg(format!("--target={target}"));
        }
        let output = cmd.output().expect("failed to run command `cargo`");
        if offline && !output.status.success() {
            // Fall back to trying online if offline-only failed
            continue;
        }
        check_output(output);
        return;
    }
}

fn copy_snapshot(name: &str, from: PathBuf) {
    let snapshot_dir = &Path::new("src").join("tests").join("snapshots").join(name);
    let to = snapshot_dir.join(from.file_name().unwrap());
    std::fs::create_dir_all(snapshot_dir)
        .expect("failed to create directory `src/tests/snapshots`");
    std::fs::copy(&from, &to).expect(&format!(
        "failed to copy snapshot from `{}` to `{}`",
        from.display(),
        to.display()
    ));
}

fn rust_leak_check() -> &'static str {
    // Test with "cargo +nightly test" to get memory leak checks
    if let Ok(toolchain) = std::env::var("RUSTUP_TOOLCHAIN") {
        if toolchain.contains("nightly") {
            return r#"
            #![feature(allocator_api)]
            struct LeakCheckAlloc;

            unsafe impl std::alloc::GlobalAlloc for LeakCheckAlloc {
                unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
                    unsafe {
                        TOTAL_MEMORY += layout.size();
                        std::alloc::System.alloc(layout)
                    }
                }

                unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
                    unsafe {
                        TOTAL_MEMORY -= layout.size();
                        std::alloc::System.dealloc(ptr, layout);
                    }
                }
            }

            #[global_allocator]
            static ALLOCATOR: LeakCheckAlloc = LeakCheckAlloc;
            static mut TOTAL_MEMORY: usize = 0;

            pub fn rust_mem_leaked() -> usize {
                unsafe { TOTAL_MEMORY }
            }
            "#;
        }
    }

    r#"
    pub fn rust_mem_leaked() -> usize {
        0
    }
    "#
}
