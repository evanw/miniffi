use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Imported {
            fn add(&self, x: i32, y: i32) -> i32;
        }

        pub trait Exported {
            fn run(&self, imported: Rc<dyn Imported>) -> i32;
        }

        struct ExportedImpl;

        impl Exported for ExportedImpl {
            fn run(&self, imported: Rc<dyn Imported>) -> i32 {
                imported.add(1, 2)
            }
        }

        static mut COUNTER: u32 = 0;

        impl Drop for ExportedImpl {
            fn drop(&mut self) {
                unsafe { COUNTER -= 1; }
            }
        }

        pub fn get_exported() -> Rc<dyn Exported> {
            unsafe { COUNTER += 1; }
            Rc::new(ExportedImpl)
        }

        pub fn get_counter() -> u32 {
            unsafe { COUNTER }
        }
    "#;

    case.js_pre = "
        class ImportedImpl {
            add(x, y) {
                return x + y
            }
        }
    ";

    case.swift_pre = "
        var counter = 0
        class ImportedImpl : Imported {
            init() {
                counter += 1
            }
            deinit {
                counter -= 1
            }
            func add(_ x: Int32, _ y: Int32) -> Int32 {
                return x + y
            }
        }
    ";

    case.cpp_pre = "
        int counter = 0;
        struct ImportedImpl : rust::Imported {
            ImportedImpl() {
                counter += 1;
            }
            ~ImportedImpl() {
                counter -= 1;
            }
            int32_t add(int32_t x, int32_t y) {
                return x + y;
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            CallMethod(
                Call(Export("get_exported").into(), [].into()).into(),
                "run",
                [NewRc(TyName("Imported"), "ImportedImpl", [].into())].into(),
            )
            .into(),
            I32(3).into(),
        ),
        AfterGC(
            AssertEqual(
                Call(Export("get_counter").into(), [].into()).into(),
                U32(0).into(),
            )
            .into(),
        ),
    ];

    case.swift_post = r#"
        assert(counter == 0, "\(counter) == \(0)")
    "#;

    case.cpp_post = "
        assert(counter == 0);
    ";

    case
}

test_all!();
