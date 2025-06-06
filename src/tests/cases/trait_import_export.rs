use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Imported {
            fn run(&self, exported: Rc<dyn Exported>) -> i32;
        }

        pub trait Exported {
            fn add(&self, x: i32, y: i32) -> i32;
        }

        struct ExportedImpl;
        static mut COUNTER: u32 = 0;

        pub fn get_counter() -> u32 {
            unsafe { COUNTER }
        }

        impl Exported for ExportedImpl {
            fn add(&self, x: i32, y: i32) -> i32 {
                x + y
            }
        }

        impl Drop for ExportedImpl {
            fn drop(&mut self) {
                unsafe { COUNTER -= 1; }
            }
        }

        pub fn set_imported(imported: Rc<dyn Imported>) -> i32 {
            unsafe { COUNTER += 1; }
            imported.run(Rc::new(ExportedImpl))
        }
    "#;

    case.js_pre = "
        class ImportedImpl {
            run(exported) {
                return exported.add(1, 2)
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
            func run(_ exported: Exported) -> Int32 {
                return exported.add(1, 2)
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
            int32_t run(std::shared_ptr<rust::Exported> exported) {
                return exported->add(1, 2);
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            Call(
                Export("set_imported").into(),
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
