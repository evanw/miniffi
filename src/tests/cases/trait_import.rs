use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Adder {
            fn add(&self, y: i32) -> i32;
        }

        pub fn set_adder_rc(adder: Rc<dyn Adder>) -> i32 {
            adder.add(10)
        }

        pub fn set_adder_box(adder: Box<dyn Adder>) -> i32 {
            adder.add(100)
        }
    "#;

    case.js_pre = "
        class AdderImpl {
            constructor(x) {
                this.x = x
            }
            add(y) {
                return this.x + y
            }
        }
    ";

    case.swift_pre = "
        var counter = 0

        class AdderImpl : Adder {
            let x: Int32
            init(_ x: Int32) {
                self.x = x
                counter += 1
            }
            deinit {
                counter -= 1
            }
            func add(_ y: Int32) -> Int32 {
                return x + y
            }
        }
    ";

    case.cpp_pre = "
        int counter = 0;

        struct AdderImpl : rust::Adder {
            int32_t x;
            AdderImpl(int32_t x) {
                this->x = x;
                counter += 1;
            }
            ~AdderImpl() {
                counter -= 1;
            }
            int32_t add(int32_t y) {
                return x + y;
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            Call(
                Export("set_adder_rc").into(),
                [NewRc(TyName("Adder"), "AdderImpl", [I32(20)].into())].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("set_adder_box").into(),
                [NewBox(TyName("Adder"), "AdderImpl", [I32(200)].into())].into(),
            )
            .into(),
            I32(300).into(),
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
