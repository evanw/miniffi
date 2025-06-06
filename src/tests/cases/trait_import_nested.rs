use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Getter {
            fn get_adder(&self) -> Rc<dyn Adder>;
        }

        pub trait Adder {
            fn add(&self, x: i32, y: i32) -> i32;
        }

        pub fn set_getter(getter: Rc<dyn Getter>) -> i32 {
            getter.get_adder().add(1, 2)
        }
    "#;

    case.js_pre = "
        class GetterImpl {
            get_adder() {
                return new AdderImpl
            }
        }

        class AdderImpl {
            add(x, y) {
                return x + y
            }
        }
    ";

    case.swift_pre = "
        var getter_counter = 0

        class GetterImpl : Getter {
            init() {
                getter_counter += 1
            }
            deinit {
                getter_counter -= 1
            }
            func get_adder() -> Adder {
                return AdderImpl()
            }
        }

        var adder_counter = 0

        class AdderImpl : Adder {
            init() {
                adder_counter += 1
            }
            deinit {
                adder_counter -= 1
            }
            func add(_ x: Int32, _ y: Int32) -> Int32 {
                return x + y
            }
        }
    ";

    case.cpp_pre = "
        int adder_counter = 0;

        struct AdderImpl : rust::Adder {
            AdderImpl() {
                adder_counter += 1;
            }
            ~AdderImpl() {
                adder_counter -= 1;
            }
            int32_t add(int32_t x, int32_t y) {
                return x + y;
            }
        };

        int getter_counter = 0;

        struct GetterImpl : rust::Getter {
            GetterImpl() {
                getter_counter += 1;
            }
            ~GetterImpl() {
                getter_counter -= 1;
            }
            std::shared_ptr<rust::Adder> get_adder() {
                return std::make_shared<AdderImpl>();
            }
        };
    ";

    case.checks = vec![AssertEqual(
        Call(
            Export("set_getter").into(),
            [NewRc(TyName("Getter"), "GetterImpl", [].into())].into(),
        )
        .into(),
        I32(3).into(),
    )];

    case.swift_post = r#"
        assert(getter_counter == 0, "\(getter_counter) == \(0)")
        assert(adder_counter == 0, "\(adder_counter) == \(0)")
    "#;

    case.cpp_post = "
        assert(getter_counter == 0);
        assert(adder_counter == 0);
    ";

    case
}

test_all!();
