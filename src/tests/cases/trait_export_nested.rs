use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Getter {
            fn get_adder(&self) -> Rc<dyn Adder>;
        }

        struct GetterImpl;
        static mut GETTER_COUNTER: u32 = 0;

        pub fn get_getter_counter() -> u32 {
            unsafe { GETTER_COUNTER }
        }

        impl Getter for GetterImpl {
            fn get_adder(&self) -> Rc<dyn Adder> {
                unsafe { ADDER_COUNTER += 1; }
                Rc::new(AdderImpl)
            }
        }

        impl Drop for GetterImpl {
            fn drop(&mut self) {
                unsafe { GETTER_COUNTER -= 1; }
            }
        }

        pub trait Adder {
            fn add(&self, x: i32, y: i32) -> i32;
        }

        struct AdderImpl;
        static mut ADDER_COUNTER: u32 = 0;

        pub fn get_adder_counter() -> u32 {
            unsafe { ADDER_COUNTER }
        }

        impl Adder for AdderImpl {
            fn add(&self, x: i32, y: i32) -> i32 {
                x + y
            }
        }

        impl Drop for AdderImpl {
            fn drop(&mut self) {
                unsafe { ADDER_COUNTER -= 1; }
            }
        }

        pub fn get_getter() -> Rc<dyn Getter> {
            unsafe { GETTER_COUNTER += 1; }
            Rc::new(GetterImpl)
        }
    "#;

    case.checks = vec![
        AssertEqual(
            CallMethod(
                CallMethod(
                    Call(Export("get_getter").into(), [].into()).into(),
                    "get_adder",
                    [].into(),
                )
                .into(),
                "add",
                [I32(1), I32(2)].into(),
            )
            .into(),
            I32(3).into(),
        ),
        AfterGC(
            AssertEqual(
                Call(Export("get_getter_counter").into(), [].into()).into(),
                U32(0).into(),
            )
            .into(),
        ),
        AfterGC(
            AssertEqual(
                Call(Export("get_adder_counter").into(), [].into()).into(),
                U32(0).into(),
            )
            .into(),
        ),
    ];

    case
}

test_all!();
