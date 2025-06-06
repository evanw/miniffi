use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Adder {
            fn add(&self, x: i32) -> i32;
        }

        struct AdderImpl(i32);
        static mut COUNTER: u32 = 0;

        pub fn get_counter() -> u32 {
            unsafe { COUNTER }
        }

        impl Adder for AdderImpl {
            fn add(&self, x: i32) -> i32 {
                self.0 + x
            }
        }

        impl Drop for AdderImpl {
            fn drop(&mut self) {
                unsafe { COUNTER -= 1; }
            }
        }

        pub fn get_adder_rc(x: i32) -> Rc<dyn Adder> {
            unsafe { COUNTER += 1; }
            Rc::new(AdderImpl(x))
        }

        pub fn get_adder_box(x: i32) -> Box<dyn Adder> {
            unsafe { COUNTER += 1; }
            Box::new(AdderImpl(x))
        }
    "#;

    case.checks = vec![
        AssertEqual(
            CallMethod(
                Call(Export("get_adder_rc").into(), [I32(20)].into()).into(),
                "add",
                [I32(10)].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AfterGC(
            AssertEqual(
                Call(Export("get_counter").into(), [].into()).into(),
                U32(0).into(),
            )
            .into(),
        ),
        AssertEqual(
            CallMethod(
                Call(Export("get_adder_box").into(), [I32(20)].into()).into(),
                "add",
                [I32(10)].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AfterGC(
            AssertEqual(
                Call(Export("get_counter").into(), [].into()).into(),
                U32(0).into(),
            )
            .into(),
        ),
    ];

    case
}

test_all!();
