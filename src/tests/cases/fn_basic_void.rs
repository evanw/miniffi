use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        static mut RESULT: i32 = 0;

        pub fn add_void(x: i32, y: i32) {
            unsafe { RESULT = x + y; }
        }

        pub fn add_empty_tuple(x: i32, y: i32) -> () {
            unsafe { RESULT = x + y; }
        }

        pub fn get_result() -> i32 {
            unsafe { RESULT }
        }

        // Make sure the "_" wildcard pattern works as an argument
        pub fn wild_arg(_: i32, _: (), _: i32) -> () {
            unsafe { RESULT = 123; }
        }
    "#;

    case.checks = vec![
        Call(Export("add_void").into(), [I32(1), I32(2)].into()),
        AssertEqual(
            Call(Export("get_result").into(), [].into()).into(),
            I32(3).into(),
        ),
        Call(Export("add_empty_tuple").into(), [I32(3), I32(4)].into()),
        AssertEqual(
            Call(Export("get_result").into(), [].into()).into(),
            I32(7).into(),
        ),
        Call(
            Export("wild_arg").into(),
            [I32(-1), Tuple([].into()), I32(-2)].into(),
        ),
        AssertEqual(
            Call(Export("get_result").into(), [].into()).into(),
            I32(123).into(),
        ),
    ];

    case
}

test_all!();
