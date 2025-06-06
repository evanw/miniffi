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
    ];

    case
}

test_all!();
