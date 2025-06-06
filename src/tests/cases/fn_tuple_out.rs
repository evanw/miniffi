use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn single_element_tuple(x: i32) -> (i32,) {
            (-x,)
        }
        pub fn return_pair(x: f32, y: bool) -> (f32, bool) {
            (-x, !y)
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("single_element_tuple").into(), [I32(123)].into()).into(),
            Tuple([I32(-123)].into()).into(),
        ),
        AssertEqual(
            Call(Export("return_pair").into(), [F32(1.23), Bool(true)].into()).into(),
            Tuple([F32(-1.23), Bool(false)].into()).into(),
        ),
    ];

    case
}

test_all!();
