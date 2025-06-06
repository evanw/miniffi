use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(PartialEq)]
        pub struct EmptyStruct;

        #[derive(PartialEq)]
        pub struct SingleElementStruct(i32);

        #[derive(PartialEq)]
        pub struct PairStruct { x: f32, y: f32 }

        pub fn empty_struct() -> EmptyStruct {
            EmptyStruct
        }

        pub fn single_element_struct(x: i32) -> SingleElementStruct {
            SingleElementStruct(x)
        }

        pub fn make_pair(x: f32, y: f32) -> PairStruct {
            PairStruct { x, y }
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("empty_struct").into(), [].into()).into(),
            Struct("EmptyStruct", [].into()).into(),
        ),
        AssertEqual(
            Call(Export("single_element_struct").into(), [I32(10)].into()).into(),
            Struct("SingleElementStruct", [("0", I32(10))].into()).into(),
        ),
        AssertEqual(
            Call(Export("make_pair").into(), [F32(2.0), F32(3.0)].into()).into(),
            Struct("PairStruct", [("x", F32(2.0)), ("y", F32(3.0))].into()).into(),
        ),
    ];

    case
}

test_all!();
