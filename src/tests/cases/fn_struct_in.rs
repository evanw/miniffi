use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub struct EmptyStruct;
        pub struct SingleElementStruct(i32);
        pub struct PairStruct { x: f32, y: f32 }

        pub fn empty_tuple(x: i32, #[allow(unused)] foo: EmptyStruct, y: i32) -> i32 {
            x + y
        }

        pub fn single_element_tuple(x: SingleElementStruct, y: SingleElementStruct) -> i32 {
            x.0 + y.0
        }

        pub fn multiply_pairs(ab: PairStruct, cd: PairStruct) -> f32 {
            let PairStruct { x: a, y: b } = ab;
            let PairStruct { x: c, y: d } = cd;
            a * c - b * d
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(
                Export("empty_tuple").into(),
                [I32(10), Struct("EmptyStruct", [].into()), I32(20)].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("single_element_tuple").into(),
                [
                    Struct("SingleElementStruct", [("0", I32(10))].into()),
                    Struct("SingleElementStruct", [("0", I32(20))].into()),
                ]
                .into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("multiply_pairs").into(),
                [
                    Struct("PairStruct", [("x", F32(2.0)), ("y", F32(3.0))].into()),
                    Struct("PairStruct", [("x", F32(5.0)), ("y", F32(7.0))].into()),
                ]
                .into(),
            )
            .into(),
            F32(-11.0).into(),
        ),
    ];

    case
}

test_all!();
