use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn empty_tuple(x: i32, #[allow(unused)] foo: (), y: i32) -> i32 {
            x + y
        }
        pub fn single_element_tuple(x: (i32,), y: (i32,)) -> i32 {
            x.0 + y.0
        }
        pub fn multiply_pairs(ab: (f32, f32), cd: (f32, f32)) -> f32 {
            let (a, b) = ab;
            let (c, d) = cd;
            a * c - b * d
        }
        pub fn nesting(x: (i32, (), (i32, (i32,)))) -> i32 {
            x.0 + x.2.0 + x.2.1.0
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(
                Export("empty_tuple").into(),
                [I32(10), Tuple([].into()), I32(20)].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("single_element_tuple").into(),
                [Tuple([I32(10)].into()), Tuple([I32(20)].into())].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("multiply_pairs").into(),
                [
                    Tuple([F32(2.0), F32(3.0)].into()),
                    Tuple([F32(5.0), F32(7.0)].into()),
                ]
                .into(),
            )
            .into(),
            F32(-11.0).into(),
        ),
        AssertEqual(
            Call(
                Export("nesting").into(),
                [Tuple(
                    [
                        I32(10),
                        Tuple([].into()),
                        Tuple([I32(20), Tuple([I32(30)].into())].into()),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            I32(60).into(),
        ),
    ];

    case
}

test_all!();
