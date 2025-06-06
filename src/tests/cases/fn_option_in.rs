use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn add_option(x: Option<i32>, y: Option<i32>) -> i32 {
            x.unwrap_or(0) + y.unwrap_or(0)
        }

        pub fn add_nested(x: Option<Option<i32>>, y: Option<Option<i32>>) -> i32 {
            x.unwrap_or(None).unwrap_or(0) + y.unwrap_or(None).unwrap_or(0)
        }

        pub fn add_all(x: Vec<Option<i32>>) -> i32 {
            x.iter().map(|x| x.unwrap_or(0)).sum()
        }

        pub fn join_all(x: Vec<Option<String>>) -> String {
            x.iter().map(|x| x.as_ref().map(String::as_str).unwrap_or("")).collect::<Vec<_>>().join("")
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(
                Export("add_option").into(),
                [OptNone(TyI32), OptNone(TyI32)].into(),
            )
            .into(),
            I32(0).into(),
        ),
        AssertEqual(
            Call(
                Export("add_option").into(),
                [OptSome(I32(1).into()), OptNone(TyI32)].into(),
            )
            .into(),
            I32(1).into(),
        ),
        AssertEqual(
            Call(
                Export("add_option").into(),
                [OptNone(TyI32), OptSome(I32(2).into())].into(),
            )
            .into(),
            I32(2).into(),
        ),
        AssertEqual(
            Call(
                Export("add_option").into(),
                [OptSome(I32(1).into()), OptSome(I32(2).into())].into(),
            )
            .into(),
            I32(3).into(),
        ),
        AssertEqual(
            Call(
                Export("add_all").into(),
                [Vector(
                    [
                        OptNone(TyI32),
                        OptSome(I32(100).into()),
                        OptNone(TyI32),
                        OptSome(I32(20).into()),
                        OptNone(TyI32),
                        OptSome(I32(3).into()),
                        OptNone(TyI32),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            I32(123).into(),
        ),
        AssertEqual(
            Call(
                Export("add_nested").into(),
                [
                    OptNone(TyOption(TyI32.into())),
                    OptNone(TyOption(TyI32.into())),
                ]
                .into(),
            )
            .into(),
            I32(0).into(),
        ),
        AssertEqual(
            Call(
                Export("add_nested").into(),
                [
                    OptSome(OptSome(I32(-1).into()).into()),
                    OptSome(OptNone(TyI32).into()),
                ]
                .into(),
            )
            .into(),
            I32(-1).into(),
        ),
        AssertEqual(
            Call(
                Export("add_nested").into(),
                [
                    OptSome(OptNone(TyI32).into()),
                    OptSome(OptSome(I32(-2).into()).into()),
                ]
                .into(),
            )
            .into(),
            I32(-2).into(),
        ),
        AssertEqual(
            Call(
                Export("add_nested").into(),
                [
                    OptSome(OptSome(I32(-1).into()).into()),
                    OptSome(OptSome(I32(-2).into()).into()),
                ]
                .into(),
            )
            .into(),
            I32(-3).into(),
        ),
        AssertEqual(
            Call(
                Export("join_all").into(),
                [Vector(
                    [
                        OptNone(TyString),
                        OptSome(Str("abc").into()),
                        OptNone(TyString),
                        OptSome(Str("xy").into()),
                        OptNone(TyString),
                        OptSome(Str("z").into()),
                        OptNone(TyString),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Str("abcxyz").into(),
        ),
    ];

    case
}

test_all!();
