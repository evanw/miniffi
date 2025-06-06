use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn opt_int(x: bool, y: i32) -> Option<i32> {
            x.then(|| y)
        }

        pub fn opt_opt_int(x: bool, y: bool, z: i32) -> Option<Option<i32>> {
            x.then(|| y.then(|| z))
        }

        pub fn vec_opt_int(n: i32) -> Vec<Option<i32>> {
            (0..n).map(|x| (x % 3 != 0 && x % 5 != 0).then_some(x)).collect()
        }

        pub fn opt_vec_opt_string(n: i32) -> Option<Vec<Option<String>>> {
            if n < 1 {
                None
            } else {
                Some((0..n).map(|x| (x % 3 != 0 && x % 5 != 0).then_some(x.to_string())).collect())
            }
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("opt_int").into(), [Bool(false), I32(10)].into()).into(),
            OptNone(TyI32.into()).into(),
        ),
        AssertEqual(
            Call(Export("opt_int").into(), [Bool(true), I32(10)].into()).into(),
            OptSome(I32(10).into()).into(),
        ),
        AssertEqual(
            Call(
                Export("opt_opt_int").into(),
                [Bool(false), Bool(false), I32(30)].into(),
            )
            .into(),
            OptNone(TyOption(TyI32.into())).into(),
        ),
        AssertEqual(
            Call(
                Export("opt_opt_int").into(),
                [Bool(true), Bool(false), I32(30)].into(),
            )
            .into(),
            OptSome(OptNone(TyI32.into()).into()).into(),
        ),
        AssertEqual(
            Call(
                Export("opt_opt_int").into(),
                [Bool(true), Bool(true), I32(30)].into(),
            )
            .into(),
            OptSome(OptSome(I32(30).into()).into()).into(),
        ),
        AssertEqual(
            Call(Export("vec_opt_int").into(), [I32(10)].into()).into(),
            Vector(
                [
                    OptNone(TyI32.into()),
                    OptSome(I32(1).into()),
                    OptSome(I32(2).into()),
                    OptNone(TyI32.into()),
                    OptSome(I32(4).into()),
                    OptNone(TyI32.into()),
                    OptNone(TyI32.into()),
                    OptSome(I32(7).into()),
                    OptSome(I32(8).into()),
                    OptNone(TyI32.into()),
                ]
                .into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(Export("opt_vec_opt_string").into(), [I32(10)].into()).into(),
            OptSome(
                Vector(
                    [
                        OptNone(TyString.into()),
                        OptSome(Str("1").into()),
                        OptSome(Str("2").into()),
                        OptNone(TyString.into()),
                        OptSome(Str("4").into()),
                        OptNone(TyString.into()),
                        OptNone(TyString.into()),
                        OptSome(Str("7").into()),
                        OptSome(Str("8").into()),
                        OptNone(TyString.into()),
                    ]
                    .into(),
                )
                .into(),
            )
            .into(),
        ),
    ];

    case
}

test_all!();
