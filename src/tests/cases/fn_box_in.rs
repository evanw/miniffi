use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(Eq, PartialEq)]
        pub struct Tree {
            value: i32,
            left: Option<Box<Tree>>,
            right: Option<Box<Tree>>,
        }

        pub fn sum_tree(tree: Tree) -> i32 {
            let mut sum = tree.value;
            sum += tree.left.map(|x| sum_tree(*x)).unwrap_or(0);
            sum += tree.right.map(|x| sum_tree(*x)).unwrap_or(0);
            sum
        }

        pub fn check_nested(x: Box<Box<Box<i32>>>) -> i32 {
            ***x
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(
                Export("sum_tree").into(),
                [Struct(
                    "Tree",
                    [
                        ("value", I32(2)),
                        (
                            "left",
                            OptSome(
                                Boxed(
                                    TyName("Tree"),
                                    Struct(
                                        "Tree",
                                        [
                                            ("value", I32(1)),
                                            ("left", OptNone(TyBox(TyName("Tree").into()))),
                                            ("right", OptNone(TyBox(TyName("Tree").into()))),
                                        ]
                                        .into(),
                                    )
                                    .into(),
                                )
                                .into(),
                            ),
                        ),
                        (
                            "right",
                            OptSome(
                                Boxed(
                                    TyName("Tree"),
                                    Struct(
                                        "Tree",
                                        [
                                            ("value", I32(4)),
                                            (
                                                "left",
                                                OptSome(
                                                    Boxed(
                                                        TyName("Tree"),
                                                        Struct(
                                                            "Tree",
                                                            [
                                                                ("value", I32(3)),
                                                                (
                                                                    "left",
                                                                    OptNone(TyBox(
                                                                        TyName("Tree").into(),
                                                                    )),
                                                                ),
                                                                (
                                                                    "right",
                                                                    OptNone(TyBox(
                                                                        TyName("Tree").into(),
                                                                    )),
                                                                ),
                                                            ]
                                                            .into(),
                                                        )
                                                        .into(),
                                                    )
                                                    .into(),
                                                ),
                                            ),
                                            (
                                                "right",
                                                OptSome(
                                                    Boxed(
                                                        TyName("Tree"),
                                                        Struct(
                                                            "Tree",
                                                            [
                                                                ("value", I32(5)),
                                                                (
                                                                    "left",
                                                                    OptNone(TyBox(
                                                                        TyName("Tree").into(),
                                                                    )),
                                                                ),
                                                                (
                                                                    "right",
                                                                    OptNone(TyBox(
                                                                        TyName("Tree").into(),
                                                                    )),
                                                                ),
                                                            ]
                                                            .into(),
                                                        )
                                                        .into(),
                                                    )
                                                    .into(),
                                                ),
                                            ),
                                        ]
                                        .into(),
                                    )
                                    .into(),
                                )
                                .into(),
                            ),
                        ),
                    ]
                    .into(),
                )
                .into()]
                .into(),
            )
            .into(),
            I32(1 + 2 + 3 + 4 + 5).into(),
        )
        .into(),
        AssertEqual(
            Call(
                Export("check_nested").into(),
                [Boxed(
                    TyBox(TyBox(TyI32.into()).into()),
                    Boxed(TyBox(TyI32.into()), Boxed(TyI32, I32(123).into()).into()).into(),
                )
                .into()]
                .into(),
            )
            .into(),
            I32(123).into(),
        )
        .into(),
    ];

    case
}

test_all!();
