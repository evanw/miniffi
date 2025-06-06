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

        pub fn get_tree() -> Tree {
            Tree {
                value: 2,
                left: Some(
                    Tree {
                        value: 1,
                        left: None,
                        right: None,
                    }
                    .into(),
                ),
                right: Some(
                    Tree {
                        value: 4,
                        left: Some(
                            Tree {
                                value: 3,
                                left: None,
                                right: None,
                            }
                            .into(),
                        ),
                        right: Some(
                            Tree {
                                value: 5,
                                left: None,
                                right: None,
                            }
                            .into(),
                        ),
                    }
                    .into(),
                ),
            }
        }

        #[derive(Eq, PartialEq)]
        pub struct Nested(Box<Box<Box<i32>>>);

        pub fn check_nested(x: i32) -> Nested {
            Nested(Box::new(Box::new(Box::new(x))))
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("get_tree").into(), [].into()).into(),
            Struct(
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
            .into(),
        )
        .into(),
        AssertEqual(
            Call(Export("check_nested").into(), [I32(123).into()].into()).into(),
            Struct(
                "Nested",
                [(
                    "0",
                    Boxed(
                        TyBox(TyBox(TyI32.into()).into()),
                        Boxed(TyBox(TyI32.into()), Boxed(TyI32, I32(123).into()).into()).into(),
                    )
                    .into(),
                )]
                .into(),
            )
            .into(),
        )
        .into(),
    ];

    case
}

test_all!();
