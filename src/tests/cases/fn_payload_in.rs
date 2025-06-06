use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(Debug, Eq, PartialEq)]
        pub enum Foo {
            Empty,
            Single(i32),
            Point { x: i32, y: i32 },
            Nested(Box<Foo>),
        }

        pub fn set_tests(tests: Vec<Foo>) -> bool {
            assert_eq!(tests, vec![
                Foo::Empty,
                Foo::Single(123),
                Foo::Point { x: 1, y: -2 },
                Foo::Nested(Box::new(Foo::Nested(Box::new(Foo::Single(-321))))),
            ]);
            true
        }
    "#;

    case.checks = vec![AssertEqual(
        Call(
            Export("set_tests").into(),
            [Vector(
                [
                    EnumPayload("Foo", "Empty", [].into()),
                    EnumPayload("Foo", "Single", [("0", I32(123))].into()),
                    EnumPayload("Foo", "Point", [("x", I32(1)), ("y", I32(-2))].into()),
                    EnumPayload(
                        "Foo",
                        "Nested",
                        [(
                            "0",
                            Boxed(
                                TyName("Foo"),
                                EnumPayload(
                                    "Foo",
                                    "Nested",
                                    [(
                                        "0",
                                        Boxed(
                                            TyName("Foo"),
                                            EnumPayload("Foo", "Single", [("0", I32(-321))].into())
                                                .into(),
                                        )
                                        .into(),
                                    )]
                                    .into(),
                                )
                                .into(),
                            ),
                        )]
                        .into(),
                    ),
                ]
                .into(),
            )
            .into()]
            .into(),
        )
        .into(),
        Bool(true).into(),
    )];

    case
}

test_all!();
