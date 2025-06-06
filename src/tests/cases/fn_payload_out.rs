use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(Eq, PartialEq)]
        pub enum Foo {
            Empty,
            Single(i32),
            Point { x: i32, y: i32 },
            Nested(Box<Foo>),
        }

        pub fn get_tests() -> Vec<Foo> {
            vec![
                Foo::Empty,
                Foo::Single(123),
                Foo::Point { x: 1, y: -2 },
                Foo::Nested(Box::new(Foo::Nested(Box::new(Foo::Single(-321))))),
            ]
        }
    "#;

    case.checks = vec![AssertEqual(
        Call(Export("get_tests").into(), [].into()).into(),
        Vector(
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
        .into(),
    )];

    case
}

test_all!();
