use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(PartialEq)]
        pub struct Foo {
            // Note: One purpose of this test is to cover our manual "Equatable"
            // implementation in Swift, since tuples aren't equatable in Swift.
            pub x: (Bar, Vec<Bar>),
            pub y: Vec<((), (i32,), (f32, bool))>,
        }

        #[derive(PartialEq)]
        pub struct Bar {
            pub x: i32,
            pub y: Vec<Foo>,
        }

        pub fn check_combo1() -> Foo {
            Foo {
                x: (
                    Bar { x: 1, y: vec![] },
                    vec![Bar { x: 2, y: vec![] }],
                ),
                y: vec![((), (3,), (4.0, true))],
            }
        }

        pub fn check_combo2() -> Foo {
            Foo {
                x: (
                    Bar { x: 5, y: vec![check_combo1(), check_combo1()] },
                    vec![Bar { x: 6, y: vec![] }],
                ),
                y: vec![((), (7,), (8.0, true))],
            }
        }
        "#;

    let mut foo = Struct(
        "Foo",
        [
            (
                "x",
                Tuple(
                    [
                        Struct(
                            "Bar",
                            [("x", I32(1)), ("y", EmptyVector(TyName("Foo")))].into(),
                        ),
                        Vector(
                            [Struct(
                                "Bar",
                                [("x", I32(2)), ("y", EmptyVector(TyName("Foo")))].into(),
                            )]
                            .into(),
                        ),
                    ]
                    .into(),
                ),
            ),
            (
                "y",
                Vector(
                    [Tuple(
                        [
                            Tuple([].into()),
                            Tuple([I32(3)].into()),
                            Tuple([F32(4.0), Bool(true)].into()),
                        ]
                        .into(),
                    )]
                    .into(),
                ),
            ),
        ]
        .into(),
    );

    case.checks = vec![AssertEqual(
        Call(Export("check_combo1").into(), [].into()).into(),
        foo.clone().into(),
    )];

    foo = Struct(
        "Foo",
        [
            (
                "x",
                Tuple(
                    [
                        Struct(
                            "Bar",
                            [
                                ("x", I32(5)),
                                ("y", Vector([foo.clone(), foo.clone()].into())),
                            ]
                            .into(),
                        ),
                        Vector(
                            [Struct(
                                "Bar",
                                [("x", I32(6)), ("y", EmptyVector(TyName("Foo")))].into(),
                            )]
                            .into(),
                        ),
                    ]
                    .into(),
                ),
            ),
            (
                "y",
                Vector(
                    [Tuple(
                        [
                            Tuple([].into()),
                            Tuple([I32(7)].into()),
                            Tuple([F32(8.0), Bool(true)].into()),
                        ]
                        .into(),
                    )]
                    .into(),
                ),
            ),
        ]
        .into(),
    );

    case.checks = vec![AssertEqual(
        Call(Export("check_combo2").into(), [].into()).into(),
        foo.into(),
    )];

    case
}

test_all!();
