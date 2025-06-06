use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(Debug)]
        pub struct Foo {
            pub x: (Bar, Vec<Bar>),
            pub y: Vec<((), (i32,), (f32, bool))>,
        }

        #[derive(Debug)]
        pub struct Bar {
            pub x: i32,
            pub y: Vec<Foo>,
        }

        pub fn check_combo(foo: Foo) -> String {
            format!("{foo:?}")
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

    case.checks.push(AssertEqual(
        Call(Export("check_combo").into(), [foo.clone()].into()).into(),
        Str("Foo { x: (Bar { x: 1, y: [] }, [\
            Bar { x: 2, y: [] }]), y: [((), (3,), (4.0, true))] }")
        .into(),
    ));

    foo = Struct(
        "Foo",
        [
            (
                "x",
                Tuple(
                    [
                        Struct(
                            "Bar",
                            [("x", I32(5)), ("y", Vector([foo.clone(), foo].into()))].into(),
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

    case.checks.push(AssertEqual(
        Call(Export("check_combo").into(), [foo].into()).into(),
        Str("Foo { x: (Bar { x: 5, y: [Foo { x: (Bar { x: 1, y: [] }, \
        [Bar { x: 2, y: [] }]), y: [((), (3,), (4.0, true))] }, \
        Foo { x: (Bar { x: 1, y: [] }, [Bar { x: 2, y: [] }]), y: [((), (3,), (4.0, true))] }] }, \
        [Bar { x: 6, y: [] }]), y: [((), (7,), (8.0, true))] }")
        .into(),
    ));

    case
}

test_all!();
