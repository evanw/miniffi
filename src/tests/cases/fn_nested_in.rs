use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub struct Foo {
            #[allow(unused)] empty: (),
            ptr: Rc<dyn Bar>,
        }

        pub trait Bar {
            fn get(&self) -> i32;
        }

        pub fn test(x: (i32, Foo, Vec<(i32, Foo)>)) -> i32 {
            let mut total = x.0 + x.1.ptr.get();
            for y in &x.2 {
                total += y.0 + y.1.ptr.get();
            }
            total
        }
    "#;

    case.js_pre = "
        class BarImpl {
            get() {
                return 20
            }
        }
    ";

    case.swift_pre = "
        var counter = 0

        class BarImpl : Bar {
            init() {
                counter += 1
            }
            deinit {
                counter -= 1
            }
            func get() -> Int32 {
                return 20
            }
        }
    ";

    case.cpp_pre = "
        int counter = 0;

        struct BarImpl : rust::Bar {
            BarImpl() {
                counter += 1;
            }
            ~BarImpl() {
                counter -= 1;
            }
            int32_t get() {
                return 20;
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            Call(
                Export("test").into(),
                [Tuple(
                    [
                        I32(10),
                        Struct(
                            "Foo",
                            [
                                ("empty", Tuple([].into())),
                                ("ptr", NewRc(TyName("Bar"), "BarImpl", [].into())),
                            ]
                            .into(),
                        ),
                        EmptyVector(TyTuple([TyI32, TyName("Foo")].into()).into()),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("test").into(),
                [Tuple(
                    [
                        I32(1),
                        Struct(
                            "Foo",
                            [
                                ("empty", Tuple([].into())),
                                ("ptr", NewRc(TyName("Bar"), "BarImpl", [].into())),
                            ]
                            .into(),
                        ),
                        Vector(
                            [
                                Tuple(
                                    [
                                        I32(5),
                                        Struct(
                                            "Foo",
                                            [
                                                ("empty", Tuple([].into())),
                                                ("ptr", NewRc(TyName("Bar"), "BarImpl", [].into())),
                                            ]
                                            .into(),
                                        ),
                                    ]
                                    .into(),
                                ),
                                Tuple(
                                    [
                                        I32(600),
                                        Struct(
                                            "Foo",
                                            [
                                                ("empty", Tuple([].into())),
                                                ("ptr", NewRc(TyName("Bar"), "BarImpl", [].into())),
                                            ]
                                            .into(),
                                        ),
                                    ]
                                    .into(),
                                ),
                            ]
                            .into(),
                        ),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            I32(666).into(),
        ),
    ];

    case.swift_post = r#"
        assert(counter == 0, "\(counter) == \(0)")
    "#;

    case.cpp_post = "
        assert(counter == 0);
    ";

    case
}

test_all!();
