use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait Trait {
            fn get(&self) -> i32;
        }

        pub struct Example {
            box_ptr: Box<dyn Trait>,
            rc_ptr: Rc<dyn Trait>,
            text: String,
            vec_box: Vec<Box<dyn Trait>>,
            vec_rc: Vec<Rc<dyn Trait>>,
        }

        pub fn test(vec: Vec<Example>) -> Vec<Example> {
            vec
        }
    "#;

    case.js_pre = "
        class TraitImpl {
            constructor(x) {
                this.x = x
            }
            get() {
                return this.x
            }
        }
    ";

    case.swift_pre = "
        var counter = 0

        class TraitImpl : Trait {
            let x: Int32
            init(_ x: Int32) {
                self.x = x
                counter += 1
            }
            deinit {
                counter -= 1
            }
            func get() -> Int32 {
                return x
            }
        }
    ";

    case.cpp_pre = "
        int counter = 0;

        struct TraitImpl : rust::Trait {
            int32_t x;
            TraitImpl(int32_t x) {
                this->x = x;
                counter += 1;
            }
            ~TraitImpl() {
                counter -= 1;
            }
            int32_t get() {
                return x;
            }
        };
    ";

    case.checks = vec![
        DeclareImmutableLocal(
            "vec",
            Call(
                Export("test").into(),
                [Vector(
                    [Struct(
                        "Example",
                        [
                            (
                                "box_ptr",
                                NewBox(TyName("Trait"), "TraitImpl", [I32(1)].into()).into(),
                            ),
                            (
                                "rc_ptr",
                                NewRc(TyName("Trait"), "TraitImpl", [I32(4)].into()).into(),
                            ),
                            ("text", Str("abc".into())),
                            (
                                "vec_box",
                                Vector(
                                    [
                                        NewBox(TyName("Trait"), "TraitImpl", [I32(2)].into())
                                            .into(),
                                        NewBox(TyName("Trait"), "TraitImpl", [I32(3)].into())
                                            .into(),
                                    ]
                                    .into(),
                                ),
                            ),
                            (
                                "vec_rc",
                                Vector(
                                    [
                                        NewRc(TyName("Trait"), "TraitImpl", [I32(5)].into()).into(),
                                        NewRc(TyName("Trait"), "TraitImpl", [I32(6)].into()).into(),
                                    ]
                                    .into(),
                                ),
                            ),
                        ]
                        .into(),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
        ),
        AssertEqual(
            CallMethod(
                StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "box_ptr").into(),
                "get",
                [].into(),
            )
            .into(),
            I32(1).into(),
        ),
        AssertEqual(
            CallMethod(
                StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "rc_ptr").into(),
                "get",
                [].into(),
            )
            .into(),
            I32(4).into(),
        ),
        AssertEqual(
            StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "text").into(),
            Str("abc".into()).into(),
        ),
        AssertEqual(
            VectorLen(
                StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "vec_box").into(),
            )
            .into(),
            I32(2).into(),
        ),
        AssertEqual(
            CallMethod(
                VectorMember(
                    StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "vec_box").into(),
                    0,
                )
                .into(),
                "get",
                [].into(),
            )
            .into(),
            I32(2).into(),
        ),
        AssertEqual(
            CallMethod(
                VectorMember(
                    StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "vec_box").into(),
                    1,
                )
                .into(),
                "get",
                [].into(),
            )
            .into(),
            I32(3).into(),
        ),
        AssertEqual(
            VectorLen(
                StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "vec_rc").into(),
            )
            .into(),
            I32(2).into(),
        ),
        AssertEqual(
            CallMethod(
                VectorMember(
                    StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "vec_rc").into(),
                    0,
                )
                .into(),
                "get",
                [].into(),
            )
            .into(),
            I32(5).into(),
        ),
        AssertEqual(
            CallMethod(
                VectorMember(
                    StructMember(VectorMember(LoadLocal("vec").into(), 0).into(), "vec_rc").into(),
                    1,
                )
                .into(),
                "get",
                [].into(),
            )
            .into(),
            I32(6).into(),
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
