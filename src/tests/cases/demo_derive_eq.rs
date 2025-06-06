use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        #[derive(Eq, PartialEq)] pub struct EmptyStruct;
        pub fn empty_tuple(x: ()) -> () { x }
        pub fn empty_struct(x: EmptyStruct) -> EmptyStruct { x }

        ////////////////////////////////////////
        // Swift has trouble with tuple equality

        #[derive(Eq, PartialEq)] pub struct BoxTup0(Box<()>);
        #[derive(Eq, PartialEq)] pub struct BoxTup1(Box<(i32,)>);
        #[derive(Eq, PartialEq)] pub struct BoxTup2(Box<(i32, i32)>);

        pub fn box_tup_0(x: BoxTup0) -> BoxTup0 { x }
        pub fn box_tup_1(x: BoxTup1) -> BoxTup1 { x }
        pub fn box_tup_2(x: BoxTup2) -> BoxTup2 { x }

        #[derive(Eq, PartialEq)] pub struct VecTup0(Vec<()>);
        #[derive(Eq, PartialEq)] pub struct VecTup1(Vec<(i32,)>);
        #[derive(Eq, PartialEq)] pub struct VecTup2(Vec<(i32, i32)>);

        pub fn vec_tup_0(x: VecTup0) -> VecTup0 { x }
        pub fn vec_tup_1(x: VecTup1) -> VecTup1 { x }
        pub fn vec_tup_2(x: VecTup2) -> VecTup2 { x }

        #[derive(Eq, PartialEq)] pub struct OptTup0(Option<()>);
        #[derive(Eq, PartialEq)] pub struct OptTup1(Option<(i32,)>);
        #[derive(Eq, PartialEq)] pub struct OptTup2(Option<(i32, i32)>);

        pub fn opt_tup_0(x: OptTup0) -> OptTup0 { x }
        pub fn opt_tup_1(x: OptTup1) -> OptTup1 { x }
        pub fn opt_tup_2(x: OptTup2) -> OptTup2 { x }

        #[derive(Eq, PartialEq)] pub enum EnumBoxTup { Foo(Box<(i32, i32)>), Bar, Baz { x: i32, y: i32 } }
        #[derive(Eq, PartialEq)] pub enum EnumVecTup { Foo(Vec<(i32, i32)>), Bar, Baz { x: i32, y: i32 } }
        #[derive(Eq, PartialEq)] pub enum EnumOptTup { Foo(Option<(i32, i32)>), Bar, Baz { x: i32, y: i32 } }

        pub fn enum_box_tup(x: EnumBoxTup) -> EnumBoxTup { x }
        pub fn enum_vec_tup(x: EnumVecTup) -> EnumVecTup { x }
        pub fn enum_opt_tup(x: EnumOptTup) -> EnumOptTup { x }

        ////////////////////////////////////////
        // C++ has trouble with box equality

        #[derive(Eq, PartialEq)] pub struct TupBox((Box<i32>, Box<bool>));

        #[derive(Eq, PartialEq)] pub struct VecBox(Vec<Box<i32>>);
        #[derive(Eq, PartialEq)] pub struct BoxVec(Box<Vec<i32>>);
        #[derive(Eq, PartialEq)] pub struct OptBox(Option<Box<i32>>);
        #[derive(Eq, PartialEq)] pub struct BoxOpt(Box<Option<i32>>);

        #[derive(Eq, PartialEq)] pub struct VecBoxVec(Vec<Box<Vec<i32>>>);
        #[derive(Eq, PartialEq)] pub struct BoxVecBox(Box<Vec<Box<i32>>>);
        #[derive(Eq, PartialEq)] pub struct OptBoxOpt(Option<Box<Option<i32>>>);
        #[derive(Eq, PartialEq)] pub struct BoxOptBox(Box<Option<Box<i32>>>);

        pub fn tup_box(x: TupBox) -> TupBox { x }

        pub fn vec_box(x: VecBox) -> VecBox { x }
        pub fn box_vec(x: BoxVec) -> BoxVec { x }
        pub fn opt_box(x: OptBox) -> OptBox { x }
        pub fn box_opt(x: BoxOpt) -> BoxOpt { x }

        pub fn vec_box_vec(x: VecBoxVec) -> VecBoxVec { x }
        pub fn box_vec_box(x: BoxVecBox) -> BoxVecBox { x }
        pub fn opt_box_opt(x: OptBoxOpt) -> OptBoxOpt { x }
        pub fn box_opt_box(x: BoxOptBox) -> BoxOptBox { x }
    "#;

    // Empty tuple, EmptyStruct
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("empty_tuple").into(),
                [Tuple([].into()).into()].into(),
            )
            .into(),
            Tuple([].into()).into(),
        ),
        AssertEqual(
            Call(
                Export("empty_struct").into(),
                [Struct("EmptyStruct", [].into()).into()].into(),
            )
            .into(),
            Struct("EmptyStruct", [].into()).into(),
        ),
    ]);

    // BoxTup0
    case.checks.extend_from_slice(&[AssertEqual(
        Call(
            Export("box_tup_0").into(),
            [Struct(
                "BoxTup0",
                [("0", Boxed(TyTuple([].into()), Tuple([].into()).into()))].into(),
            )]
            .into(),
        )
        .into(),
        Struct(
            "BoxTup0",
            [("0", Boxed(TyTuple([].into()), Tuple([].into()).into()))].into(),
        )
        .into(),
    )]);

    // BoxTup1
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("box_tup_1").into(),
                [Struct(
                    "BoxTup1",
                    [(
                        "0",
                        Boxed(TyTuple([TyI32].into()), Tuple([I32(10)].into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxTup1",
                [(
                    "0",
                    Boxed(TyTuple([TyI32].into()), Tuple([I32(10)].into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_tup_1").into(),
                [Struct(
                    "BoxTup1",
                    [(
                        "0",
                        Boxed(TyTuple([TyI32].into()), Tuple([I32(10)].into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxTup1",
                [(
                    "0",
                    Boxed(TyTuple([TyI32].into()), Tuple([I32(20)].into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    // BoxTup2
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("box_tup_2").into(),
                [Struct(
                    "BoxTup2",
                    [(
                        "0",
                        Boxed(
                            TyTuple([TyI32, TyI32].into()),
                            Tuple([I32(20), I32(30)].into()).into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxTup2",
                [(
                    "0",
                    Boxed(
                        TyTuple([TyI32, TyI32].into()),
                        Tuple([I32(20), I32(30)].into()).into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_tup_2").into(),
                [Struct(
                    "BoxTup2",
                    [(
                        "0",
                        Boxed(
                            TyTuple([TyI32, TyI32].into()),
                            Tuple([I32(20), I32(30)].into()).into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxTup2",
                [(
                    "0",
                    Boxed(
                        TyTuple([TyI32, TyI32].into()),
                        Tuple([I32(30), I32(20)].into()).into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    // VecTup0
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("vec_tup_0").into(),
                [Struct(
                    "VecTup0",
                    [("0", EmptyVector(TyTuple([].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("VecTup0", [("0", EmptyVector(TyTuple([].into())))].into()).into(),
        ),
        AssertEqual(
            Call(
                Export("vec_tup_0").into(),
                [Struct(
                    "VecTup0",
                    [("0", Vector([Tuple([].into()), Tuple([].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup0",
                [("0", Vector([Tuple([].into()), Tuple([].into())].into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_0").into(),
                [Struct(
                    "VecTup0",
                    [("0", Vector([Tuple([].into()), Tuple([].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("VecTup0", [("0", Vector([Tuple([].into())].into()))].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_0").into(),
                [Struct(
                    "VecTup0",
                    [("0", Vector([Tuple([].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup0",
                [("0", Vector([Tuple([].into()), Tuple([].into())].into()))].into(),
            )
            .into(),
        ),
    ]);

    // VecTup1
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("vec_tup_1").into(),
                [Struct(
                    "VecTup1",
                    [("0", EmptyVector(TyTuple([TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup1",
                [("0", EmptyVector(TyTuple([TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("vec_tup_1").into(),
                [Struct(
                    "VecTup1",
                    [(
                        "0",
                        Vector([Tuple([I32(10)].into()), Tuple([I32(20)].into())].into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup1",
                [(
                    "0",
                    Vector([Tuple([I32(10)].into()), Tuple([I32(20)].into())].into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_1").into(),
                [Struct(
                    "VecTup1",
                    [(
                        "0",
                        Vector([Tuple([I32(10)].into()), Tuple([I32(20)].into())].into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup1",
                [(
                    "0",
                    Vector([Tuple([I32(20)].into()), Tuple([I32(10)].into())].into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_1").into(),
                [Struct(
                    "VecTup1",
                    [(
                        "0",
                        Vector([Tuple([I32(1)].into()), Tuple([I32(1)].into())].into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup1",
                [("0", Vector([Tuple([I32(1)].into())].into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_1").into(),
                [Struct(
                    "VecTup1",
                    [("0", Vector([Tuple([I32(1)].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup1",
                [(
                    "0",
                    Vector([Tuple([I32(1)].into()), Tuple([I32(1)].into())].into()),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    // VecTup2
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("vec_tup_2").into(),
                [Struct(
                    "VecTup2",
                    [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup2",
                [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("vec_tup_2").into(),
                [Struct(
                    "VecTup2",
                    [(
                        "0",
                        Vector(
                            [
                                Tuple([I32(1), I32(2)].into()),
                                Tuple([I32(3), I32(4)].into()),
                            ]
                            .into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup2",
                [(
                    "0",
                    Vector(
                        [
                            Tuple([I32(1), I32(2)].into()),
                            Tuple([I32(3), I32(4)].into()),
                        ]
                        .into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_2").into(),
                [Struct(
                    "VecTup2",
                    [(
                        "0",
                        Vector(
                            [
                                Tuple([I32(1), I32(2)].into()),
                                Tuple([I32(3), I32(4)].into()),
                            ]
                            .into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup2",
                [(
                    "0",
                    Vector(
                        [
                            Tuple([I32(1), I32(4)].into()),
                            Tuple([I32(3), I32(2)].into()),
                        ]
                        .into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_2").into(),
                [Struct(
                    "VecTup2",
                    [(
                        "0",
                        Vector(
                            [
                                Tuple([I32(1), I32(1)].into()),
                                Tuple([I32(1), I32(1)].into()),
                            ]
                            .into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup2",
                [("0", Vector([Tuple([I32(1), I32(1)].into())].into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_tup_2").into(),
                [Struct(
                    "VecTup2",
                    [("0", Vector([Tuple([I32(1), I32(1)].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecTup2",
                [(
                    "0",
                    Vector(
                        [
                            Tuple([I32(1), I32(1)].into()),
                            Tuple([I32(1), I32(1)].into()),
                        ]
                        .into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    // OptTup0
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("opt_tup_0").into(),
                [Struct(
                    "OptTup0",
                    [("0", OptNone(TyTuple([].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptTup0", [("0", OptNone(TyTuple([].into())))].into()).into(),
        ),
        AssertEqual(
            Call(
                Export("opt_tup_0").into(),
                [Struct(
                    "OptTup0",
                    [("0", OptSome(Tuple([].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptTup0", [("0", OptSome(Tuple([].into()).into()))].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_0").into(),
                [Struct(
                    "OptTup0",
                    [("0", OptSome(Tuple([].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptTup0", [("0", OptNone(TyTuple([].into())))].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_0").into(),
                [Struct(
                    "OptTup0",
                    [("0", OptNone(TyTuple([].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptTup0", [("0", OptSome(Tuple([].into()).into()))].into()).into(),
        ),
    ]);

    // OptTup1
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("opt_tup_1").into(),
                [Struct(
                    "OptTup1",
                    [("0", OptNone(TyTuple([TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptTup1", [("0", OptNone(TyTuple([TyI32].into())))].into()).into(),
        ),
        AssertEqual(
            Call(
                Export("opt_tup_1").into(),
                [Struct(
                    "OptTup1",
                    [("0", OptSome(Tuple([I32(1)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup1",
                [("0", OptSome(Tuple([I32(1)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_1").into(),
                [Struct(
                    "OptTup1",
                    [("0", OptSome(Tuple([I32(1)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptTup1", [("0", OptNone(TyTuple([TyI32].into())))].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_1").into(),
                [Struct(
                    "OptTup1",
                    [("0", OptNone(TyTuple([TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup1",
                [("0", OptSome(Tuple([I32(1)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_1").into(),
                [Struct(
                    "OptTup1",
                    [("0", OptSome(Tuple([I32(1)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup1",
                [("0", OptSome(Tuple([I32(2)].into()).into()))].into(),
            )
            .into(),
        ),
    ]);

    // OptTup2
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("opt_tup_2").into(),
                [Struct(
                    "OptTup2",
                    [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup2",
                [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("opt_tup_2").into(),
                [Struct(
                    "OptTup2",
                    [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup2",
                [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_2").into(),
                [Struct(
                    "OptTup2",
                    [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup2",
                [("0", OptSome(Tuple([I32(2), I32(1)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_2").into(),
                [Struct(
                    "OptTup2",
                    [("0", OptNone(TyTuple([TyI32, TyI32].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup2",
                [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_tup_2").into(),
                [Struct(
                    "OptTup2",
                    [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptTup2",
                [("0", OptNone(TyTuple([TyI32, TyI32].into()).into()))].into(),
            )
            .into(),
        ),
    ]);

    // EnumBoxTup
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("enum_box_tup").into(),
                [EnumPayload(
                    "EnumBoxTup",
                    "Foo",
                    [(
                        "0",
                        Boxed(
                            TyTuple([TyI32, TyI32].into()),
                            Tuple([I32(1), I32(2)].into()).into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumBoxTup",
                "Foo",
                [(
                    "0",
                    Boxed(
                        TyTuple([TyI32, TyI32].into()),
                        Tuple([I32(1), I32(2)].into()).into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_box_tup").into(),
                [EnumPayload(
                    "EnumBoxTup",
                    "Foo",
                    [(
                        "0",
                        Boxed(
                            TyTuple([TyI32, TyI32].into()),
                            Tuple([I32(1), I32(2)].into()).into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumBoxTup",
                "Foo",
                [(
                    "0",
                    Boxed(
                        TyTuple([TyI32, TyI32].into()),
                        Tuple([I32(1), I32(3)].into()).into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("enum_box_tup").into(),
                [EnumPayload("EnumBoxTup", "Bar", [].into())].into(),
            )
            .into(),
            EnumPayload("EnumBoxTup", "Bar", [].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_box_tup").into(),
                [EnumPayload(
                    "EnumBoxTup",
                    "Foo",
                    [(
                        "0",
                        Boxed(
                            TyTuple([TyI32, TyI32].into()),
                            Tuple([I32(1), I32(2)].into()).into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload("EnumBoxTup", "Bar", [].into()).into(),
        ),
    ]);

    // EnumVecTup
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload(
                    "EnumVecTup",
                    "Foo",
                    [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumVecTup",
                "Foo",
                [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload(
                    "EnumVecTup",
                    "Foo",
                    [("0", Vector([Tuple([I32(1), I32(2)].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumVecTup",
                "Foo",
                [("0", Vector([Tuple([I32(1), I32(2)].into())].into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload(
                    "EnumVecTup",
                    "Foo",
                    [("0", Vector([Tuple([I32(1), I32(2)].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumVecTup",
                "Foo",
                [("0", Vector([Tuple([I32(1), I32(3)].into())].into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload(
                    "EnumVecTup",
                    "Foo",
                    [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumVecTup",
                "Foo",
                [("0", Vector([Tuple([I32(1), I32(2)].into())].into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload(
                    "EnumVecTup",
                    "Foo",
                    [("0", Vector([Tuple([I32(1), I32(2)].into())].into()))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumVecTup",
                "Foo",
                [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload("EnumVecTup", "Bar", [].into())].into(),
            )
            .into(),
            EnumPayload("EnumVecTup", "Bar", [].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_vec_tup").into(),
                [EnumPayload("EnumVecTup", "Bar", [].into())].into(),
            )
            .into(),
            EnumPayload(
                "EnumVecTup",
                "Foo",
                [("0", EmptyVector(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
    ]);

    // EnumOptTup
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload(
                    "EnumOptTup",
                    "Foo",
                    [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumOptTup",
                "Foo",
                [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload(
                    "EnumOptTup",
                    "Foo",
                    [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumOptTup",
                "Foo",
                [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload(
                    "EnumOptTup",
                    "Foo",
                    [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumOptTup",
                "Foo",
                [("0", OptSome(Tuple([I32(1), I32(3)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload(
                    "EnumOptTup",
                    "Foo",
                    [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumOptTup",
                "Foo",
                [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload(
                    "EnumOptTup",
                    "Foo",
                    [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
                )]
                .into(),
            )
            .into(),
            EnumPayload(
                "EnumOptTup",
                "Foo",
                [("0", OptSome(Tuple([I32(1), I32(2)].into()).into()))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload("EnumOptTup", "Bar", [].into())].into(),
            )
            .into(),
            EnumPayload("EnumOptTup", "Bar", [].into()).into(),
        ),
        AssertNotEqual(
            Call(
                Export("enum_opt_tup").into(),
                [EnumPayload("EnumOptTup", "Bar", [].into())].into(),
            )
            .into(),
            EnumPayload(
                "EnumOptTup",
                "Foo",
                [("0", OptNone(TyTuple([TyI32, TyI32].into())))].into(),
            )
            .into(),
        ),
    ]);

    // TupBox
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("tup_box").into(),
                [Struct(
                    "TupBox",
                    [(
                        "0",
                        Tuple(
                            [
                                Boxed(TyI32, I32(0).into()),
                                Boxed(TyBool, Bool(true).into()),
                            ]
                            .into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "TupBox",
                [(
                    "0",
                    Tuple(
                        [
                            Boxed(TyI32, I32(0).into()),
                            Boxed(TyBool, Bool(true).into()),
                        ]
                        .into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("tup_box").into(),
                [Struct(
                    "TupBox",
                    [(
                        "0",
                        Tuple(
                            [
                                Boxed(TyI32, I32(0).into()),
                                Boxed(TyBool, Bool(true).into()),
                            ]
                            .into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "TupBox",
                [(
                    "0",
                    Tuple(
                        [
                            Boxed(TyI32, I32(0).into()),
                            Boxed(TyBool, Bool(false).into()),
                        ]
                        .into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("tup_box").into(),
                [Struct(
                    "TupBox",
                    [(
                        "0",
                        Tuple(
                            [
                                Boxed(TyI32, I32(0).into()),
                                Boxed(TyBool, Bool(true).into()),
                            ]
                            .into(),
                        ),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "TupBox",
                [(
                    "0",
                    Tuple(
                        [
                            Boxed(TyI32, I32(2).into()),
                            Boxed(TyBool, Bool(true).into()),
                        ]
                        .into(),
                    ),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    // VecBox
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("vec_box").into(),
                [Struct(
                    "VecBox",
                    [("0", EmptyVector(TyBox(TyI32.into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("VecBox", [("0", EmptyVector(TyBox(TyI32.into())))].into()).into(),
        ),
        AssertEqual(
            Call(
                Export("vec_box").into(),
                [Struct(
                    "VecBox",
                    [(
                        "0",
                        Vector([Boxed(TyI32, I32(10).into()), Boxed(TyI32, I32(20).into())].into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecBox",
                [(
                    "0",
                    Vector([Boxed(TyI32, I32(10).into()), Boxed(TyI32, I32(20).into())].into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_box").into(),
                [Struct(
                    "VecBox",
                    [(
                        "0",
                        Vector([Boxed(TyI32, I32(10).into()), Boxed(TyI32, I32(20).into())].into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecBox",
                [(
                    "0",
                    Vector([Boxed(TyI32, I32(10).into()), Boxed(TyI32, I32(30).into())].into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_box").into(),
                [Struct(
                    "VecBox",
                    [("0", EmptyVector(TyBox(TyI32.into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "VecBox",
                [(
                    "0",
                    Vector([Boxed(TyI32, I32(10).into()), Boxed(TyI32, I32(20).into())].into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("vec_box").into(),
                [Struct(
                    "VecBox",
                    [(
                        "0",
                        Vector([Boxed(TyI32, I32(10).into()), Boxed(TyI32, I32(20).into())].into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct("VecBox", [("0", EmptyVector(TyBox(TyI32.into())))].into()).into(),
        ),
    ]);

    // BoxVec
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("box_vec").into(),
                [Struct(
                    "BoxVec",
                    [("0", Boxed(TyVec(TyI32.into()), EmptyVector(TyI32).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxVec",
                [("0", Boxed(TyVec(TyI32.into()), EmptyVector(TyI32).into()))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("box_vec").into(),
                [Struct(
                    "BoxVec",
                    [(
                        "0",
                        Boxed(TyVec(TyI32.into()), Vector([I32(1), I32(2)].into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxVec",
                [(
                    "0",
                    Boxed(TyVec(TyI32.into()), Vector([I32(1), I32(2)].into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_vec").into(),
                [Struct(
                    "BoxVec",
                    [(
                        "0",
                        Boxed(TyVec(TyI32.into()), Vector([I32(1)].into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxVec",
                [(
                    "0",
                    Boxed(TyVec(TyI32.into()), Vector([I32(1), I32(2)].into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_vec").into(),
                [Struct(
                    "BoxVec",
                    [(
                        "0",
                        Boxed(TyVec(TyI32.into()), Vector([I32(1), I32(2)].into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxVec",
                [(
                    "0",
                    Boxed(TyVec(TyI32.into()), Vector([I32(1)].into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    // OptBox
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("opt_box").into(),
                [Struct(
                    "OptBox",
                    [("0", OptNone(TyBox(TyI32.into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptBox", [("0", OptNone(TyBox(TyI32.into())))].into()).into(),
        ),
        AssertEqual(
            Call(
                Export("opt_box").into(),
                [Struct(
                    "OptBox",
                    [("0", OptSome(Boxed(TyI32.into(), I32(10).into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptBox",
                [("0", OptSome(Boxed(TyI32.into(), I32(10).into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_box").into(),
                [Struct(
                    "OptBox",
                    [("0", OptSome(Boxed(TyI32.into(), I32(10).into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptBox",
                [("0", OptSome(Boxed(TyI32.into(), I32(20).into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_box").into(),
                [Struct(
                    "OptBox",
                    [("0", OptNone(TyBox(TyI32.into())))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "OptBox",
                [("0", OptSome(Boxed(TyI32.into(), I32(10).into()).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("opt_box").into(),
                [Struct(
                    "OptBox",
                    [("0", OptSome(Boxed(TyI32.into(), I32(10).into()).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct("OptBox", [("0", OptNone(TyBox(TyI32.into())))].into()).into(),
        ),
    ]);

    // BoxOpt
    case.checks.extend_from_slice(&[
        AssertEqual(
            Call(
                Export("box_opt").into(),
                [Struct(
                    "BoxOpt",
                    [("0", Boxed(TyOption(TyI32.into()), OptNone(TyI32).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxOpt",
                [("0", Boxed(TyOption(TyI32.into()), OptNone(TyI32).into()))].into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(
                Export("box_opt").into(),
                [Struct(
                    "BoxOpt",
                    [(
                        "0",
                        Boxed(TyOption(TyI32.into()), OptSome(I32(10).into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxOpt",
                [(
                    "0",
                    Boxed(TyOption(TyI32.into()), OptSome(I32(10).into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_opt").into(),
                [Struct(
                    "BoxOpt",
                    [(
                        "0",
                        Boxed(TyOption(TyI32.into()), OptSome(I32(10).into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxOpt",
                [(
                    "0",
                    Boxed(TyOption(TyI32.into()), OptSome(I32(20).into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_opt").into(),
                [Struct(
                    "BoxOpt",
                    [(
                        "0",
                        Boxed(TyOption(TyI32.into()), OptSome(I32(10).into()).into()),
                    )]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxOpt",
                [("0", Boxed(TyOption(TyI32.into()), OptNone(TyI32).into()))].into(),
            )
            .into(),
        ),
        AssertNotEqual(
            Call(
                Export("box_opt").into(),
                [Struct(
                    "BoxOpt",
                    [("0", Boxed(TyOption(TyI32.into()), OptNone(TyI32).into()))].into(),
                )]
                .into(),
            )
            .into(),
            Struct(
                "BoxOpt",
                [(
                    "0",
                    Boxed(TyOption(TyI32.into()), OptSome(I32(10).into()).into()),
                )]
                .into(),
            )
            .into(),
        ),
    ]);

    case
}

test_all!();
