use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn sum_u8(values: Vec<u8>) -> u8 { values.iter().sum() }
        pub fn sum_u16(values: Vec<u16>) -> u16 { values.iter().sum() }
        pub fn sum_u32(values: Vec<u32>) -> u32 { values.iter().sum() }
        pub fn sum_usize(values: Vec<usize>) -> usize { values.iter().sum() }
        pub fn sum_u64(values: Vec<u64>) -> u64 { values.iter().sum() }

        pub fn sum_i8(values: Vec<i8>) -> i8 { values.iter().sum() }
        pub fn sum_i16(values: Vec<i16>) -> i16 { values.iter().sum() }
        pub fn sum_i32(values: Vec<i32>) -> i32 { values.iter().sum() }
        pub fn sum_isize(values: Vec<isize>) -> isize { values.iter().sum() }
        pub fn sum_i64(values: Vec<i64>) -> i64 { values.iter().sum() }

        // Note: Rust changed how "sum()" behaves in version 1.82.0. It used
        // to return +0.0 when empty but now returns -0.0. Detect this case and
        // deliberately always return -0.0 to make this pass with older Rust.
        // https://github.com/rust-lang/rust/commit/490818851860fb257e23fe7aa0ee32eaffc4ba40
        pub fn sum_f32(values: Vec<f32>) -> f32 {
            if values.is_empty() { -0.0 } else { values.iter().sum() }
        }
        pub fn sum_f64(values: Vec<f64>) -> f64 {
            if values.is_empty() { -0.0 } else { values.iter().sum() }
        }

        pub fn check_nested(values: Vec<Vec<i32>>) -> String {
            format!("{values:?}")
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("sum_u8").into(), [EmptyVector(TyU8)].into()).into(),
            U8(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_u16").into(), [EmptyVector(TyU16)].into()).into(),
            U16(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_u32").into(), [EmptyVector(TyU32)].into()).into(),
            U32(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_usize").into(), [EmptyVector(TyUsize)].into()).into(),
            Usize(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_u64").into(), [EmptyVector(TyU64)].into()).into(),
            U64(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_i8").into(), [EmptyVector(TyI8)].into()).into(),
            I8(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_i16").into(), [EmptyVector(TyI16)].into()).into(),
            I16(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_i32").into(), [EmptyVector(TyI32)].into()).into(),
            I32(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_isize").into(), [EmptyVector(TyIsize)].into()).into(),
            Isize(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_i64").into(), [EmptyVector(TyI64)].into()).into(),
            I64(0).into(),
        ),
        AssertEqual(
            Call(Export("sum_f32").into(), [EmptyVector(TyF32)].into()).into(),
            F32(-0.0).into(),
        ),
        AssertEqual(
            Call(Export("sum_f64").into(), [EmptyVector(TyF64)].into()).into(),
            F64(-0.0).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_u8").into(),
                [Vector([U8(10), U8(20), U8(30)].into())].into(),
            )
            .into(),
            U8(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_u16").into(),
                [Vector([U16(10), U16(20), U16(30)].into())].into(),
            )
            .into(),
            U16(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_u32").into(),
                [Vector([U32(10), U32(20), U32(30)].into())].into(),
            )
            .into(),
            U32(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_isize").into(),
                [Vector([Isize(10), Isize(20), Isize(30)].into())].into(),
            )
            .into(),
            Isize(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_u64").into(),
                [Vector([U64(10), U64(20), U64(30)].into())].into(),
            )
            .into(),
            U64(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_i8").into(),
                [Vector([I8(10), I8(20), I8(30)].into())].into(),
            )
            .into(),
            I8(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_i16").into(),
                [Vector([I16(10), I16(20), I16(30)].into())].into(),
            )
            .into(),
            I16(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_i32").into(),
                [Vector([I32(10), I32(20), I32(30)].into())].into(),
            )
            .into(),
            I32(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_i64").into(),
                [Vector([I64(10), I64(20), I64(30)].into())].into(),
            )
            .into(),
            I64(60).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_f32").into(),
                [Vector([F32(1.5), F32(2.5), F32(3.5)].into())].into(),
            )
            .into(),
            F32(7.5).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_f64").into(),
                [Vector([F64(1.5), F64(2.5), F64(3.5)].into())].into(),
            )
            .into(),
            F64(7.5).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_i32").into(),
                [Vector(
                    [
                        I32(1),
                        I32(2),
                        I32(3),
                        I32(4),
                        I32(5),
                        I32(6),
                        I32(7),
                        I32(8),
                        I32(9),
                        I32(10),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            I32(55).into(),
        ),
        AssertEqual(
            Call(
                Export("sum_u64").into(),
                [Vector(
                    [
                        U64(1),
                        U64(2),
                        U64(3),
                        U64(4),
                        U64(5),
                        U64(6),
                        U64(7),
                        U64(8),
                        U64(9),
                        U64(10),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            U64(55).into(),
        ),
        AssertEqual(
            Call(
                Export("check_nested").into(),
                [Vector(
                    [
                        EmptyVector(TyI32),
                        Vector([I32(1)].into()),
                        Vector([I32(2), I32(3)].into()),
                        Vector([I32(4), I32(5), I32(6)].into()),
                        Vector([I32(7), I32(8), I32(9), I32(10)].into()),
                    ]
                    .into(),
                )]
                .into(),
            )
            .into(),
            Str("[[], [1], [2, 3], [4, 5, 6], [7, 8, 9, 10]]").into(),
        ),
    ];

    case
}

test_all!();
