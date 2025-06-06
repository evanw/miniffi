use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn add_bool(x: bool, y: bool) -> bool { x || y }

        pub fn add_u8(x: u8, y: u8) -> u8 { x + y }
        pub fn add_u16(x: u16, y: u16) -> u16 { x + y }
        pub fn add_u32(x: u32, y: u32) -> u32 { x + y }
        pub fn add_usize(x: usize, y: usize) -> usize { x + y }
        pub fn add_u64(x: u64, y: u64) -> u64 { x + y }

        pub fn add_i8(x: i8, y: i8) -> i8 { x + y }
        pub fn add_i16(x: i16, y: i16) -> i16 { x + y }
        pub fn add_i32(x: i32, y: i32) -> i32 { x + y }
        pub fn add_isize(x: isize, y: isize) -> isize { x + y }
        pub fn add_i64(x: i64, y: i64) -> i64 { x + y }

        pub fn add_f32(x: f32, y: f32) -> f32 { x + y }
        pub fn add_f64(x: f64, y: f64) -> f64 { x + y }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("add_bool").into(), [Bool(false), Bool(false)].into()).into(),
            Bool(false).into(),
        ),
        AssertEqual(
            Call(Export("add_bool").into(), [Bool(true), Bool(false)].into()).into(),
            Bool(true).into(),
        ),
        AssertEqual(
            Call(Export("add_bool").into(), [Bool(false), Bool(true)].into()).into(),
            Bool(true).into(),
        ),
        AssertEqual(
            Call(Export("add_bool").into(), [Bool(true), Bool(true)].into()).into(),
            Bool(true).into(),
        ),
        AssertEqual(
            Call(Export("add_u8").into(), [U8(1), U8(2)].into()).into(),
            U8(3).into(),
        ),
        AssertEqual(
            Call(Export("add_u16").into(), [U16(1), U16(2)].into()).into(),
            U16(3).into(),
        ),
        AssertEqual(
            Call(Export("add_u32").into(), [U32(1), U32(2)].into()).into(),
            U32(3).into(),
        ),
        AssertEqual(
            Call(Export("add_usize").into(), [Usize(1), Usize(2)].into()).into(),
            Usize(3).into(),
        ),
        AssertEqual(
            Call(Export("add_u64").into(), [U64(1), U64(2)].into()).into(),
            U64(3).into(),
        ),
        AssertEqual(
            Call(Export("add_i8").into(), [I8(1), I8(2)].into()).into(),
            I8(3).into(),
        ),
        AssertEqual(
            Call(Export("add_i16").into(), [I16(1), I16(2)].into()).into(),
            I16(3).into(),
        ),
        AssertEqual(
            Call(Export("add_i32").into(), [I32(1), I32(2)].into()).into(),
            I32(3).into(),
        ),
        AssertEqual(
            Call(Export("add_isize").into(), [Isize(1), Isize(2)].into()).into(),
            Isize(3).into(),
        ),
        AssertEqual(
            Call(Export("add_i64").into(), [I64(1), I64(2)].into()).into(),
            I64(3).into(),
        ),
        AssertEqual(
            Call(Export("add_f32").into(), [F32(1.5), F32(-0.25)].into()).into(),
            F32(1.25).into(),
        ),
        AssertEqual(
            Call(Export("add_f64").into(), [F64(-1.5), F64(0.25)].into()).into(),
            F64(-1.25).into(),
        ),
    ];

    case
}

test_all!();
