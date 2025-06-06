use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub const CONST_FALSE: bool = false;
        pub const CONST_TRUE: bool = true;

        pub const CONST_U8_MIN: u8 = 0;
        pub const CONST_U8_MAX: u8 = 0xff;
        pub const CONST_U16_MIN: u16 = 0;
        pub const CONST_U16_MAX: u16 = 0xffff;
        pub const CONST_U32_MIN: u32 = 0;
        pub const CONST_U32_MAX: u32 = 0xffff_ffff;
        pub const CONST_U64_MIN: u64 = 0;
        pub const CONST_U64_MAX: u64 = 0xffff_ffff_ffff_ffff;

        pub const CONST_I8_MIN: i8 = -0x80;
        pub const CONST_I8_MAX: i8 = 0x7f;
        pub const CONST_I16_MIN: i16 = -0x8000;
        pub const CONST_I16_MAX: i16 = 0x7fff;
        pub const CONST_I32_MIN: i32 = -0x8000_0000;
        pub const CONST_I32_MAX: i32 = 0x7fff_ffff;
        pub const CONST_I64_MIN: i64 = -0x8000_0000_0000_0000;
        pub const CONST_I64_MAX: i64 = 0x7fff_ffff_ffff_ffff;

        pub const CONST_F32_NEG_0: f32 = -0.0;
        pub const CONST_F64_NEG_0: f64 = -0.0;
        pub const CONST_F32_PI: f32 = 3.141592653589793;
        pub const CONST_F64_PI: f64 = -3.141592653589793;

        pub const CONST_STRING: &str = "\0\r\n\u{1f980}";
    "#;

    case.checks = vec![
        AssertEqual(Export("CONST_FALSE").into(), Bool(false).into()),
        AssertEqual(Export("CONST_TRUE").into(), Bool(true).into()),
        AssertEqual(Export("CONST_U8_MIN").into(), U8(0).into()),
        AssertEqual(Export("CONST_U8_MAX").into(), U8(0xff).into()),
        AssertEqual(Export("CONST_U16_MIN").into(), U16(0).into()),
        AssertEqual(Export("CONST_U16_MAX").into(), U16(0xffff).into()),
        AssertEqual(Export("CONST_U32_MIN").into(), U32(0).into()),
        AssertEqual(Export("CONST_U32_MAX").into(), U32(0xffff_ffff).into()),
        AssertEqual(Export("CONST_U64_MIN").into(), U64(0).into()),
        AssertEqual(
            Export("CONST_U64_MAX").into(),
            U64(0xffff_ffff_ffff_ffff).into(),
        ),
        AssertEqual(Export("CONST_I8_MIN").into(), I8(-0x80).into()),
        AssertEqual(Export("CONST_I8_MAX").into(), I8(0x7f).into()),
        AssertEqual(Export("CONST_I16_MIN").into(), I16(-0x8000).into()),
        AssertEqual(Export("CONST_I16_MAX").into(), I16(0x7fff).into()),
        AssertEqual(Export("CONST_I32_MIN").into(), I32(-0x8000_0000).into()),
        AssertEqual(Export("CONST_I32_MAX").into(), I32(0x7fff_ffff).into()),
        AssertEqual(
            Export("CONST_I64_MIN").into(),
            I64(-0x8000_0000_0000_0000).into(),
        ),
        AssertEqual(
            Export("CONST_I64_MAX").into(),
            I64(0x7fff_ffff_ffff_ffff).into(),
        ),
        AssertEqual(Export("CONST_F32_NEG_0").into(), F32(-0.0).into()),
        AssertEqual(Export("CONST_F64_NEG_0").into(), F64(-0.0).into()),
        AssertEqual(
            Export("CONST_F32_PI").into(),
            F32(std::f32::consts::PI).into(),
        ),
        AssertEqual(
            Export("CONST_F64_PI").into(),
            F64(-std::f64::consts::PI).into(),
        ),
        AssertEqual(Export("CONST_STRING").into(), Str("\0\r\n\u{1f980}").into()),
    ];

    case
}

test_all!();
