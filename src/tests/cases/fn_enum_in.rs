use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub enum Foo {
            Zero,
            One,
            Hundred = 100,
        }
        pub fn foo_to_i32(foo: Foo) -> i32 {
            foo as i32
        }

        pub enum Big {
            Min = -0x8000_0000,
            Max = 0x7fff_ffff,
        }
        pub fn big_to_i32(big: Big) -> i32 {
            big as i32
        }

        // Also test code generation for enums with many members
        pub enum LongEnum {
            Empty,
            ShortTuple(i32),
            ShortStruct { a: i32, },
            LongTuple(i32, i32, i32, i32, i32, i32, i32, i32),
            LongStruct { a: i32, b: i32, c: i32, d: i32, e: i32, f: i32 },
        }
        pub fn long_in(_: LongEnum) {}
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("foo_to_i32").into(), [Enum("Foo", "Zero")].into()).into(),
            I32(0).into(),
        ),
        AssertEqual(
            Call(Export("foo_to_i32").into(), [Enum("Foo", "One")].into()).into(),
            I32(1).into(),
        ),
        AssertEqual(
            Call(Export("foo_to_i32").into(), [Enum("Foo", "Hundred")].into()).into(),
            I32(100).into(),
        ),
        AssertEqual(
            Call(Export("big_to_i32").into(), [Enum("Big", "Min")].into()).into(),
            I32(-0x8000_0000).into(),
        ),
        AssertEqual(
            Call(Export("big_to_i32").into(), [Enum("Big", "Max")].into()).into(),
            I32(0x7fff_ffff).into(),
        ),
    ];

    case
}

test_all!();
