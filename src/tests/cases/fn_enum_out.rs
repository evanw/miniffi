use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub enum Foo {
            Zero,
            One,
            Hundred = 100,
        }
        pub fn i32_to_foo(foo: i32) -> Foo {
            match foo {
                0 => Foo::Zero,
                1 => Foo::One,
                2 => Foo::Hundred,
                _ => panic!(),
            }
        }

        pub enum Big {
            Min = -0x8000_0000,
            Max = 0x7fff_ffff,
        }
        pub fn i32_to_big(big: i32) -> Big {
            match big {
                0 => Big::Min,
                1 => Big::Max,
                _ => panic!(),
            }
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("i32_to_foo").into(), [I32(0)].into()).into(),
            Enum("Foo", "Zero").into(),
        ),
        AssertEqual(
            Call(Export("i32_to_foo").into(), [I32(1)].into()).into(),
            Enum("Foo", "One").into(),
        ),
        AssertEqual(
            Call(Export("i32_to_foo").into(), [I32(2)].into()).into(),
            Enum("Foo", "Hundred").into(),
        ),
        AssertEqual(
            Call(Export("i32_to_big").into(), [I32(0)].into()).into(),
            Enum("Big", "Min").into(),
        ),
        AssertEqual(
            Call(Export("i32_to_big").into(), [I32(1)].into()).into(),
            Enum("Big", "Max").into(),
        ),
    ];

    case
}

test_all!();
