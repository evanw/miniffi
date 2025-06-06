use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub trait Foo {
            fn set_enum(&self, bar: Bar);
            fn get_enum(&self) -> Bar;
        }

        pub fn get_foo() -> Box<dyn Foo> {
            struct FooImpl;
            impl Foo for FooImpl {
                fn set_enum(&self, bar: Bar) {
                    assert_eq!(bar, Bar::C);
                }
                fn get_enum(&self) -> Bar {
                    Bar::B
                }
            }
            Box::new(FooImpl)
        }

        #[derive(Debug, Eq, PartialEq)]
        pub enum Bar {
            A,
            B,
            C,
        }
    "#;

    case.checks = vec![
        DeclareImmutableLocal("foo", Call(Export("get_foo").into(), [].into()).into()),
        CallMethod(
            LoadLocal("foo").into(),
            "set_enum",
            [Enum("Bar", "C")].into(),
        ),
        AssertEqual(
            CallMethod(LoadLocal("foo").into(), "get_enum", [].into()).into(),
            Enum("Bar", "B").into(),
        ),
    ];

    case
}

test_all!();
