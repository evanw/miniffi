use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub struct Foo {
            ptr: Rc<dyn Bar>,
        }

        pub trait Bar {
            fn get(&self) -> i32;
        }

        struct BarImpl(i32);

        impl Bar for BarImpl {
            fn get(&self) -> i32 {
                self.0
            }
        }

        pub fn test(x: i32) -> (i32, Foo) {
            (x, Foo { ptr: Rc::new(BarImpl(-x)) })
        }
    "#;

    case.checks = vec![
        AssertEqual(
            TupleMember(Call(Export("test").into(), [I32(123)].into()).into(), 0).into(),
            I32(123).into(),
        ),
        AssertEqual(
            CallMethod(
                StructMember(
                    TupleMember(Call(Export("test").into(), [I32(123)].into()).into(), 1).into(),
                    "ptr",
                )
                .into(),
                "get",
                [].into(),
            )
            .into(),
            I32(-123).into(),
        ),
    ];

    case
}

test_all!();
