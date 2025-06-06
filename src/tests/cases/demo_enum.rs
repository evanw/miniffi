use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        // This tests the generation of "indirect case" in Swift

        pub enum NestedBox {
            Foo,
            Bar(Box<NestedBox>),
        }

        pub enum NestedVec {
            Foo,
            Bar(Vec<NestedVec>),
        }

        pub enum NestedOption {
            Foo,
            Bar(Option<Box<NestedOption>>),
        }

        pub enum NestedTuple {
            Foo,
            Bar((i32, Box<NestedTuple>)),
        }

        pub enum NestedStruct {
            Foo,
            Bar((i32, Box<InnerStruct>)),
        }

        pub struct InnerStruct {
            pub x: NestedStruct,
        }

        pub enum NestedEnum {
            Foo,
            Bar((i32, Box<InnerEnum>)),
        }

        pub enum InnerEnum {
            Foo(NestedEnum),
        }
    "#;

    case
}

test_all!();
