use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        // These tests are for C++ where declaration is order-dependent

        // Traits that refer to each other
        pub trait FirstTrait {
            fn second(&self) -> Rc<dyn SecondTrait>;
        }
        pub trait SecondTrait {
            fn first(&self) -> Rc<dyn FirstTrait>;
        }

        ////////////////////

        // Early struct inside later struct
        pub struct EarlyInsideStruct {
            pub y: bool,
        }
        pub struct LaterOutsideStruct {
            pub x: EarlyInsideStruct,
        }

        // Early struct outside later struct
        pub struct EarlyOutsideStruct {
            pub x: LaterInsideStruct,
        }
        pub struct LaterInsideStruct {
            pub y: bool,
        }

        // Structs that refer to each other
        pub struct FirstStruct {
            pub second: Vec<SecondStruct>,
        }
        pub struct SecondStruct {
            pub first: Vec<FirstStruct>,
        }

        ////////////////////

        // Early enum inside later enum
        pub enum EarlyInsideEnum {
            Value(bool),
        }
        pub enum LaterOutsideEnum {
            Value(EarlyInsideEnum),
        }

        // Early enum outside later enum
        pub enum EarlyOutsideEnum {
            Value(LaterInsideEnum),
        }
        pub enum LaterInsideEnum {
            Value(bool),
        }

        // Enums that refer to each other
        pub enum FirstEnum {
            Second(Vec<SecondEnum>),
        }
        pub enum SecondEnum {
            First(Vec<FirstEnum>),
        }
    "#;

    case
}

test_all!();
