use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub struct EmptyStruct;
        pub struct SingleElementStruct(i32);
        pub struct PairStruct { x: f32, y: f32 }

        pub trait StructIn {
            fn empty_struct(&self, x: i32, #[allow(unused)] foo: EmptyStruct, y: i32) -> i32;
            fn single_element_struct(&self, x: SingleElementStruct, y: SingleElementStruct) -> i32;
            fn multiply_pairs(&self, ab: PairStruct, cd: PairStruct) -> f32;
        }

        pub fn set_empty_struct(struct_in: Rc<dyn StructIn>) -> i32 {
            struct_in.empty_struct(10, EmptyStruct, 20)
        }

        pub fn set_single_element_struct(struct_in: Rc<dyn StructIn>) -> i32 {
            struct_in.single_element_struct(SingleElementStruct(10), SingleElementStruct(20))
        }

        pub fn set_multiply_pairs(struct_in: Rc<dyn StructIn>) -> f32 {
            struct_in.multiply_pairs(PairStruct { x: 2.0, y: 3.0 }, PairStruct { x: 5.0, y: 7.0 })
        }
    "#;

    case.js_pre = "
        class StructInImpl {
            empty_struct(x, foo, y) {
                assert.deepStrictEqual(foo, {})
                return x + y
            }
            single_element_struct(x, y) {
                return x[0] + y[0]
            }
            multiply_pairs({ x: a, y: b }, { x: c, y: d }) {
                return a * c - b * d
            }
        }
    ";

    case.swift_pre = "
        class StructInImpl : StructIn {
            func empty_struct(_ x: Int32, _ foo: EmptyStruct, _ y: Int32) -> Int32 {
                return x + y
            }
            func single_element_struct(_ x: SingleElementStruct, _ y: SingleElementStruct) -> Int32 {
                return x._0 + y._0
            }
            func multiply_pairs(_ ab: PairStruct, _ cd: PairStruct) -> Float32 {
                return ab.x * cd.x - ab.y * cd.y
            }
        }
    ";

    case.cpp_pre = "
        struct StructInImpl : rust::StructIn {
            int32_t empty_struct(int32_t x, rust::EmptyStruct, int32_t y) {
                return x + y;
            }
            int32_t single_element_struct(rust::SingleElementStruct x, rust::SingleElementStruct y) {
                return x._0 + y._0;
            }
            float multiply_pairs(rust::PairStruct ab, rust::PairStruct cd) {
                return ab.x * cd.x - ab.y * cd.y;
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            Call(
                Export("set_empty_struct").into(),
                [NewRc(TyName("StructIn"), "StructInImpl", [].into())].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("set_single_element_struct").into(),
                [NewRc(TyName("StructIn"), "StructInImpl", [].into())].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("set_multiply_pairs").into(),
                [NewRc(TyName("StructIn"), "StructInImpl", [].into())].into(),
            )
            .into(),
            F32(-11.0).into(),
        ),
    ];

    case
}

test_all!();
