use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;

        pub trait TupleIn {
            fn empty_tuple(&self, x: i32, foo: (), y: i32) -> i32;
            fn single_element_tuple(&self, x: (i32,), y: (i32,)) -> i32;
            fn multiply_pairs(&self, ab: (f32, f32), cd: (f32, f32)) -> f32;
        }

        pub fn set_empty_tuple(tuple_in: Rc<dyn TupleIn>) -> i32 {
            tuple_in.empty_tuple(10, (), 20)
        }

        pub fn set_single_element_tuple(tuple_in: Rc<dyn TupleIn>) -> i32 {
            tuple_in.single_element_tuple((10,), (20,))
        }

        pub fn set_multiply_pairs(tuple_in: Rc<dyn TupleIn>) -> f32 {
            tuple_in.multiply_pairs((2.0, 3.0), (5.0, 7.0))
        }
    "#;

    case.js_pre = "
        class TupleInImpl {
            empty_tuple(x, foo, y) {
                assert.deepStrictEqual(foo, undefined)
                return x + y
            }
            single_element_tuple(x, y) {
                return x[0] + y[0]
            }
            multiply_pairs([a, b], [c, d]) {
                return a * c - b * d
            }
        }
    ";

    case.swift_pre = "
        class TupleInImpl : TupleIn {
            func empty_tuple(_ x: Int32, _ foo: (), _ y: Int32) -> Int32 {
                return x + y
            }
            func single_element_tuple(_ x: Int32, _ y: Int32) -> Int32 {
                return x + y
            }
            func multiply_pairs(_ ab: (Float32, Float32), _ cd: (Float32, Float32)) -> Float32 {
                let (a, b) = ab
                let (c, d) = cd
                return a * c - b * d
            }
        }
    ";

    case.cpp_pre = "
        struct TupleInImpl : rust::TupleIn {
            int32_t empty_tuple(int32_t x, std::tuple<>, int32_t y) {
                return x + y;
            }
            int32_t single_element_tuple(std::tuple<int32_t> x, std::tuple<int32_t> y) {
                return std::get<0>(x) + std::get<0>(y);
            }
            float multiply_pairs(std::tuple<float, float> ab, std::tuple<float, float> cd) {
                float a, b, c, d;
                std::tie(a, b) = ab;
                std::tie(c, d) = cd;
                return a * c - b * d;
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            Call(
                Export("set_empty_tuple").into(),
                [NewRc(TyName("TupleIn"), "TupleInImpl", [].into())].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("set_single_element_tuple").into(),
                [NewRc(TyName("TupleIn"), "TupleInImpl", [].into())].into(),
            )
            .into(),
            I32(30).into(),
        ),
        AssertEqual(
            Call(
                Export("set_multiply_pairs").into(),
                [NewRc(TyName("TupleIn"), "TupleInImpl", [].into())].into(),
            )
            .into(),
            F32(-11.0).into(),
        ),
    ];

    case
}

test_all!();
