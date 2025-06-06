use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        pub fn get_vec(n: i32) -> Vec<i32> {
            let mut vec = Vec::new();
            for i in 0..n {
                vec.push(i * 10);
            }
            vec
        }

        pub fn check_nested() -> Vec<Vec<i32>> {
            vec![vec![], vec![1], vec![2, 3], vec![4, 5, 6], vec![7, 8, 9, 10]]
        }
    "#;

    case.checks = vec![
        AssertEqual(
            VectorLen(Call(Export("get_vec").into(), [I32(0)].into()).into()).into(),
            Usize(0).into(),
        ),
        AssertEqual(
            Call(Export("get_vec").into(), [I32(3)].into()).into(),
            Vector([I32(0), I32(10), I32(20)].into()).into(),
        ),
        AssertEqual(
            Call(Export("get_vec").into(), [I32(10)].into()).into(),
            Vector(
                [
                    I32(0),
                    I32(10),
                    I32(20),
                    I32(30),
                    I32(40),
                    I32(50),
                    I32(60),
                    I32(70),
                    I32(80),
                    I32(90),
                ]
                .into(),
            )
            .into(),
        ),
        AssertEqual(
            Call(Export("check_nested").into(), [].into()).into(),
            Vector(
                [
                    EmptyVector(TyI32),
                    Vector([I32(1)].into()),
                    Vector([I32(2), I32(3)].into()),
                    Vector([I32(4), I32(5), I32(6)].into()),
                    Vector([I32(7), I32(8), I32(9), I32(10)].into()),
                ]
                .into(),
            )
            .into(),
        ),
    ];

    case
}

test_all!();
