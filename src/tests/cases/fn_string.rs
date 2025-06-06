use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::cell::UnsafeCell;

        // Can't use "Mutex" because it allocates memory and throws off our memory check
        struct Wrapper(UnsafeCell<Option<String>>);
        unsafe impl Sync for Wrapper {}
        static STRING: Wrapper = Wrapper(UnsafeCell::new(None));

        // This prepares for the memory leak check at the end
        pub fn reset() {
            unsafe { *STRING.0.get() = None; }
        }

        pub fn get_string_len() -> i32 {
            let x = unsafe { (*STRING.0.get()).as_ref() };
            x.map(|x| x.len()).unwrap_or(0) as i32
        }

        pub fn get_string() -> String {
            let x = unsafe { (*STRING.0.get()).as_ref() };
            x.map(|x| x.as_str()).unwrap_or("").to_string()
        }

        pub fn set_string(x: String) {
            unsafe { *STRING.0.get() = Some(x); }
        }

        pub fn set_str(x: &str) {
            unsafe { *STRING.0.get() = Some(x.to_string()); }
        }
    "#;

    case.checks = vec![
        AssertEqual(
            Call(Export("get_string_len").into(), [].into()).into(),
            I32(0).into(),
        ),
        AssertEqual(
            Call(Export("get_string").into(), [].into()).into(),
            Str("").into(),
        ),
        Call(Export("set_string").into(), [Str("abc")].into()).into(),
        AssertEqual(
            Call(Export("get_string_len").into(), [].into()).into(),
            I32(3).into(),
        ),
        AssertEqual(
            Call(Export("get_string").into(), [].into()).into(),
            Str("abc").into(),
        ),
        Call(Export("set_string").into(), [Str("\0\r\n\u{1f980}")].into()),
        AssertEqual(
            Call(Export("get_string_len").into(), [].into()).into(),
            I32(7).into(),
        ),
        AssertEqual(
            Call(Export("get_string").into(), [].into()).into(),
            Str("\0\r\n\u{1f980}").into(),
        ),
        Call(Export("set_str").into(), [Str("<<<\0>>>")].into()),
        AssertEqual(
            Call(Export("get_string_len").into(), [].into()).into(),
            I32(7).into(),
        ),
        AssertEqual(
            Call(Export("get_string").into(), [].into()).into(),
            Str("<<<\0>>>").into(),
        ),
        Call(Export("reset").into(), [].into()),
    ];

    case
}

test_all!();
