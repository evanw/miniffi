use super::*;

fn test_case() -> TestCase {
    let mut case = TestCase::default();

    case.rust = r#"
        use std::rc::Rc;
        use std::cell::RefCell;

        pub trait Test {
            fn get_string(&self) -> String;
            fn set_string(&self, x: String);
            fn set_str(&self, x: &str);
        }

        struct TestImpl(RefCell<String>);

        impl Test for TestImpl {
            fn get_string(&self) -> String {
                self.0.borrow().clone()
            }
            fn set_string(&self, x: String) {
                *self.0.borrow_mut() = x;
            }
            fn set_str(&self, x: &str) {
                *self.0.borrow_mut() = x.to_string();
            }
        }

        pub fn get_test() -> Rc<dyn Test> {
            Rc::new(TestImpl(RefCell::new(String::new())))
        }

        pub fn set_test(test: Rc<dyn Test>) -> String {
            let mut foo = test.get_string();
            test.set_string("abc".to_string());
            foo.push_str(&test.get_string());
            test.set_str("xyz");
            foo.push_str(&test.get_string());
            foo
        }
    "#;

    case.js_pre = r#"
        class TestImpl {
            x = ""
            get_string() {
                return this.x
            }
            set_string(x) {
                this.x = x
            }
            set_str(x) {
                this.x = x
            }
        }
    "#;

    case.swift_pre = r#"
        var counter = 0

        class TestImpl : Test {
            var x: String = ""
            init() {
                counter += 1
            }
            deinit {
                counter -= 1
            }
            func get_string() -> String {
                return x
            }
            func set_string(_ x: String) {
                self.x = x
            }
            func set_str(_ x: String) {
                self.x = x
            }
        }
    "#;

    case.cpp_pre = "
        int counter = 0;

        struct TestImpl : rust::Test {
            std::string x;
            TestImpl() {
                counter += 1;
            }
            ~TestImpl() {
                counter -= 1;
            }
            std::string get_string() {
                return x;
            }
            void set_string(std::string x) {
                this->x = std::move(x);
            }
            void set_str(std::string x) {
                this->x = std::move(x);
            }
        };
    ";

    case.checks = vec![
        AssertEqual(
            Call(
                Export("set_test").into(),
                [NewRc(TyName("Test"), "TestImpl", [].into())].into(),
            )
            .into(),
            Str("abcxyz").into(),
        ),
        DeclareImmutableLocal(
            "test_from_rust",
            Call(Export("get_test").into(), [].into()).into(),
        ),
        DeclareMutableLocal(
            "foo",
            CallMethod(LoadLocal("test_from_rust").into(), "get_string", [].into()).into(),
        ),
        CallMethod(
            LoadLocal("test_from_rust").into(),
            "set_string",
            [Str("ABC")].into(),
        ),
        StoreLocal(
            "foo",
            StrConcat(
                LoadLocal("foo").into(),
                CallMethod(LoadLocal("test_from_rust").into(), "get_string", [].into()).into(),
            )
            .into(),
        ),
        CallMethod(
            LoadLocal("test_from_rust").into(),
            "set_string",
            [Str("XYZ")].into(),
        ),
        StoreLocal(
            "foo",
            StrConcat(
                LoadLocal("foo").into(),
                CallMethod(LoadLocal("test_from_rust").into(), "get_string", [].into()).into(),
            )
            .into(),
        ),
        AssertEqual(LoadLocal("foo").into(), Str("ABCXYZ").into()),
    ];

    case.swift_post = r#"
        assert(counter == 0, "\(counter) == \(0)")
    "#;

    case.cpp_post = "
        assert(counter == 0);
    ";

    case
}

test_all!();
