use super::*;

#[test]
fn syntax_error() {
    check_warnings(
        "
        fn main() {
            this is a syntax error
        }
        ",
        &["{MESSAGE}
 --> src/lib.rs:3:18
  |
3 |             this is a syntax error
  |                  ^^
"],
    );
}

#[test]
fn unsupported_variant_discriminant() {
    check_warnings(
        r#"
        pub enum Foo {
            A,
            B = 123,
            C = include!("./foo.txt"),
        }
        "#,
        &[
            r#"unsupported discriminant `include!("./foo.txt")` for variant `C` in enum `Foo`
 --> src/lib.rs:5:17
  |
5 |             C = include!("./foo.txt"),
  |                 ^^^^^^^^^^^^^^^^^^^^^ must be an integer literal
"#,
        ],
    );
}

#[test]
fn unsupported_constant_type() {
    check_warnings(
        "
        pub const FOO: i128 = 0;
        ",
        &["unsupported type `i128` for constant `FOO`
 --> src/lib.rs:2:24
  |
2 |         pub const FOO: i128 = 0;
  |                        ^^^^
"],
    );
}

#[test]
fn unsupported_constant_value() {
    check_warnings(
        "
        pub const FOO: bool = cfg!(unix);
        ",
        &["unsupported value `cfg!(unix)` for constant `FOO`
 --> src/lib.rs:2:31
  |
2 |         pub const FOO: bool = cfg!(unix);
  |                               ^^^^^^^^^^ only literals are supported
"],
    );
}

#[test]
fn unsupported_trait_item() {
    check_warnings(
        r"
        pub trait Foo {
            type Bar = Foo;
        }
        ",
        &[r"unsupported item `type Bar = Foo;` for trait `Foo`
 --> src/lib.rs:3:13
  |
3 |             type Bar = Foo;
  |             ^^^^^^^^^^^^^^^ only functions are supported
"],
    );
}

#[test]
fn unsupported_field_type() {
    check_warnings(
        "
        pub struct Foo(i128);
        pub struct Bar { x: i128 }
        pub enum A { Foo(i128) }
        pub enum B { Bar { x: i128 } }
        ",
        &[
            "unsupported type `i128` for field `0` in struct `Foo`
 --> src/lib.rs:2:24
  |
2 |         pub struct Foo(i128);
  |                        ^^^^
",
            "unsupported type `i128` for field `x` in struct `Bar`
 --> src/lib.rs:3:29
  |
3 |         pub struct Bar { x: i128 }
  |                             ^^^^
",
            "unsupported type `i128` for field `0` in variant `A::Foo`
 --> src/lib.rs:4:26
  |
4 |         pub enum A { Foo(i128) }
  |                          ^^^^
",
            "unsupported type `i128` for field `x` in variant `B::Bar`
 --> src/lib.rs:5:31
  |
5 |         pub enum B { Bar { x: i128 } }
  |                               ^^^^
",
        ],
    );
}

#[test]
fn unsupported_fn_arg_pattern() {
    check_warnings(
        "
        pub fn foo((x, y): (i32, i32)) {}
        ",
        &["unsupported pattern `(x, y)` for function `foo`
 --> src/lib.rs:2:20
  |
2 |         pub fn foo((x, y): (i32, i32)) {}
  |                    ^^^^^^ only identifiers are supported
"],
    );
}

#[test]
fn unsupported_fn_arg_type() {
    check_warnings(
        "
        pub fn foo(x: i128) {}
        ",
        &["unsupported type `i128` for argument `x`
 --> src/lib.rs:2:23
  |
2 |         pub fn foo(x: i128) {}
  |                       ^^^^
"],
    );
}

#[test]
fn unsupported_fn_return_type() {
    check_warnings(
        "
        pub fn foo() -> i128 { 0 }
        ",
        &["unsupported return type `i128` for function `foo`
 --> src/lib.rs:2:25
  |
2 |         pub fn foo() -> i128 { 0 }
  |                         ^^^^
"],
    );
}

#[test]
fn unsupported_fn_receiver() {
    check_warnings(
        "
        pub trait ByVal {
            fn foo(self, x: i32);
        }
        pub trait ByMutRef {
            fn foo(&mut self, x: i32);
        }
        pub trait ByBox {
            fn foo(self: &Box<Self>, x: i32);
        }
        pub trait NoReceiver {
            fn foo(x: i32);
        }
        pub fn foo(&self, x: i32) {}
        ",
        &[
            "unsupported receiver `self` for argument `foo` in trait `ByVal`
 --> src/lib.rs:3:20
  |
3 |             fn foo(self, x: i32);
  |                    ^^^^ use `&self` instead
",
            "unsupported receiver `&mut self` for argument `foo` in trait `ByMutRef`
 --> src/lib.rs:6:20
  |
6 |             fn foo(&mut self, x: i32);
  |                    ^^^^^^^^^ use `&self` instead
",
            "unsupported receiver `self: &Box<Self>` for argument `foo` in trait `ByBox`
 --> src/lib.rs:9:20
  |
9 |             fn foo(self: &Box<Self>, x: i32);
  |                    ^^^^^^^^^^^^^^^^ use `&self` instead
",
            "unsupported function `foo` for trait `NoReceiver`
  --> src/lib.rs:12:16
   |
12 |             fn foo(x: i32);
   |                ^^^ methods on public traits must use `&self`
",
            "unsupported argument `self` for function `foo`
  --> src/lib.rs:14:21
   |
14 |         pub fn foo(&self, x: i32) {}
   |                     ^^^^
",
        ],
    );
}

fn check_warnings(contents: &str, expected_warnings: &[&str]) {
    #[derive(Eq, PartialEq)]
    struct Wrapper(String);
    impl std::fmt::Debug for Wrapper {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!("\n{}", self.0))
        }
    }
    let suffix = r#"include!(concat!(env!("OUT_DIR"), "/miniffi.rs"));"#;
    let input_file = FileData {
        path: "src/lib.rs".parse().unwrap(),
        contents: format!("{contents}{suffix}"),
    };
    let result = NullTarget::new().build_custom(input_file, "miniffi.rs".parse().unwrap());
    assert_eq!(result.errors, Vec::<String>::new());
    assert_eq!(
        result
            .warnings
            .iter()
            .map(|w| Wrapper(w.to_human_string()))
            .collect::<Vec<_>>(),
        expected_warnings
            .iter()
            .enumerate()
            .map(|(i, w)| {
                // Be robust to different versions of the "syn" crate returning different errors
                if w.contains("{MESSAGE}") && result.warnings.len() == expected_warnings.len() {
                    Wrapper(w.replace("{MESSAGE}", &result.warnings[i].message))
                } else {
                    Wrapper(w.to_string())
                }
            })
            .collect::<Vec<_>>()
    );
}
