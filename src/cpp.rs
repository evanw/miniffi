use super::*;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

/// Use this target when the host language is C++.
///
/// This needs to generate a source file (`.cpp`, `.cc`, etc.) and a header
/// file (`.hpp`, `.h`, etc.). By default, the files will be named
/// `miniffi.cpp` and `miniffi.h` and will be written to the directory
/// containing your `Cargo.toml` file. You can customize these paths before
/// calling [`build`](Target::build):
///
/// ```no_run
/// use miniffi::*;
///
/// fn main() {
///     CppTarget::new()
///         .write_source_to("../app/rust.cpp")
///         .write_header_to("../app/rust.h")
///         .build();
/// }
/// ```
///
/// Generated bindings for calling your Rust code from C++ will be placed in
/// the `rust` namespace. For example, if your `src/lib.rs` looks like this:
///
/// ```no_run
/// pub fn add(left: u64, right: u64) -> u64 {
///     left + right
/// }
///
/// # macro_rules! env { ($a:expr) => { $a } }
/// # macro_rules! include { ($a:expr) => { $a } }
/// include!(concat!(env!("OUT_DIR"), "/miniffi.rs"));
/// ```
///
/// You can call that from C++ like this:
///
/// ```c++
/// #include <iostream>
/// #include "miniffi.h"
///
/// int main() {
///     std::cout << "1 + 2 = " << rust::add(1, 2) << std::endl;
///     return 0;
/// }
/// ```
///
/// The generated code tries to map Rust types into the corresponding types
/// from the C++ standard template library:
///
/// | Rust type   | C++ type                             |
/// |-------------|--------------------------------------|
/// | `String`    | `std::string`                        |
/// | `Vec<T>`    | `std::vector<T>`                     |
/// | `(A, B)`    | `std::tuple<A, B>` *(since C++11)*   |
/// | `Box<T>`    | `std::unique_ptr<T>` *(since C++11)* |
/// | `Rc<T>`     | `std::shared_ptr<T>` *(since C++11)* |
/// | `Option<T>` | `std::optional<T>` *(since C++17)*   |
///
/// Enums without fields are turned into a C++ enum struct:
///
/// <table>
/// <tr><th>Rust</th><th>C++</th></tr>
/// <tr><td valign=top>
///
/// ```
/// // Exported enum in Rust
/// pub enum Color {
///     Window,
///     Text,
/// }
///
/// // Example usage
/// let color = Color::Text;
/// let hex = match color {
///     Color::Window => 0x353535,
///     Color::Text => 0xD2991D,
/// };
/// ```
///
/// </td><td valign=top>
///
/// ```c++
/// // Generated enum in C++
/// enum struct Color : int32_t {
///     Window = 0,
///     Text = 1,
/// };
///
/// // Example usage
/// auto color = Color::Text;
/// int hex;
/// if (color == Color::Window)
///     hex = 0x353535;
/// else if (color == Color::Text)
///     hex = 0xD2991D;
/// else
///     std::abort();
/// ```
///
/// </td></tr>
/// </table>
///
/// Enums with fields use `std::variant` from C++17:
///
/// <table>
/// <tr><th>Rust</th><th>C++</th></tr>
/// <tr><td valign=top>
///
/// ```
/// // Exported enum in Rust
/// pub enum Color {
///     Window,
///     Text,
///     RGB(u8, u8, u8),
/// }
///
/// // Example usage
/// let color = Color::RGB(255, 0, 0);
/// let hex = match color {
///     Color::Window => 0x353535,
///     Color::Text => 0xD2991D,
///     Color::RGB(r, g, b) =>
///         u32::from_le_bytes([b, g, r, 0]),
/// };
/// ```
///
/// </td><td valign=top>
///
/// ```c++
/// // Generated enum in C++ (approximately)
/// struct Color : std::variant<...> {
///     struct Window {};
///     struct Text {};
///     struct RGB { uint8_t _0, _1, _2; };
///     template <typename T> bool is();
///     template <typename T> T* as();
/// };
///
/// // Example usage
/// Color color{ Color::RGB{ 255, 0, 0 } };
/// int hex;
/// if (color.is<Color::Window>())
///     hex = 0x353535;
/// else if (color.is<Color::Text>())
///     hex = 0xD2991D;
/// else if (auto x = color.as<Color::RGB>())
///     hex = (x->_0 << 16) | (x->_1 << 8) | x->_2;
/// else
///     std::abort();
/// ```
///
/// </td></tr>
/// </table>
///
/// ## Using the command line
///
/// If you are using C++ from the command line, then you can call Rust from
/// C++ by compiling the generated `.cpp` file and linking that along with the
/// compiled Rust code to the rest of your C++. For example:
///
/// ```text
/// # Build the Rust code
/// cargo rustc --crate-type=staticlib
///
/// # Build the C++ code
/// c++ -std=c++17 main.cpp miniffi.cpp ./target/debug/libexample.a
/// ```
pub struct CppTarget {
    namespace: String,
    source_path: PathBuf,
    header_path: PathBuf,
}

impl CppTarget {
    pub fn new() -> CppTarget {
        CppTarget {
            namespace: "rust".to_string(),
            source_path: "miniffi.cpp".into(),
            header_path: "miniffi.h".into(),
        }
    }

    pub fn write_source_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.source_path = path.into();
        self
    }

    pub fn write_header_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.header_path = path.into();
        self
    }
}

pub const CPP_KEYWORDS: &[&str] = &[
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "atomic_cancel",
    "atomic_commit",
    "atomic_noexcept",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char8_t",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "concept",
    "const",
    "consteval",
    "constexpr",
    "constinit",
    "const_cast",
    "continue",
    "contract_assert",
    "co_await",
    "co_return",
    "co_yield",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "reflexpr",
    "register",
    "reinterpret_cast",
    "requires",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "synchronized",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
];

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum HeaderGroup {
    Include,
    ForwardDecl,
    Other,
    Constants,
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum SourceGroup {
    Include,
    MultiRet,
    ExternCDecl,
    ExternCImpl,
    Anonymous,
    Other,
}

impl Compile for CppTarget {
    fn compile(&self, mut ast: AST, rust_path: PathBuf) -> Vec<FileData> {
        let mut rust_helpers = HelperSet::<(), String>::default();
        let mut source_helpers = HelperSet::<SourceGroup, String>::default();
        let mut header_helpers = HelperSet::<HeaderGroup, String>::default();

        add_common_rust_helpers(&mut rust_helpers);
        ast.rename_keywords(CPP_KEYWORDS);

        for (name, code) in [
            ("<algorithm>", "#include <algorithm>\n"),
            ("<memory>", "#include <memory>\n"),
            ("<optional>", "#include <optional>\n"),
            ("<stdint.h>", "#include <stdint.h>\n"),
            ("<stdlib.h>", "#include <stdlib.h>\n"),
            ("<string_view>", "#include <string_view>\n"),
            ("<string>", "#include <string>\n"),
            ("<tuple>", "#include <tuple>\n"),
            ("<variant>", "#include <variant>\n"),
            ("<vector>", "#include <vector>\n"),
        ] {
            source_helpers.add_group(SourceGroup::Include, name, code);
            header_helpers.add_group(HeaderGroup::Include, name, code);
        }

        source_helpers
            .add_group(
                SourceGroup::ExternCDecl,
                "_ffi_alloc",
                "void* _ffi_alloc(uintptr_t len);\n",
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>");

        source_helpers
            .add_group(
                SourceGroup::ExternCDecl,
                "_ffi_dealloc",
                "void _ffi_dealloc(const void* ptr, uintptr_t capacity);\n",
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>");

        source_helpers
            .add_group(
                SourceGroup::Anonymous,
                "_ffi_read",
                r#"
template <typename T>
T _ffi_read(const uint8_t* &ptr) {
    T val;
    memcpy(&val, ptr, sizeof(T));
    ptr += sizeof(T);
    return val;
}
"#,
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>");

        source_helpers
            .add_group(
                SourceGroup::Anonymous,
                "_ffi_write",
                r#"
template <typename T>
void _ffi_write(T val, std::vector<uint8_t> &buf) {
    buf.insert(buf.end(), (const uint8_t*)&val, (const uint8_t*)&val + sizeof(T));
}
"#,
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>")
            .add_dep_group(SourceGroup::Include, "<vector>");

        source_helpers
            .add_group(
                SourceGroup::Anonymous,
                "_ffi_vec_to_rust",
                r"
const void* _ffi_vec_to_rust(const std::vector<uint8_t>& vec) {
    return memcpy(_ffi_alloc(vec.size()), vec.data(), vec.size());
}
",
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>")
            .add_dep_group(SourceGroup::Include, "<vector>")
            .add_dep_group(SourceGroup::ExternCDecl, "_ffi_alloc");

        source_helpers
            .add_group(
                SourceGroup::Anonymous,
                "_ffi_string_to_rust",
                r"
const void* _ffi_string_to_rust(const std::string& str, uintptr_t &len) {
    len = str.size();
    return memcpy(_ffi_alloc(len), str.data(), len);
}
",
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>")
            .add_dep_group(SourceGroup::Include, "<string>")
            .add_dep_group(SourceGroup::ExternCDecl, "_ffi_alloc");

        source_helpers
            .add_group(
                SourceGroup::Anonymous,
                "_ffi_string_from_rust",
                r"
std::string _ffi_string_from_rust(const char* ptr, uintptr_t len, uintptr_t cap) {
    std::string str(ptr, len);
    _ffi_dealloc(ptr, cap);
    return str;
}
",
            )
            .add_dep_group(SourceGroup::Include, "<stdint.h>")
            .add_dep_group(SourceGroup::Include, "<string>")
            .add_dep_group(SourceGroup::ExternCDecl, "_ffi_dealloc");

        // Forward declarations
        for t in &ast.traits {
            header_helpers
                .add_group(
                    HeaderGroup::ForwardDecl,
                    &t.name,
                    format!("struct {};\n", t.name),
                )
                .set_is_forward_decl();
        }
        for s in &ast.structs {
            header_helpers
                .add_group(
                    HeaderGroup::ForwardDecl,
                    &s.name,
                    format!("struct {};\n", s.name),
                )
                .set_is_forward_decl();
        }
        for e in &ast.enums {
            header_helpers
                .add_group(
                    HeaderGroup::ForwardDecl,
                    &e.name,
                    format!("struct {};\n", e.name),
                )
                .set_is_forward_decl();
        }

        // Traits
        for t in &ast.traits {
            let mut deps = HashSet::new();
            let mut code = String::new();
            _ = write!(code, "\nstruct {} {{\n", t.name);
            _ = write!(code, "    virtual ~{}() {{}}\n", t.name);
            for f in &t.fns {
                code.push_str("    virtual ");
                CppTypeCtx {
                    cpp: &mut code,
                    ast: &ast,
                    deps: &mut deps,
                    include_group: HeaderGroup::Include,
                    decl_groups: Some(DeclGroups {
                        forward: HeaderGroup::ForwardDecl,
                        full: HeaderGroup::Other,
                    }),
                    loc: TypeLoc::InsideRustNamespace,
                    ns: &self.namespace,
                }
                .append_cpp_signature(
                    f.returns.as_ref().map(|r| &r.ty),
                    &f.name,
                    None,
                    &f.args,
                );
                code.push_str(" = 0;\n");
            }
            code.push_str("};\n");
            header_helpers
                .add_group(HeaderGroup::Other, &t.name, code)
                .add_forward_decl_group(HeaderGroup::ForwardDecl, &t.name)
                .add_deps_group(deps)
                .mark_used();
        }

        // Enums
        for e in &ast.enums {
            let mut code = String::new();
            let mut enum_deps = HashSet::new();
            if !e.has_fields() {
                // Enums without fields are just integers
                _ = write!(code, "\nenum struct {} : int32_t", e.name);
                code.push_str(" {\n");
                for v in &e.variants {
                    if v.discriminant == std::i32::MIN {
                        // Avoid a Visual C++ warning
                        _ = write!(code, "    {} = {} - 1,\n", v.name, v.discriminant + 1);
                    } else {
                        _ = write!(code, "    {} = {},\n", v.name, v.discriminant);
                    }
                }
                code.push_str("};\n");
            } else {
                // Enums with fields map to "std::variant"
                _ = write!(code, "\nstruct {} : std::variant<std::monostate", e.name);
                for v in &e.variants {
                    _ = write!(code, ", detail::{}__{}", e.name, v.name);
                }
                code.push_str("> {\n");
                for v in &e.variants {
                    _ = write!(
                        code,
                        "    using {} = detail::{}__{};\n",
                        v.name, e.name, v.name
                    );
                }
                _ = write!(code, "    using std::variant<std::monostate");
                for v in &e.variants {
                    _ = write!(code, ", {}", v.name);
                }
                code.push_str(">::operator =;\n");
                code.push_str(
                    "    template <typename T> bool is() const { return std::holds_alternative<T>(*this); }\n",
                );
                code.push_str(
                    "    template <typename T> const T* as() const { return std::get_if<T>(this); }\n",
                );
                code.push_str(
                    "    template <typename T> T* as() { return std::get_if<T>(this); }\n",
                );
                code.push_str("};\n");
                enum_deps.insert((HeaderGroup::Include, "<variant>".to_string()));

                // Declarations for the variants need to be written out separately
                let mut detail = String::new();
                let mut detail_deps = HashSet::new();
                let detail_name = format!("detail::{}", e.name);
                _ = write!(detail, "\nnamespace detail {{\n");
                for v in &e.variants {
                    _ = write!(detail, "\nstruct {}__{} {{\n", e.name, v.name);
                    for f in &v.fields {
                        detail.push_str("    ");
                        CppTypeCtx {
                            cpp: &mut detail,
                            ast: &ast,
                            deps: &mut detail_deps,
                            include_group: HeaderGroup::Include,
                            decl_groups: Some(DeclGroups {
                                forward: HeaderGroup::ForwardDecl,
                                full: HeaderGroup::Other,
                            }),
                            loc: TypeLoc::InsideRustNamespace,
                            ns: &self.namespace,
                        }
                        .append_cpp_type(&f.ty, CppDecl::Full);
                        _ = write!(
                            detail,
                            " {}{};\n",
                            with_digit_prefix(&f.name),
                            cpp_struct_field_init(&ast, &f.ty)
                        );
                    }
                    if e.derives_partial_eq {
                        generate_operator_eq(
                            &mut source_helpers,
                            &format!("{}::detail", self.namespace),
                            &format!("{}__{}", e.name, v.name),
                            &v.fields,
                            &mut detail,
                        );
                    }
                    detail.push_str("};\n");
                }
                _ = write!(detail, "\n}} // namespace detail\n");
                header_helpers
                    .add_group(HeaderGroup::Other, &detail_name, detail)
                    .add_deps_group(detail_deps);
                enum_deps.insert((HeaderGroup::Other, detail_name));
            }
            header_helpers
                .add_group(HeaderGroup::Other, &e.name, code)
                .add_deps_group(enum_deps)
                .mark_used();
        }

        // Structs
        for s in &ast.structs {
            let mut deps = HashSet::new();
            let mut code = String::new();
            _ = write!(code, "\nstruct {}", s.name);
            code.push_str(" {\n");
            for f in &s.fields {
                code.push_str("    ");
                CppTypeCtx {
                    cpp: &mut code,
                    ast: &ast,
                    deps: &mut deps,
                    include_group: HeaderGroup::Include,
                    decl_groups: Some(DeclGroups {
                        forward: HeaderGroup::ForwardDecl,
                        full: HeaderGroup::Other,
                    }),
                    loc: TypeLoc::InsideRustNamespace,
                    ns: &self.namespace,
                }
                .append_cpp_type(&f.ty, CppDecl::Full);
                _ = write!(
                    code,
                    " {}{};\n",
                    with_digit_prefix(&f.name),
                    cpp_struct_field_init(&ast, &f.ty)
                );
            }
            if s.derives_partial_eq {
                generate_operator_eq(
                    &mut source_helpers,
                    &self.namespace,
                    &s.name,
                    &s.fields,
                    &mut code,
                );
            }
            code.push_str("};\n");
            header_helpers
                .add_group(HeaderGroup::Other, &s.name, code)
                .add_forward_decl_group(HeaderGroup::ForwardDecl, &s.name)
                .add_deps_group(deps)
                .mark_used();
        }

        // Constants
        for c in &ast.consts {
            let mut deps = HashSet::new();
            let mut code = String::new();
            CppTypeCtx {
                cpp: &mut code,
                ast: &ast,
                deps: &mut deps,
                include_group: HeaderGroup::Include,
                decl_groups: Some(DeclGroups {
                    forward: HeaderGroup::ForwardDecl,
                    full: HeaderGroup::Other,
                }),
                loc: TypeLoc::ForConstant,
                ns: &self.namespace,
            }
            .append_cpp_type(&c.ty, CppDecl::Full);
            _ = write!(code, " const {} = ", c.name);
            append_cpp_val(&mut code, &c.val);
            code.push_str(";\n");
            header_helpers
                .add_group(HeaderGroup::Constants, &c.name, code)
                .add_deps_group(deps)
                .mark_used();
        }

        let mut ctx = CppCtx {
            namespace: self.namespace.clone(),
            rust_helpers,
            source_helpers,
            header_helpers,
            ..CppCtx::default()
        };
        let mut header_fn_decls = String::new();

        // Functions
        for f in &ast.fns {
            generate_cpp_to_rust_fn(&ast, &mut ctx, f, &mut header_fn_decls, None);
        }

        // Determine the path from the source to the header
        let path_to_header = path_relative_from(
            &self.header_path,
            &self.source_path.parent().unwrap_or(Path::new("")),
        );
        let path_to_header = path_to_header.as_ref().unwrap_or(&self.header_path);
        let path_to_header = format!("{:?}", path_to_header.display());

        // Assemble the Rust code
        let mut rust = format!("// {DO_NOT_EDIT_COMMENT}\n");
        for it in ctx.rust_helpers.code_in_order() {
            rust.push_str(it);
        }

        // Assemble the C++ header
        let mut includes = HashSet::new();
        let mut header = format!("// {DO_NOT_EDIT_COMMENT}\n");
        let header_code = ctx.header_helpers.code_by_group_in_order();
        header.push_str("\n#pragma once\n");
        if let Some(code) = header_code.get(&HeaderGroup::Include) {
            header.push('\n');
            for it in code {
                includes.insert(it);
                header.push_str(it);
            }
        }
        header.push_str("\nnamespace rust {\n");
        if let Some(code) = header_code.get(&HeaderGroup::ForwardDecl) {
            header.push('\n');
            for it in code {
                header.push_str(it);
            }
        }
        if let Some(code) = header_code.get(&HeaderGroup::Other) {
            for it in code {
                header.push_str(it);
            }
        }
        if let Some(code) = header_code.get(&HeaderGroup::Constants) {
            header.push('\n');
            for it in code {
                header.push_str(it);
            }
        }
        header.push_str(&header_fn_decls);
        header.push_str("\n} // namespace rust\n");

        // Assemble the C++ source
        let mut source = format!("// {DO_NOT_EDIT_COMMENT}\n");
        let source_code = ctx.source_helpers.code_by_group_in_order();
        _ = write!(source, "\n#include {path_to_header}\n");
        if let Some(code) = source_code.get(&SourceGroup::Include) {
            for it in code {
                if !includes.contains(it) {
                    source.push_str(it);
                }
            }
        }
        if let Some(code) = source_code.get(&SourceGroup::MultiRet) {
            for it in code {
                source.push_str(it);
            }
        }
        if let Some(code) = source_code.get(&SourceGroup::ExternCDecl) {
            source.push_str("\nextern \"C\" {\n\n");
            for it in code {
                source.push_str(it);
            }
            source.push_str("\n} // extern \"C\"\n");
        }
        if let Some(code) = source_code.get(&SourceGroup::Anonymous) {
            source.push_str("\nnamespace {\n");
            for it in code {
                source.push_str(it);
            }
            source.push_str("\n} // namespace\n");
        }
        if let Some(code) = source_code.get(&SourceGroup::ExternCImpl) {
            source.push_str("\nextern \"C\" {\n");
            for it in code {
                source.push_str(it);
            }
            source.push_str("\n} // extern \"C\"\n");
        }
        if let Some(code) = source_code.get(&SourceGroup::Other) {
            for it in code {
                source.push_str(it);
            }
        }

        vec![
            FileData {
                path: rust_path,
                contents: rust,
            },
            FileData {
                path: self.source_path.clone(),
                contents: source,
            },
            FileData {
                path: self.header_path.clone(),
                contents: header,
            },
        ]
    }
}

// C++ struct fields without an initializer are uninitialized, which means they
// start off being random garbage. This can lead to undefined behavior and
// security vulnerabilities. Always initialize all struct fields to avoid that.
fn cpp_struct_field_init(ast: &AST, ty: &RustType) -> &'static str {
    use RustType::*;
    match ty {
        Bool => " = false",
        U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 => " = 0",
        F32 => " = 0.0f",
        F64 => " = 0.0",
        Enum(enum_index) if !ast.enums[*enum_index].has_fields() => "{}",
        _ => "",
    }
}

#[derive(Default)]
struct CppCtx {
    namespace: String,
    helper_names: NameSet,
    rust_helpers: HelperSet<(), String>,
    header_helpers: HelperSet<HeaderGroup, String>,
    source_helpers: HelperSet<SourceGroup, String>,
    multi_ret_helpers: HashMap<Vec<RustType>, String>,
    trait_to_rust_helpers: HashMap<(RustPtr, usize), String>,
    trait_to_cpp_helpers: HashMap<(RustPtr, usize), String>,
    vec_to_rust_helpers: HashMap<RustType, (String, String)>,
    vec_to_cpp_helpers: HashMap<RustType, (String, String)>,
    box_to_rust_helpers: HashMap<RustType, (String, String)>,
    box_to_cpp_helpers: HashMap<RustType, (String, String)>,
    enum_to_rust_helpers: HashMap<usize, (String, String)>,
    enum_to_cpp_helpers: HashMap<usize, (String, String)>,
}

#[derive(Default)]
struct Transform {
    cpp: FnBuilder,
    rust: FnBuilder,
    ffi_args: Vec<RustArg>,
    buf: Option<Rc<SharedBuf>>,
    buf_status: BufStatus,
    buf_ref: &'static str,
}

struct TraitInfo<'a> {
    t: &'a RustTrait,
    kind: RustPtr,
}

impl TraitInfo<'_> {
    fn self_type(&self, ns: &str) -> RustType {
        RustType::Verbatim(match self.kind {
            RustPtr::Box => format!("{ns}::{}*", self.t.name),
            RustPtr::Rc => format!("std::shared_ptr<{ns}::{}>*", self.t.name),
        })
    }

    fn cpp_name(&self) -> String {
        format!("_ffi_{:?}_{}", self.kind, self.t.name)
    }
}

fn generate_cpp_to_rust_fn(
    ast: &AST,
    ctx: &mut CppCtx,
    f: &RustFn,
    header: &mut String,
    trait_info: Option<TraitInfo>,
) {
    let ffi_name = ctx.helper_names.create(&match &trait_info {
        None => format!("_ffi_fn_{}", f.name),
        Some(info) => format!("_ffi_{:?}_{}__{}", info.kind, info.t.name, f.name),
    });
    let mut names = NameSet::default();
    for arg in &f.args {
        names.add(arg.name.clone());
    }
    if let Some(ret) = &f.returns {
        names.add(ret.name.clone());
    }
    let mut source_deps = HashSet::new();

    // Transform the arguments
    let mut arg_tfm = Transform::default();
    if let Some(info) = &trait_info {
        arg_tfm.rust.line(format!(
            "let _self = unsafe {{ &*(_self as *const {}<dyn {}>) }};",
            info.kind.path(),
            info.t.name
        ));
    }
    for arg in &f.args {
        arg_tfm.cpp.mark_pure(&arg.name);
        transform_to_rust(
            ast,
            ctx,
            &mut names,
            &mut arg_tfm,
            &arg.name,
            &arg.ty,
            &mut source_deps,
        );
    }
    arg_tfm.cpp.insert_deferred_lines_here();

    // Generate the Rust call to the FFI function
    let mut rust_call = String::new();
    if trait_info.is_some() {
        rust_call.push_str("_self.");
    }
    _ = write!(rust_call, "{}(", f.name);
    rust_call.push_str(&arg_tfm.rust.find_args(&f.args, RefInline));
    rust_call.push(')');

    // Transform the result
    let mut ret_tfm = Transform::default();
    if let Some(ret) = &f.returns {
        ret_tfm.rust.decl(&ret.name, rust_call);
        transform_to_cpp(
            ast,
            ctx,
            &mut names,
            &mut ret_tfm,
            &ret.name,
            &ret.ty,
            &mut source_deps,
        );
    } else {
        rust_call.push(';');
        ret_tfm.rust.line(rust_call);
    }

    // Generate the C++ call to the FFI function
    let mut cpp_call = format!("{ffi_name}(");
    if trait_info.is_some() {
        cpp_call.push_str("_self");
        if !arg_tfm.ffi_args.is_empty() {
            cpp_call.push_str(", ");
        }
    }
    cpp_call.push_str(&arg_tfm.cpp.find_args(&arg_tfm.ffi_args, RefStdMove));
    cpp_call.push(')');

    // Header
    {
        let mut header_deps = HashSet::new();
        if trait_info.is_none() {
            header.push('\n');
        }
        CppTypeCtx {
            cpp: header,
            ast,
            deps: &mut header_deps,
            include_group: HeaderGroup::Include,
            decl_groups: None,
            loc: match trait_info {
                None => TypeLoc::InsideRustNamespace,
                Some(_) => TypeLoc::OutsideRustNamespace,
            },
            ns: &ctx.namespace,
        }
        .append_cpp_signature(f.returns.as_ref().map(|r| &r.ty), &f.name, None, &f.args);
        header.push_str(";\n");
        ctx.header_helpers.mark_all_used_group(header_deps);
    }

    // Source: Other
    {
        // Handle the return values
        let mut fb = arg_tfm.cpp;
        match &ret_tfm.ffi_args[..] {
            [] => {
                cpp_call.push(';');
                fb.line(cpp_call);
            }
            [arg] => fb.decl(&arg.name, cpp_call),
            _ => {
                let ret = names.create("multi_ret");
                fb.decl(&ret, cpp_call);
                for (i, arg) in ret_tfm.ffi_args.iter().enumerate() {
                    fb.decl(&arg.name, format!("{ret}._{i}"));
                }
            }
        };
        fb.extend(ret_tfm.cpp);
        fb.insert_deferred_lines_here();
        if let Some(ret) = &f.returns {
            let code = fb.find(&ret.name, &ret.ty, RefInline).code;
            fb.line(format!("return {code};"));
        }

        // Write out the final function
        let mut source = String::new();
        let fn_name = &match &trait_info {
            None => format!("{}::{}", ctx.namespace, f.name),
            Some(info) => format!("{}::{}", info.cpp_name(), f.name),
        };
        source.push('\n');
        CppTypeCtx {
            cpp: &mut source,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_signature(f.returns.as_ref().map(|r| &r.ty), fn_name, None, &f.args);
        source.push_str(" {\n");
        fb.write_to_cpp(ctx, ast, &mut source_deps, &mut source, "    ");
        source.push_str("}\n");
        source_deps.insert((SourceGroup::ExternCDecl, ffi_name.clone()));
        ctx.source_helpers
            .add_group(SourceGroup::Other, &fn_name, source)
            .add_deps_group(source_deps)
            .mark_used();
    }

    // Source: ExternCDecl
    {
        let mut source_deps = HashSet::new();
        let mut source = String::new();
        let return_ty = match &ret_tfm.ffi_args[..] {
            [] => None,
            [arg] => Some(Cow::Borrowed(&arg.ty)),
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                source_deps.insert((SourceGroup::MultiRet, ty_name.clone()));
                Some(Cow::Owned(RustType::Verbatim(ty_name)))
            }
        };
        CppTypeCtx {
            cpp: &mut source,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_signature(
            return_ty.as_deref(),
            &ffi_name,
            trait_info
                .as_ref()
                .map(|_| RustArg {
                    name: "_self".into(),
                    ty: RustType::ForeignHandle,
                })
                .as_ref(),
            &arg_tfm.ffi_args,
        );
        source.push_str(";\n");
        ctx.source_helpers
            .add_group(SourceGroup::ExternCDecl, &ffi_name, source)
            .add_deps_group(source_deps);
    }

    // Rust
    {
        // Handle the return values
        let mut fb = arg_tfm.rust;
        fb.extend(ret_tfm.rust);
        fb.insert_deferred_lines_here();
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                let code = fb.find(&arg.name, &arg.ty, RefInline).code;
                fb.line(code);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                let args = fb.find_args(&ret_tfm.ffi_args, RefInline);
                fb.line(format!("{ty_name}({args})"));
            }
        }

        // Write out the final function
        let mut rust = String::new();
        rust.push_str("\n#[unsafe(no_mangle)]\n");
        _ = write!(rust, "extern \"C\" fn {ffi_name}(");
        if trait_info.is_some() {
            rust.push_str("_self: *const u8");
        }
        for (i, arg) in arg_tfm.ffi_args.iter().enumerate() {
            if trait_info.is_some() || i > 0 {
                rust.push_str(", ");
            }
            _ = write!(rust, "{}: ", arg.name);
            append_rust_type(&mut rust, ast, &arg.ty);
        }
        rust.push(')');
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                rust.push_str(" -> ");
                append_rust_type(&mut rust, ast, &arg.ty);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(rust, " -> {ty_name}");
            }
        }
        rust.push_str(" {\n");
        fb.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&ffi_name, rust).mark_used();
    }
}

fn generate_rust_to_cpp_fn(
    ast: &AST,
    ctx: &mut CppCtx,
    kind: RustPtr,
    t: &RustTrait,
    f: &RustFn,
    rust: &mut String,
) {
    let ffi_name = ctx
        .helper_names
        .create(&format!("_ffi_cpp_{kind:?}_{}__{}", t.name, f.name));
    let mut names = NameSet::default();
    for arg in &f.args {
        names.add(arg.name.clone());
    }
    if let Some(ret) = &f.returns {
        names.add(ret.name.clone());
    }
    let mut source_deps = HashSet::new();

    // Transform the arguments
    let mut arg_tfm = Transform::default();
    for arg in &f.args {
        arg_tfm.rust.mark_pure(&arg.name);
        transform_to_cpp(
            ast,
            ctx,
            &mut names,
            &mut arg_tfm,
            &arg.name,
            &arg.ty,
            &mut source_deps,
        );
    }
    arg_tfm.rust.insert_deferred_lines_here();

    // Generate the C++ call to the API function
    let mut cpp_call = String::new();
    cpp_call.push_str(match kind {
        RustPtr::Box => "_self",
        RustPtr::Rc => "_self->get()",
    });
    _ = write!(cpp_call, "->{}(", f.name);
    cpp_call.push_str(&arg_tfm.cpp.find_args(&f.args, RefStdMove));
    cpp_call.push(')');

    // Transform the result
    let mut ret_tfm = Transform::default();
    if let Some(ret) = &f.returns {
        ret_tfm.cpp.decl(&ret.name, cpp_call);
        transform_to_rust(
            ast,
            ctx,
            &mut names,
            &mut ret_tfm,
            &ret.name,
            &ret.ty,
            &mut source_deps,
        );
    } else {
        cpp_call.push(';');
        ret_tfm.cpp.line(cpp_call);
    }

    // Generate the Rust call to the FFI function
    let mut rust_call = format!("unsafe {{ {ffi_name}(self.0");
    if !arg_tfm.ffi_args.is_empty() {
        rust_call.push_str(", ");
    }
    rust_call.push_str(&arg_tfm.rust.find_args(&arg_tfm.ffi_args, RefInline));
    rust_call.push_str(") }");

    // Rust
    {
        // Handle the return values
        let mut fb = arg_tfm.rust;
        match &ret_tfm.ffi_args[..] {
            [] => {
                rust_call.push(';');
                fb.line(rust_call);
            }
            [arg] => fb.decl(&arg.name, rust_call),
            _ => {
                let ret = names.create("multi_ret");
                fb.decl(&ret, rust_call);
                for (i, arg) in ret_tfm.ffi_args.iter().enumerate() {
                    fb.decl(&arg.name, format!("{ret}.{i}"));
                }
            }
        };
        fb.extend(ret_tfm.rust);
        fb.insert_deferred_lines_here();
        if let Some(ret) = &f.returns {
            let code = fb.find(&ret.name, &ret.ty, RefInline).code;
            fb.line(code);
        }

        // Write out the final function
        _ = write!(rust, "\n    fn {}(&self", f.name);
        for arg in &f.args {
            _ = write!(rust, ", {}: ", arg.name);
            append_rust_type(rust, ast, &arg.ty);
        }
        rust.push(')');
        if let Some(returns) = &f.returns {
            rust.push_str(" -> ");
            append_rust_type(rust, ast, &returns.ty);
        }
        rust.push_str(" {\n");
        _ = write!(
            rust,
            "        unsafe extern \"C\" {{ fn {ffi_name}(_: *const u8"
        );
        for arg in &arg_tfm.ffi_args {
            _ = write!(rust, ", {}: ", arg.name);
            append_rust_type(rust, ast, &arg.ty);
        }
        match &ret_tfm.ffi_args[..] {
            [] => rust.push(')'),
            [arg] => {
                rust.push_str(") -> ");
                append_rust_type(rust, ast, &arg.ty);
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(rust, ") -> {ty_name}");
            }
        }
        rust.push_str("; }\n");
        fb.write_to_rust(ast, rust, "        ");
        rust.push_str("    }\n");
    }

    // Source: ExternCImpl
    {
        // Handle the return values
        let mut fb = arg_tfm.cpp;
        fb.extend(ret_tfm.cpp);
        fb.insert_deferred_lines_here();
        let return_ty = match &ret_tfm.ffi_args[..] {
            [] => None,
            [arg] => {
                let code = fb.find(&arg.name, &arg.ty, RefInline).code;
                fb.line(format!("return {code};"));
                Some(Cow::Borrowed(&arg.ty))
            }
            _ => {
                let ty_name = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                let args = fb.find_args(&ret_tfm.ffi_args, RefStdMove);
                fb.line(format!("return {ty_name}{{{args}}};"));
                source_deps.insert((SourceGroup::MultiRet, ty_name.clone()));
                Some(Cow::Owned(RustType::Verbatim(ty_name)))
            }
        };

        // Write out the final function
        let mut source = String::new();
        source.push('\n');
        CppTypeCtx {
            cpp: &mut source,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_signature(
            return_ty.as_deref(),
            &ffi_name,
            Some(&RustArg {
                name: "_self".into(),
                ty: TraitInfo { t, kind }.self_type(&ctx.namespace),
            }),
            &arg_tfm.ffi_args,
        );
        source.push_str(" {\n");
        fb.write_to_cpp(ctx, ast, &mut source_deps, &mut source, "    ");
        source.push_str("}\n");
        ctx.source_helpers
            .add_group(SourceGroup::ExternCImpl, &ffi_name, source)
            .add_deps_group(source_deps)
            .mark_used();
    }
}

fn generate_operator_eq(
    source_helpers: &mut HelperSet<SourceGroup, String>,
    ns: &str,
    name: &str,
    fields: &[RustField],
    header: &mut String,
) {
    use RustType::*;

    fn cpp_type_contains_box(ty: &RustType) -> bool {
        match ty {
            Pair { other, .. } => cpp_type_contains_box(other),
            Vector(inner_ty) => cpp_type_contains_box(inner_ty),
            Optional(inner_ty) => cpp_type_contains_box(inner_ty),
            Ptr(kind, inner_ty) if *kind == RustPtr::Box => match &**inner_ty {
                DynTrait(_) => false,
                _ => true,
            },
            Tuple(types) => types.iter().any(cpp_type_contains_box),
            _ => false,
        }
    }

    fn emit_eq(
        parts: &mut Vec<String>,
        ty: &RustType,
        a: &str,
        b: &str,
        source_deps: &mut HashSet<(SourceGroup, String)>,
    ) {
        if !cpp_type_contains_box(ty) {
            parts.push(format!("{a} == {b}"));
            return;
        }

        match ty {
            Pair { other, .. } => emit_eq(parts, other, a, b, source_deps),

            Tuple(types) => {
                for (i, item_ty) in types.iter().enumerate() {
                    let a = format!("std::get<{i}>({a})");
                    let b = format!("std::get<{i}>({b})");
                    emit_eq(parts, item_ty, &a, &b, source_deps);
                }
            }

            Ptr(_, inner_ty) => {
                let a = format!("*{a}");
                let b = format!("*{b}");
                emit_eq(parts, inner_ty, &a, &b, source_deps);
            }

            Optional(inner_ty) => {
                let mut inner_parts = Vec::new();
                let inner_a = format!("*{a}");
                let inner_b = format!("*{b}");
                emit_eq(&mut inner_parts, inner_ty, &inner_a, &inner_b, source_deps);
                parts.push(if inner_parts.is_empty() {
                    format!("!{a} == !{b}")
                } else {
                    format!("({a} && {b} ? {} : !{a} && !{b})", inner_parts.join(" && "))
                });
            }

            Vector(inner_ty) => {
                let mut inner_parts = Vec::new();
                emit_eq(&mut inner_parts, inner_ty, "a", "b", source_deps);
                parts.push(if inner_parts.is_empty() {
                    format!("{a}.size() == {b}.size()")
                } else {
                    source_deps.insert((SourceGroup::Include, "<algorithm>".into())); // For "std::equal"
                    let a = if a.starts_with('*') {
                        &format!("({a})")
                    } else {
                        a
                    };
                    let b = if b.starts_with('*') {
                        &format!("({b})")
                    } else {
                        b
                    };
                    format!(
                        "std::equal({a}.begin(), {a}.end(), {b}.begin(), {b}.end(), \
                        [](const auto& a, const auto& b) {{ return {}; }})",
                        inner_parts.join(" && ")
                    )
                });
            }

            _ => parts.push(format!("{a} == {b}")),
        }
    }

    let mut names = NameSet::default();
    for f in fields {
        names.add(with_digit_prefix(&f.name).to_string());
    }
    let other = names.create(
        &name
            .chars()
            .next()
            .unwrap()
            .to_ascii_lowercase()
            .to_string(),
    );
    let mut parts = Vec::new();
    let mut source_deps = HashSet::new();

    for f in fields {
        let a = with_digit_prefix(&f.name);
        let b = format!("{other}.{a}");
        emit_eq(&mut parts, &f.ty, &a, &b, &mut source_deps);
    }

    if parts.is_empty() {
        _ = write!(
            header,
            "    bool operator == (const {name}&) const {{ return true; }}\n"
        );
    } else {
        let mut source =
            format!("\nbool {ns}::{name}::operator == (const {ns}::{name}& {other}) const {{\n");
        if parts.len() == 1 || parts.iter().map(|x| x.len()).sum::<usize>() < 100 {
            _ = write!(source, "    return {};\n", parts.join(" && "));
        } else {
            source.push_str("    return (");
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    source.push_str(" &&");
                }
                source.push_str("\n        ");
                source.push_str(part);
            }
            source.push_str("\n    );\n");
        }
        source.push_str("}\n");
        _ = write!(header, "    bool operator == (const {name}&) const;\n");
        source_helpers
            .add_group(SourceGroup::Other, &format!("{name}::operator =="), source)
            .add_deps_group(source_deps)
            .mark_used();
    }

    _ = write!(
        header,
        "    bool operator != (const {name}& {other}) const {{ return !(*this == {other}); }}\n"
    );
}

fn transform_to_rust(
    ast: &AST,
    ctx: &mut CppCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    name: &str,
    ty: &RustType,
    source_deps: &mut HashSet<(SourceGroup, String)>,
) {
    use RustType::*;

    fn add_ffi_arg(
        ast: &AST,
        ctx: &mut CppCtx,
        tfm: &mut Transform,
        source_deps: &mut HashSet<(SourceGroup, String)>,
        name: &str,
        ty: &RustType,
    ) {
        match tfm.buf_status {
            BufStatus::Outside => tfm.ffi_args.push(RustArg {
                name: name.to_string(),
                ty: ty.clone(),
            }),
            BufStatus::Inside => {
                let buf = tfm.buf.as_ref().unwrap();

                // C++ (write)
                let code = tfm.cpp.find(name, ty, RefStdMove).code;
                tfm.cpp
                    .line(format!("_ffi_write({code}, {});", buf.buf_name()));
                source_deps.insert((SourceGroup::Anonymous, "_ffi_write".into()));

                // Rust (read)
                let mut rust = "_ffi_read::<".to_string();
                append_rust_type(&mut rust, ast, ty);
                _ = write!(rust, ">({}{})", tfm.buf_ref, buf.end_name());
                tfm.rust.decl(name, rust);
                ctx.rust_helpers.mark_used("_ffi_read");
            }
        }
    }

    match ty {
        Bool | U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 | F32 | F64
        | ForeignHandle => add_ffi_arg(ast, ctx, tfm, source_deps, name, ty),

        RefStr | OwnStr => {
            let cpp_code = tfm.cpp.find(name, ty, RefInline).code;
            let ptr_name = names.create(&format!("{name}_ptr"));
            let len_name = names.create(&format!("{name}_len"));
            let opt_ref = match ty {
                RefStr => "&",
                _ => "",
            };
            tfm.cpp.line(format!("uintptr_t {len_name};"));
            tfm.cpp.line(format!(
                "const void* {ptr_name} = _ffi_string_to_rust({cpp_code}, {len_name});"
            ));
            ctx.source_helpers
                .mark_used_group(SourceGroup::Anonymous, "_ffi_string_to_rust");
            add_ffi_arg(ast, ctx, tfm, source_deps, &ptr_name, &ForeignHandle);
            add_ffi_arg(ast, ctx, tfm, source_deps, &len_name, &Usize);
            let len_code = tfm.rust.find(&len_name, &Usize, RefInline).code;
            let ptr_code = tfm.rust.find(&ptr_name, &ForeignHandle, RefInline).code;
            tfm.rust.decl(
                name,
                format!("{opt_ref}_ffi_string_from_host({ptr_code}, {len_code})"),
            );
            ctx.rust_helpers.mark_used("_ffi_string_from_host");
        }

        Enum(enum_index) => {
            let e = &ast.enums[*enum_index];
            let (cpp_helper, rust_helper) = enum_to_rust_helper(ast, ctx, *enum_index);
            if !e.has_fields() {
                let cpp = tfm.cpp.find(name, ty, RefInline);
                let raw_name = names.create(&format!("{name}_raw"));
                tfm.cpp
                    .maybe_pure_decl(cpp.pure, &raw_name, format!("int32_t({})", cpp.code));
                add_ffi_arg(ast, ctx, tfm, source_deps, &raw_name, &I32);
                tfm.rust.decl(name, format!("{rust_helper}({raw_name})"));
            } else {
                let cpp = tfm.cpp.find(name, ty, RefStdMove);
                let buf = ensure_cpp_buf(ctx, names, tfm, source_deps);
                tfm.cpp
                    .line(format!("{cpp_helper}({}, {});", cpp.code, buf.buf_name()));
                source_deps.insert((SourceGroup::Anonymous, cpp_helper));
                tfm.rust.decl(
                    name,
                    format!("{rust_helper}({}{})", tfm.buf_ref, buf.end_name()),
                );
            }
        }

        Struct(struct_index) => {
            let cpp = tfm.cpp.find(name, ty, RefMany);
            let s = &ast.structs[*struct_index];
            let mut item_names = Vec::new();
            for f in &s.fields {
                let item_name = names.create(&format!("{name}_{}", f.name));
                tfm.cpp.maybe_pure_decl(
                    cpp.pure,
                    &item_name,
                    format!("{}.{}", cpp.code, with_digit_prefix(&f.name)),
                );
                transform_to_rust(ast, ctx, names, tfm, &item_name, &f.ty, source_deps);
                item_names.push(item_name.into());
            }
            if s.fields.is_empty() {
                // Avoid an unused variable warning in C++
                let code = tfm.cpp.find(name, ty, RefInline).code;
                tfm.cpp.line(format!("(void){code};"));
            }
            rust_decl_ctor(&mut tfm.rust, name, &s.name, &s.fields, item_names);
        }

        Tuple(types) => {
            let cpp = tfm.cpp.find(name, ty, RefMany);
            let mut item_args = Vec::new();
            for (i, item_ty) in types.iter().enumerate() {
                let item_name = names.create(&format!("{name}_{i}"));
                tfm.cpp.maybe_pure_decl(
                    cpp.pure,
                    &item_name,
                    format!("std::get<{i}>({})", cpp.code),
                );
                transform_to_rust(ast, ctx, names, tfm, &item_name, &item_ty, source_deps);
                item_args.push(RustArg {
                    name: item_name,
                    ty: item_ty.clone(),
                });
            }
            if types.is_empty() {
                // Avoid an unused variable warning in C++
                let code = tfm.cpp.find(name, ty, RefInline).code;
                tfm.cpp.line(format!("(void){code};"));
            }
            let mut rust_code = tfm.rust.find_args(&item_args, RefInline);
            if types.len() == 1 {
                rust_code.push(',');
            }
            tfm.rust.decl(name, format!("({rust_code})"));
        }

        Ptr(kind, inner_ty) => {
            if let DynTrait(trait_index) = &**inner_ty {
                let cpp_code = tfm.cpp.find(name, ty, RefInline).code;
                let ptr_name = names.create(&format!("{name}_ptr"));
                let rust_helper = trait_to_rust_helper(ast, ctx, *trait_index, *kind);
                let cpp_code = match kind {
                    RustPtr::Box => format!("{cpp_code}.release()"),
                    RustPtr::Rc => format!(
                        "new std::shared_ptr<{}::{}>({cpp_code})",
                        ctx.namespace, ast.traits[*trait_index].name
                    ),
                };
                tfm.cpp.decl(&ptr_name, cpp_code);
                add_ffi_arg(ast, ctx, tfm, source_deps, &ptr_name, &ForeignHandle);
                let ptr_code = tfm.rust.find(&ptr_name, &ForeignHandle, RefInline).code;
                tfm.rust.decl(
                    name,
                    format!("{}::new({rust_helper}({ptr_code}))", kind.path()),
                );
            } else if *kind == RustPtr::Box {
                let cpp_code = tfm.cpp.find(name, ty, RefMany).code;
                let (cpp_helper, rust_helper) = box_to_rust_helper(ast, ctx, inner_ty);
                let buf = ensure_cpp_buf(ctx, names, tfm, source_deps);
                tfm.cpp.line(format!(
                    "{cpp_helper}(std::move(*{cpp_code}), {});",
                    buf.buf_name()
                ));
                source_deps.insert((SourceGroup::Anonymous, cpp_helper));
                tfm.rust.decl(
                    name,
                    format!("{rust_helper}({}{})", tfm.buf_ref, buf.end_name()),
                );
            } else {
                unreachable!()
            }
        }

        Vector(inner_ty) => {
            let cpp_code = tfm.cpp.find(name, ty, RefMany).code;
            let len_name = names.create(&format!("{name}_len"));
            let (cpp_helper, rust_helper) = vec_to_rust_helper(ast, ctx, inner_ty);
            let buf = ensure_cpp_buf(ctx, names, tfm, source_deps);
            tfm.cpp.decl(&len_name, format!("{cpp_code}.size()"));
            add_ffi_arg(ast, ctx, tfm, source_deps, &len_name, &Usize);
            tfm.cpp.line(format!(
                "{cpp_helper}(std::move({cpp_code}), {});",
                buf.buf_name()
            ));
            source_deps.insert((SourceGroup::Anonymous, cpp_helper));
            let len_code = tfm.rust.find(&len_name, ty, RefInline).code;
            tfm.rust.decl(
                name,
                format!(
                    "{rust_helper}({len_code}, {}{})",
                    tfm.buf_ref,
                    buf.end_name()
                ),
            );
        }

        Optional(inner_ty) => {
            let cpp_code = tfm.cpp.find(name, ty, RefMany).code;
            let has_name = names.create(&format!("has_{name}"));
            let val_name = names.create(&format!("{name}_val"));
            ensure_cpp_buf(ctx, names, tfm, source_deps);

            tfm.cpp.decl(&has_name, format!("{cpp_code}.has_value()"));
            add_ffi_arg(ast, ctx, tfm, source_deps, &has_name, &Bool);

            let mut rust = FnBuilder::default();
            tfm.cpp.line(format!("if ({has_name}) {{"));
            tfm.cpp.pure_decl(&val_name, format!("{cpp_code}.value()"));
            {
                let old = tfm.buf_status;
                tfm.buf_status = BufStatus::Inside;
                std::mem::swap(&mut tfm.rust, &mut rust);
                transform_to_rust(ast, ctx, names, tfm, &val_name, inner_ty, source_deps);
                std::mem::swap(&mut tfm.rust, &mut rust);
                tfm.buf_status = old;
            }
            tfm.cpp.line("}".into());

            let has_code = tfm.rust.find(&has_name, ty, RefInline).code;
            let val_code = rust.find(&val_name, ty, RefInline).code;
            if rust.is_empty() {
                tfm.rust
                    .decl(name, format!("{has_code}.then(|| {val_code})"));
            } else {
                rust.insert_deferred_lines_here();
                rust.line(val_code);
                tfm.rust.line(format!("let {name} = {has_code}.then(|| {{"));
                tfm.rust.extend(rust);
                tfm.rust.line("});".to_string());
            }
        }

        Pair { .. } | Verbatim(_) | DynTrait(_) => unreachable!(),
    }
}

fn transform_to_cpp(
    ast: &AST,
    ctx: &mut CppCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    name: &str,
    ty: &RustType,
    source_deps: &mut HashSet<(SourceGroup, String)>,
) {
    use RustType::*;

    fn add_ffi_arg(
        ast: &AST,
        ctx: &mut CppCtx,
        tfm: &mut Transform,
        source_deps: &mut HashSet<(SourceGroup, String)>,
        name: &str,
        ty: &RustType,
    ) {
        match tfm.buf_status {
            BufStatus::Outside => tfm.ffi_args.push(RustArg {
                name: name.to_string(),
                ty: ty.clone(),
            }),
            BufStatus::Inside => {
                let buf = tfm.buf.as_ref().unwrap();

                // Rust (write)
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!(
                    "_ffi_write({code}, {}{});",
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                ctx.rust_helpers.mark_used("_ffi_write");

                // C++ (read)
                let mut source = "_ffi_read<".to_string();
                CppTypeCtx {
                    cpp: &mut source,
                    ast,
                    deps: source_deps,
                    include_group: SourceGroup::Include,
                    decl_groups: None,
                    loc: TypeLoc::OutsideRustNamespace,
                    ns: &ctx.namespace,
                }
                .append_cpp_type(ty, CppDecl::Full);
                _ = write!(source, ">({})", buf.end_name());
                tfm.cpp.decl(name, source);
                source_deps.insert((SourceGroup::Anonymous, "_ffi_read".into()));
            }
        }
    }

    match ty {
        Bool | U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 | F32 | F64
        | ForeignHandle => add_ffi_arg(ast, ctx, tfm, source_deps, name, ty),

        RefStr | OwnStr => {
            let mut rust_code = tfm.rust.find(name, ty, RefInline).code;
            let ptr_name = names.create(&format!("{name}_ptr"));
            let len_name = names.create(&format!("{name}_len"));
            let cap_name = names.create(&format!("{name}_cap"));
            let ptr_ty = Pair {
                rust: ForeignHandle.into(),
                other: Verbatim("const char*".into()).into(),
            };
            if let RefStr = ty {
                rust_code.push_str(".into()");
            }
            tfm.rust.line(format!(
                "let ({ptr_name}, {len_name}, {cap_name}) = _ffi_string_to_host({rust_code});"
            ));
            ctx.rust_helpers.mark_used("_ffi_string_to_host");
            add_ffi_arg(ast, ctx, tfm, source_deps, &ptr_name, &ptr_ty);
            add_ffi_arg(ast, ctx, tfm, source_deps, &len_name, &Usize);
            add_ffi_arg(ast, ctx, tfm, source_deps, &cap_name, &Usize);
            tfm.cpp.decl(
                name,
                format!("_ffi_string_from_rust({ptr_name}, {len_name}, {cap_name})"),
            );
            source_deps.insert((SourceGroup::Anonymous, "_ffi_string_from_rust".into()));
        }

        Enum(enum_index) => {
            let rust = tfm.rust.find(name, ty, RefInline);
            let e = &ast.enums[*enum_index];
            if !e.has_fields() {
                let raw_name = names.create(&format!("{name}_raw"));
                tfm.rust
                    .maybe_pure_decl(rust.pure, &raw_name, format!("{} as i32", rust.code));
                add_ffi_arg(ast, ctx, tfm, source_deps, &raw_name, &I32);
                tfm.cpp
                    .decl(name, format!("{}::{}({raw_name})", ctx.namespace, e.name));
            } else {
                let (cpp_helper, rust_helper) = enum_to_cpp_helper(ast, ctx, *enum_index);
                let buf = ensure_rust_buf(ctx, names, tfm, source_deps);
                tfm.rust.line(format!(
                    "{rust_helper}({}, {}{});",
                    rust.code,
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                tfm.cpp
                    .decl(name, format!("{cpp_helper}({})", buf.end_name()));
                source_deps.insert((SourceGroup::Anonymous, cpp_helper));
            }
        }

        Struct(struct_index) => {
            let rust = tfm.rust.find(name, ty, RefMany);
            let s = &ast.structs[*struct_index];
            let mut item_args = Vec::new();
            for f in &s.fields {
                let item_name = names.create(&format!("{name}_{}", f.name));
                tfm.rust.maybe_pure_decl(
                    rust.pure,
                    &item_name,
                    format!("{}.{}", rust.code, f.name),
                );
                transform_to_cpp(ast, ctx, names, tfm, &item_name, &f.ty, source_deps);
                item_args.push(RustArg {
                    name: item_name,
                    ty: f.ty.clone(),
                });
            }
            if s.fields.is_empty() {
                // Avoid an unused variable warning in Rust
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!("_ = {code};"));
            }
            let fields = tfm.cpp.find_args(&item_args, RefStdMove);
            tfm.cpp
                .decl(name, format!("{}::{}{{{fields}}}", ctx.namespace, s.name));
        }

        Tuple(types) => {
            let rust = tfm.rust.find(name, ty, RefMany);
            let mut item_args = Vec::new();
            for (i, item_ty) in types.iter().enumerate() {
                let item_name = names.create(&format!("{name}_{i}"));
                tfm.rust
                    .maybe_pure_decl(rust.pure, &item_name, format!("{}.{i}", rust.code));
                transform_to_cpp(ast, ctx, names, tfm, &item_name, &item_ty, source_deps);
                item_args.push(RustArg {
                    name: item_name,
                    ty: item_ty.clone(),
                });
            }
            if types.is_empty() {
                tfm.cpp.decl(name, "std::tuple<>()".to_string());

                // Avoid an unused variable warning in Rust
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!("_ = {code};"));
            } else {
                let cpp_code = tfm.cpp.find_args(&item_args, RefStdMove);
                tfm.cpp.decl(
                    name,
                    match types.len() {
                        1 => cpp_code,
                        _ => format!("std::make_tuple({cpp_code})"),
                    },
                );
            }
        }

        Ptr(kind, inner_ty) => {
            if let DynTrait(trait_index) = &**inner_ty {
                let rust_code = tfm.rust.find(name, ty, RefInline).code;
                let ptr_name = names.create(&format!("{name}_ptr"));
                let cpp_helper = trait_to_cpp_helper(ast, ctx, *trait_index, *kind);
                tfm.rust.decl(
                    &ptr_name,
                    format!("Box::into_raw(Box::new({rust_code})) as *const u8"),
                );
                add_ffi_arg(ast, ctx, tfm, source_deps, &ptr_name, &ForeignHandle);
                tfm.cpp.decl(
                    name,
                    match kind {
                        RustPtr::Box => {
                            let mut cpp_type = String::new();
                            CppTypeCtx {
                                cpp: &mut cpp_type,
                                ast,
                                deps: source_deps,
                                include_group: SourceGroup::Include,
                                decl_groups: None,
                                loc: TypeLoc::OutsideRustNamespace,
                                ns: &ctx.namespace,
                            }
                            .append_cpp_type(inner_ty, CppDecl::Forward);
                            format!("std::unique_ptr<{cpp_type}>(new {cpp_helper}({ptr_name}))")
                        }
                        RustPtr::Rc => format!("std::make_shared<{cpp_helper}>({ptr_name})"),
                    },
                );
            } else if *kind == RustPtr::Box {
                let rust_code = tfm.rust.find(name, ty, RefMany).code;
                let (cpp_helper, rust_helper) = box_to_cpp_helper(ast, ctx, inner_ty);
                let buf = ensure_rust_buf(ctx, names, tfm, source_deps);
                tfm.rust.line(format!(
                    "{rust_helper}(*{rust_code}, {}{});",
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                tfm.cpp
                    .decl(name, format!("{cpp_helper}({})", buf.end_name()));
                source_deps.insert((SourceGroup::Anonymous, cpp_helper));
            } else {
                unreachable!()
            }
        }

        Vector(inner_ty) => {
            let rust_code = tfm.rust.find(name, ty, RefMany).code;
            let len_name = names.create(&format!("{name}_len"));
            let (cpp_helper, rust_helper) = vec_to_cpp_helper(ast, ctx, inner_ty);
            let buf = ensure_rust_buf(ctx, names, tfm, source_deps);
            tfm.rust.decl(&len_name, format!("{rust_code}.len()"));
            add_ffi_arg(ast, ctx, tfm, source_deps, &len_name, &Usize);
            tfm.rust.line(format!(
                "{rust_helper}({rust_code}, {}{});",
                tfm.buf_ref,
                buf.buf_name()
            ));
            let len_code = tfm.cpp.find(&len_name, ty, RefMany).code;
            tfm.cpp.decl(
                name,
                format!("{cpp_helper}({len_code}, {})", buf.end_name()),
            );
            source_deps.insert((SourceGroup::Anonymous, cpp_helper));
        }

        Optional(inner_ty) => {
            let rust_code = tfm.rust.find(name, ty, RefMany).code;
            let has_name = names.create(&format!("has_{name}"));
            let val_name = names.create(&format!("{name}_val"));
            ensure_rust_buf(ctx, names, tfm, source_deps);

            tfm.rust.decl(&has_name, format!("{rust_code}.is_some()"));
            add_ffi_arg(ast, ctx, tfm, source_deps, &has_name, &Bool);

            let mut cpp = FnBuilder::default();
            tfm.rust
                .line(format!("if let Some({val_name}) = {rust_code} {{"));
            {
                let old = tfm.buf_status;
                tfm.buf_status = BufStatus::Inside;
                std::mem::swap(&mut tfm.cpp, &mut cpp);
                transform_to_cpp(ast, ctx, names, tfm, &val_name, inner_ty, source_deps);
                std::mem::swap(&mut tfm.cpp, &mut cpp);
                tfm.buf_status = old;
            }
            tfm.rust.line("}".into());

            let has_code = tfm.cpp.find(&has_name, ty, RefInline).code;
            let mut val_code = cpp.find(&val_name, ty, RefInline).code;
            val_code = format!("std::make_optional({val_code})");
            if cpp.is_empty() {
                tfm.cpp
                    .decl(name, format!("{has_code} ? {val_code} : std::nullopt"));
            } else {
                let mut cpp_ty = String::new();
                CppTypeCtx {
                    cpp: &mut cpp_ty,
                    ast,
                    deps: source_deps,
                    include_group: SourceGroup::Include,
                    decl_groups: None,
                    loc: TypeLoc::OutsideRustNamespace,
                    ns: &ctx.namespace,
                }
                .append_cpp_type(ty, CppDecl::Full);
                cpp.insert_deferred_lines_here();
                cpp.line(format!("{name} = {val_code};"));
                tfm.cpp.line(format!("{cpp_ty} {name};"));
                tfm.cpp.line(format!("if ({has_code}) {{"));
                tfm.cpp.extend(cpp);
                tfm.cpp.line("}".to_string());
            }
        }

        Pair { .. } | Verbatim(_) | DynTrait(_) => unreachable!(),
    }
}

fn ensure_cpp_buf(
    ctx: &mut CppCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    source_deps: &mut HashSet<(SourceGroup, String)>,
) -> Rc<SharedBuf> {
    if let Some(buf) = &tfm.buf {
        return buf.clone();
    }

    let buf_name = names.create("buf");
    let ptr_name = names.create("buf_ptr");
    let end_name = names.create("buf_end");
    let buf = SharedBuf::new(&buf_name, &end_name);

    // C++ (write)
    tfm.cpp.line(format!("std::vector<uint8_t> {buf_name};"));
    tfm.cpp
        .defer_decl(&ptr_name, format!("_ffi_vec_to_rust({buf_name})"));
    source_deps.insert((SourceGroup::Anonymous, "_ffi_vec_to_rust".into()));

    // Rust (read)
    tfm.rust.line_alt(
        // Avoid a warning about an unnecessarily mutable variable
        format!("let {end_name} = {ptr_name};"),
        format!("let mut {end_name} = {ptr_name};"),
        buf.is_buf_name_used_flag(),
    );
    tfm.rust
        .defer_line(format!("_ffi_buf_from_host({ptr_name}, {end_name});"));
    ctx.rust_helpers.mark_used("_ffi_buf_from_host");

    // FFI
    tfm.buf = Some(buf.clone());
    tfm.buf_ref = "&mut ";
    tfm.ffi_args.push(RustArg {
        name: ptr_name,
        ty: RustType::ForeignHandle,
    });
    buf
}

fn ensure_rust_buf(
    ctx: &mut CppCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    source_deps: &mut HashSet<(SourceGroup, String)>,
) -> Rc<SharedBuf> {
    if let Some(buf) = &tfm.buf {
        return buf.clone();
    }

    let buf_name = names.create("buf");
    let ptr_name = names.create("buf_ptr");
    let end_name = names.create("buf_end");
    let cap_name = names.create("buf_cap");
    let buf = SharedBuf::new(&buf_name, &end_name);

    // Rust (write)
    tfm.rust.line_alt(
        // Avoid a warning about an unnecessarily mutable variable
        format!("let {buf_name} = Vec::<u8>::new();"),
        format!("let mut {buf_name} = Vec::<u8>::new();"),
        buf.is_buf_name_used_flag(),
    );
    tfm.rust.defer_line(format!(
        "let ({ptr_name}, {cap_name}) = _ffi_buf_to_host({buf_name});"
    ));
    ctx.rust_helpers.mark_used("_ffi_buf_to_host");

    // C++ (read)
    tfm.cpp.line_alt(
        "".to_string(),
        format!("auto {end_name} = (const uint8_t*){ptr_name};"),
        buf.is_end_name_used_flag(),
    );
    tfm.cpp
        .defer_line(format!("_ffi_dealloc({ptr_name}, {cap_name});"));
    source_deps.insert((SourceGroup::ExternCDecl, "_ffi_dealloc".into()));

    // FFI
    tfm.buf = Some(buf.clone());
    tfm.buf_ref = "&mut ";
    tfm.ffi_args.push(RustArg {
        name: ptr_name,
        ty: RustType::ForeignHandle,
    });
    tfm.ffi_args.push(RustArg {
        name: cap_name,
        ty: RustType::Usize,
    });
    buf
}

fn vec_to_rust_helper(ast: &AST, ctx: &mut CppCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.vec_to_rust_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_vec_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let cpp_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_cpp"));
    let cpp_name_decl = format!("{cpp_name}[decl]");

    // This must be done first to avoid a stack overflow
    ctx.vec_to_rust_helpers
        .insert(inner_ty.clone(), (cpp_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let vec_name = locals.create("items");
    let item_name = locals.create("item");
    let end_name = locals.create("end");
    let len_name = locals.create("len");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let mut source_deps = HashSet::new();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.cpp.mark_pure(&item_name);
    transform_to_rust(
        ast,
        ctx,
        &mut locals,
        &mut tfm,
        &item_name,
        &inner_ty,
        &mut source_deps,
    );
    let item_code = tfm.rust.find(&item_name, inner_ty, RefInline).code;
    tfm.rust.line(format!("{vec_name}.push({item_code});"));

    // Source (forward declaration)
    let mut cpp_ty = String::new();
    {
        let mut source_deps = HashSet::new();
        CppTypeCtx {
            cpp: &mut cpp_ty,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_type(inner_ty, CppDecl::Full);
        let source = format!(
            "\nvoid {cpp_name}(std::vector<{cpp_ty}>&& {vec_name}, std::vector<uint8_t>& {buf_name});\n"
        );
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name_decl, source)
            .set_is_forward_decl();
    }

    // Source
    {
        let mut source = format!(
            "\nvoid {cpp_name}(std::vector<{cpp_ty}>&& {vec_name}, std::vector<uint8_t>& {buf_name}) {{\n"
        );
        if !tfm.cpp.is_empty() {
            _ = write!(source, "    for (auto&& {item_name} : {vec_name}) {{\n");
            tfm.cpp
                .write_to_cpp(ctx, ast, &mut source_deps, &mut source, "        ");
            source.push_str("    }\n");
        }
        source.push_str("}\n");
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_forward_decl_group(SourceGroup::Anonymous, &cpp_name_decl)
            .add_deps_group(source_deps);
    }

    // Rust
    {
        let mut rust = String::new();
        let mut item_ty = String::new();
        let end_name = buf.final_end_name_for_rust();
        append_rust_type(&mut item_ty, ast, inner_ty);
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({len_name}: usize, {end_name}: &mut *const u8) -> Vec<{item_ty}> {{\n"
        );
        _ = write!(
            rust,
            "    let mut {vec_name} = Vec::<{item_ty}>::with_capacity({len_name});\n"
        );
        _ = write!(rust, "    for _ in 0..{len_name} {{\n");
        tfm.rust.write_to_rust(ast, &mut rust, "        ");
        rust.push_str("    }\n");
        _ = write!(rust, "    {vec_name}\n");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    (cpp_name, rust_name)
}

fn vec_to_cpp_helper(ast: &AST, ctx: &mut CppCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.vec_to_cpp_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_vec_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let cpp_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_cpp"));
    let cpp_name_decl = format!("{cpp_name}[decl]");

    // This must be done first to avoid a stack overflow
    ctx.vec_to_cpp_helpers
        .insert(inner_ty.clone(), (cpp_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let vec_name = locals.create("items");
    let item_name = locals.create("item");
    let end_name = locals.create("end");
    let len_name = locals.create("len");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let mut source_deps = HashSet::new();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.rust.mark_pure(&item_name);
    transform_to_cpp(
        ast,
        ctx,
        &mut locals,
        &mut tfm,
        &item_name,
        &inner_ty,
        &mut source_deps,
    );
    let item_code = tfm.cpp.find(&item_name, inner_ty, RefStdMove).code;
    tfm.cpp
        .line(format!("{vec_name}.emplace_back({item_code});"));

    // Rust
    {
        let mut rust = String::new();
        let buf_name = buf.final_buf_name_for_rust();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(rust, "\nfn {rust_name}({vec_name}: Vec<");
        append_rust_type(&mut rust, ast, inner_ty);
        _ = write!(rust, ">, {buf_name}: &mut Vec<u8>) {{\n");
        if !tfm.rust.is_empty() {
            _ = write!(rust, "    for {item_name} in {vec_name} {{\n");
            tfm.rust.write_to_rust(ast, &mut rust, "        ");
            rust.push_str("    }\n");
        }
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    // Source (forward declaration)
    let mut cpp_ty = String::new();
    {
        let mut source_deps = HashSet::new();
        CppTypeCtx {
            cpp: &mut cpp_ty,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_type(inner_ty, CppDecl::Full);
        let source = format!(
            "\nstd::vector<{cpp_ty}> {cpp_name}(uintptr_t {len_name}, const uint8_t*& {end_name});\n"
        );
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name_decl, source)
            .set_is_forward_decl();
    }

    // Source
    {
        let mut source = String::new();
        _ = write!(
            source,
            "
std::vector<{cpp_ty}> {cpp_name}(uintptr_t {len_name}, const uint8_t*& {end_name}) {{
    std::vector<{cpp_ty}> {vec_name};
    {vec_name}.reserve({len_name});
    while ({vec_name}.size() < {len_name}) {{
"
        );
        tfm.cpp
            .write_to_cpp(ctx, ast, &mut source_deps, &mut source, "        ");
        source.push_str("    }\n");
        _ = write!(source, "    return {vec_name};\n");
        source.push_str("}\n");
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_forward_decl_group(SourceGroup::Anonymous, &cpp_name_decl)
            .add_deps_group(source_deps);
    }

    (cpp_name, rust_name)
}

fn trait_to_rust_helper(ast: &AST, ctx: &mut CppCtx, trait_index: usize, kind: RustPtr) -> String {
    if let Some(result) = ctx.trait_to_rust_helpers.get(&(kind, trait_index)) {
        return result.clone();
    }

    let t = &ast.traits[trait_index];
    let rust_name = format!("_ffi_rs_{kind:?}_{}", t.name);
    let drop_name = format!("_ffi_cpp_drop_{kind:?}_{}", t.name);

    // This must be done first to avoid a stack overflow
    ctx.trait_to_rust_helpers
        .insert((kind, trait_index), rust_name.clone());

    // Source: ExternCImpl
    {
        let mut source = String::new();
        let mut source_deps = HashSet::new();
        source.push('\n');
        CppTypeCtx {
            cpp: &mut source,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_signature(
            None,
            &drop_name,
            None,
            &[RustArg {
                name: "self".to_string(),
                ty: TraitInfo { t, kind }.self_type(&ctx.namespace),
            }],
        );
        source.push_str(" {\n");
        source.push_str("    delete self;\n");
        source.push_str("}\n");
        ctx.source_helpers
            .add_group(SourceGroup::ExternCImpl, &drop_name, source)
            .add_deps_group(source_deps)
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        allow_non_camel_case_types(&mut rust, &rust_name);
        _ = write!(rust, "\nstruct {rust_name}(*const u8);\n");
        _ = write!(rust, "\nimpl Drop for {rust_name} {{\n");
        rust.push_str("    fn drop(&mut self) {\n");
        _ = write!(
            rust,
            "        unsafe extern \"C\" {{ fn {drop_name}(_: *const u8); }}\n"
        );
        _ = write!(rust, "        unsafe {{ {drop_name}(self.0) }};\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        _ = write!(rust, "\nimpl {} for {rust_name} {{", t.name);
        for f in &t.fns {
            generate_rust_to_cpp_fn(ast, ctx, kind, t, f, &mut rust);
        }
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    rust_name
}

fn trait_to_cpp_helper(ast: &AST, ctx: &mut CppCtx, trait_index: usize, kind: RustPtr) -> String {
    if let Some(result) = ctx.trait_to_cpp_helpers.get(&(kind, trait_index)) {
        return result.clone();
    }

    let t = &ast.traits[trait_index];
    let drop_name = format!("_ffi_drop_{kind:?}_{}", t.name);
    let cpp_name = TraitInfo { t, kind }.cpp_name();

    // This must be done first to avoid a stack overflow
    ctx.trait_to_cpp_helpers
        .insert((kind, trait_index), cpp_name.clone());

    // Source: ExternCDecl
    {
        let mut source = String::new();
        let mut source_deps = HashSet::new();
        CppTypeCtx {
            cpp: &mut source,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_signature(
            None,
            &drop_name,
            None,
            &[RustArg {
                name: "ptr".to_string(),
                ty: RustType::ForeignHandle,
            }],
        );
        source.push_str(";\n");
        ctx.source_helpers
            .add_group(SourceGroup::ExternCDecl, &drop_name, source)
            .add_deps_group(source_deps);
    }

    // Source: Anonymous
    {
        let mut source = String::new();
        _ = write!(
            source,
            "\nstruct {cpp_name} final : {}::{} {{\n",
            ctx.namespace, t.name
        );
        _ = write!(
            source,
            "    {cpp_name}(const void* ptr) : _self(ptr) {{}}\n"
        );
        _ = write!(
            source,
            "    virtual ~{cpp_name}() {{ {drop_name}(_self); }}\n"
        );
        for f in &t.fns {
            let info = Some(TraitInfo { t, kind });
            source.push_str("    virtual ");
            generate_cpp_to_rust_fn(ast, ctx, f, &mut source, info);
        }
        source.push_str("    const void* _self;\n");
        source.push_str("};\n");
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_dep_group(SourceGroup::ExternCDecl, &drop_name)
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        rust.push_str("\n#[unsafe(no_mangle)]\n");
        _ = write!(rust, "extern \"C\" fn {drop_name}(ptr: *const u8) {{\n");
        _ = write!(
            rust,
            "    drop(unsafe {{ Box::from_raw(ptr as *mut {}<dyn {}>) }});\n",
            kind.path(),
            t.name
        );
        rust.push_str("}\n");
        ctx.rust_helpers.add(&drop_name, rust).mark_used();
    }

    cpp_name
}

fn box_to_rust_helper(ast: &AST, ctx: &mut CppCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.box_to_rust_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_box_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let cpp_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_cpp"));
    let cpp_name_decl = format!("{cpp_name}[decl]");

    // This must be done first to avoid a stack overflow
    ctx.box_to_rust_helpers
        .insert(inner_ty.clone(), (cpp_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let end_name = locals.create("end");
    let buf_name = locals.create("buf");

    // Transform the value
    let mut tfm = Transform::default();
    let mut source_deps = HashSet::new();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.cpp.mark_pure(&val_name);
    transform_to_rust(
        ast,
        ctx,
        &mut locals,
        &mut tfm,
        &val_name,
        &inner_ty,
        &mut source_deps,
    );
    let val_code = tfm.rust.find(&val_name, inner_ty, RefInline).code;
    tfm.rust.line(format!("Box::new({val_code})"));

    // Source (forward declaration)
    let mut cpp_ty = String::new();
    {
        CppTypeCtx {
            cpp: &mut cpp_ty,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_type(inner_ty, CppDecl::Full);
        ctx.source_helpers
            .add_group(
                SourceGroup::Anonymous,
                &cpp_name_decl,
                format!(
                    "\nvoid {cpp_name}({cpp_ty} {val_name}, std::vector<uint8_t>& {buf_name});\n"
                ),
            )
            .set_is_forward_decl();
    }

    // Source
    {
        let mut source = String::new();
        _ = write!(
            source,
            "\nvoid {cpp_name}({cpp_ty} {val_name}, std::vector<uint8_t>& {buf_name}) {{\n"
        );
        tfm.cpp
            .write_to_cpp(ctx, ast, &mut source_deps, &mut source, "    ");
        source.push_str("}\n");
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_forward_decl_group(SourceGroup::Anonymous, &cpp_name_decl)
            .add_deps_group(source_deps);
    }

    // Rust
    {
        let mut rust = String::new();
        let mut item_ty = String::new();
        let end_name = buf.final_end_name_for_rust();
        append_rust_type(&mut item_ty, ast, inner_ty);
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({end_name}: &mut *const u8) -> Box<{item_ty}> {{\n"
        );
        tfm.rust.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    (cpp_name, rust_name)
}

fn box_to_cpp_helper(ast: &AST, ctx: &mut CppCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.box_to_cpp_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_box_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let cpp_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_cpp"));
    let cpp_name_decl = format!("{cpp_name}[decl]");

    // This must be done first to avoid a stack overflow
    ctx.box_to_cpp_helpers
        .insert(inner_ty.clone(), (cpp_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let end_name = locals.create("end");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let mut source_deps = HashSet::new();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.rust.mark_pure(&val_name);
    transform_to_cpp(
        ast,
        ctx,
        &mut locals,
        &mut tfm,
        &val_name,
        &inner_ty,
        &mut source_deps,
    );
    let val_code = tfm.cpp.find(&val_name, inner_ty, RefStdMove).code;

    // Rust
    {
        let mut rust = String::new();
        let buf_name = buf.final_buf_name_for_rust();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(rust, "\nfn {rust_name}({val_name}: ");
        append_rust_type(&mut rust, ast, inner_ty);
        _ = write!(rust, ", {buf_name}: &mut Vec<u8>) {{\n");
        tfm.rust.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
    }

    // Source (forward declaration)
    let mut cpp_ty = String::new();
    {
        CppTypeCtx {
            cpp: &mut cpp_ty,
            ast,
            deps: &mut source_deps,
            include_group: SourceGroup::Include,
            decl_groups: None,
            loc: TypeLoc::OutsideRustNamespace,
            ns: &ctx.namespace,
        }
        .append_cpp_type(inner_ty, CppDecl::Full);
        ctx.source_helpers
            .add_group(
                SourceGroup::Anonymous,
                &cpp_name_decl,
                format!("\nstd::unique_ptr<{cpp_ty}> {cpp_name}(const uint8_t*& {end_name});\n"),
            )
            .set_is_forward_decl();
    }

    // Source
    {
        let mut source = String::new();
        _ = write!(
            source,
            "\nstd::unique_ptr<{cpp_ty}> {cpp_name}(const uint8_t*& {end_name}) {{\n"
        );
        tfm.cpp
            .write_to_cpp(ctx, ast, &mut source_deps, &mut source, "    ");
        _ = write!(
            source,
            "    return std::make_unique<{cpp_ty}>({val_code});\n"
        );
        source.push_str("}\n");
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_forward_decl_group(SourceGroup::Anonymous, &cpp_name_decl)
            .add_deps_group(source_deps);
    }

    (cpp_name, rust_name)
}

fn enum_to_rust_helper(ast: &AST, ctx: &mut CppCtx, enum_index: usize) -> (String, String) {
    if let Some(result) = ctx.enum_to_rust_helpers.get(&enum_index) {
        return result.clone();
    }

    let e = &ast.enums[enum_index];
    let base_name = format!("_ffi_enum_{}", e.name);
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_cpp"));
    let cpp_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let cpp_name_decl = format!("{cpp_name}[decl]");

    // This must be done first to avoid a stack overflow
    ctx.enum_to_rust_helpers
        .insert(enum_index, (cpp_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");
    let end_name = locals.create("end");
    let it_name = locals.create("it");
    let mut branches = Vec::new();
    let mut source_deps = HashSet::new();

    // Enums without fields are just integers
    if !e.has_fields() {
        let mut rust = String::new();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(rust, "\nfn {rust_name}({val_name}: i32) -> {} {{\n", e.name);
        _ = write!(rust, "    match {val_name} {{\n");
        for v in &e.variants {
            _ = write!(
                rust,
                "        {} => {}::{},\n",
                v.discriminant, e.name, v.name
            );
        }
        rust.push_str("        _ => panic!(),\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
        return (cpp_name, rust_name);
    }

    struct Branch {
        tfm: Transform,
        fields: Vec<String>,
    }

    // Transform all fields for each variant in a separate branch
    for v in &e.variants {
        let mut branch_locals = locals.clone();
        let mut fields = Vec::new();
        let mut tfm = Transform::default();
        let buf = SharedBuf::new(&buf_name, &end_name);
        tfm.buf = Some(buf.clone());
        tfm.buf_status = BufStatus::Inside;
        for f in &v.fields {
            let field_name = branch_locals.create(&name_for_match(&f.name, v.fields.len()));
            tfm.cpp.pure_decl(
                &field_name,
                format!("{it_name}->{}", with_digit_prefix(&f.name)),
            );
            transform_to_rust(
                ast,
                ctx,
                &mut branch_locals,
                &mut tfm,
                &field_name,
                &f.ty,
                &mut source_deps,
            );
            fields.push(field_name);
        }
        rust_decl_ctor(
            &mut tfm.rust,
            &val_name,
            &format!("{}::{}", e.name, v.name),
            &v.fields,
            fields.iter().map(|x| Cow::Borrowed(x.as_str())).collect(),
        );
        branches.push(Branch { tfm, fields });
    }

    // Source (forward declaration)
    {
        ctx.source_helpers
            .add_group(
                SourceGroup::Anonymous,
                &cpp_name_decl,
                format!(
                    "\nvoid {cpp_name}({}::{} {val_name}, std::vector<uint8_t>& {buf_name});\n",
                    ctx.namespace, e.name
                ),
            )
            .set_is_forward_decl();
    }

    // Source
    {
        let mut source = String::new();
        let mut is_first = true;
        _ = write!(
            source,
            "\nvoid {cpp_name}({}::{} {val_name}, std::vector<uint8_t>& {buf_name}) {{",
            ctx.namespace, e.name
        );
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            source.push_str(if is_first { "\n    " } else { " else " });
            is_first = false;
            if branch.fields.is_empty() {
                _ = write!(
                    source,
                    "if ({val_name}.is<{}::{}::{}>()) {{\n",
                    ctx.namespace, e.name, v.name
                );
            } else {
                _ = write!(
                    source,
                    "if (auto {it_name} = {val_name}.as<{}::{}::{}>()) {{\n",
                    ctx.namespace, e.name, v.name
                );
            }
            _ = write!(
                source,
                "        _ffi_write(int32_t({}), {buf_name});\n",
                v.discriminant
            );
            source_deps.insert((SourceGroup::Anonymous, "_ffi_write".into()));
            branch
                .tfm
                .cpp
                .write_to_cpp(ctx, ast, &mut source_deps, &mut source, "        ");
            source.push_str("    }");
        }
        if is_first {
            source.push_str("    abort();\n");
        } else {
            source.push_str(" else {\n        abort();\n    }");
        }
        source_deps.insert((SourceGroup::Include, "<stdlib.h>".into())); // For "abort"
        source.push_str("\n}\n");
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_forward_decl_group(SourceGroup::Anonymous, &cpp_name_decl)
            .add_deps_group(source_deps)
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({end_name}: &mut *const u8) -> {} {{\n",
            e.name
        );
        _ = write!(rust, "    match _ffi_read::<i32>({end_name}) {{\n");
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            _ = write!(rust, "        {} => ", v.discriminant);
            let val_code = branch
                .tfm
                .rust
                .find(&val_name, &RustType::Enum(enum_index), RefInline)
                .code;
            if branch.tfm.rust.is_empty() {
                _ = write!(rust, "{val_code},\n");
                continue;
            }
            rust.push_str("{\n");
            branch
                .tfm
                .rust
                .write_to_rust(ast, &mut rust, "            ");
            _ = write!(rust, "            {val_code}\n");
            rust.push_str("        }\n");
        }
        rust.push_str("        _ => panic!(),\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers
            .add(&rust_name, rust)
            .add_dep("_ffi_read")
            .mark_used();
    }

    (cpp_name, rust_name)
}

fn enum_to_cpp_helper(ast: &AST, ctx: &mut CppCtx, enum_index: usize) -> (String, String) {
    if let Some(result) = ctx.enum_to_cpp_helpers.get(&enum_index) {
        return result.clone();
    }

    let e = &ast.enums[enum_index];
    let base_name = format!("_ffi_enum_{}", e.name);
    let cpp_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_cpp"));
    let cpp_name_decl = format!("{cpp_name}[decl]");

    // This must be done first to avoid a stack overflow
    ctx.enum_to_cpp_helpers
        .insert(enum_index, (cpp_name.clone(), rust_name.clone()));

    struct Branch {
        tfm: Transform,
        fields: Vec<String>,
    }

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");
    let end_name = locals.create("end");
    let mut branches = Vec::new();
    let mut source_deps = HashSet::new();

    // Transform all fields for each variant in a separate branch
    for v in &e.variants {
        let mut branch_locals = locals.clone();
        let mut fields = Vec::new();
        let mut tfm = Transform::default();
        let buf = SharedBuf::new(&buf_name, &end_name);
        tfm.buf = Some(buf.clone());
        tfm.buf_status = BufStatus::Inside;
        for f in &v.fields {
            let field_name = branch_locals.create(&name_for_match(&f.name, v.fields.len()));
            tfm.rust.mark_pure(&field_name);
            transform_to_cpp(
                ast,
                ctx,
                &mut branch_locals,
                &mut tfm,
                &field_name,
                &f.ty,
                &mut source_deps,
            );
            fields.push(field_name);
        }
        let args = tfm.cpp.find_args(
            &v.fields
                .iter()
                .zip(&fields)
                .map(|(f, name)| RustArg {
                    name: name.clone(),
                    ty: f.ty.clone(),
                })
                .collect(),
            RefStdMove,
        );
        tfm.cpp.decl(
            &val_name,
            format!(
                "{}::{}{{{}::{}::{}{{{args}}}}}",
                ctx.namespace, e.name, ctx.namespace, e.name, v.name
            ),
        );
        branches.push(Branch { tfm, fields });
    }

    // Rust
    {
        let mut rust = String::new();
        allow_non_snake_case(&mut rust, &rust_name);
        _ = write!(
            rust,
            "\nfn {rust_name}({val_name}: {}, {buf_name}: &mut Vec<u8>) {{\n",
            e.name
        );
        _ = write!(rust, "    match {val_name} {{\n");
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            _ = write!(rust, "        {}::{}", e.name, v.name);
            if v.fields.iter().any(|f| starts_with_digit(&f.name)) {
                rust.push_str("(");
                for (i, name) in branch.fields.iter().enumerate() {
                    rust.push_str(if i > 0 { ", " } else { "" });
                    rust.push_str(name);
                }
                rust.push_str(")");
            } else if !v.fields.is_empty() {
                rust.push_str(" {");
                for (i, (f, name)) in v.fields.iter().zip(&branch.fields).enumerate() {
                    rust.push_str(if i > 0 { ", " } else { " " });
                    if f.name != *name {
                        _ = write!(rust, "{}: ", f.name);
                    }
                    rust.push_str(name);
                }
                rust.push_str(" }");
            }
            if branch.tfm.rust.is_empty() {
                _ = write!(
                    rust,
                    " => _ffi_write({} as i32, {buf_name}),\n",
                    v.discriminant
                );
            } else {
                rust.push_str(" => {\n");
                _ = write!(
                    rust,
                    "            _ffi_write({} as i32, {buf_name});\n",
                    v.discriminant
                );
                branch
                    .tfm
                    .rust
                    .write_to_rust(ast, &mut rust, "            ");
                rust.push_str("        }\n");
            }
        }
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers
            .add(&rust_name, rust)
            .add_dep("_ffi_write")
            .mark_used();
    }

    // Source (forward declaration)
    {
        ctx.source_helpers
            .add_group(
                SourceGroup::Anonymous,
                &cpp_name_decl,
                format!(
                    "\n{}::{} {cpp_name}(const uint8_t*& {end_name});\n",
                    ctx.namespace, e.name
                ),
            )
            .set_is_forward_decl();
    }

    // Source
    {
        let mut source = String::new();
        _ = write!(
            source,
            "\n{}::{} {cpp_name}(const uint8_t*& {end_name}) {{\n",
            ctx.namespace, e.name
        );
        _ = write!(source, "    switch (_ffi_read<int32_t>({end_name})) {{\n",);
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            _ = write!(source, "    case {}:", v.discriminant);
            let val_code = branch
                .tfm
                .cpp
                .find(&val_name, &RustType::Enum(enum_index), RefInline)
                .code;
            if branch.tfm.cpp.is_empty() {
                _ = write!(source, "\n        return {val_code};\n");
            } else {
                source.push_str(" {\n");
                branch
                    .tfm
                    .cpp
                    .write_to_cpp(ctx, ast, &mut source_deps, &mut source, "        ");
                _ = write!(source, "        return {val_code};\n");
                source.push_str("    }\n");
            }
        }
        source.push_str("    default:\n        abort();\n");
        source_deps.insert((SourceGroup::Include, "<stdlib.h>".into())); // For "abort"
        source.push_str("    }\n");
        source.push_str("}\n");
        source_deps.insert((SourceGroup::Anonymous, "_ffi_read".into()));
        ctx.source_helpers
            .add_group(SourceGroup::Anonymous, &cpp_name, source)
            .add_forward_decl_group(SourceGroup::Anonymous, &cpp_name_decl)
            .add_deps_group(source_deps)
            .mark_used();
    }

    (cpp_name, rust_name)
}

fn multi_ret_helper(ast: &AST, ctx: &mut CppCtx, args: &[RustArg]) -> String {
    let types: Vec<_> = args.iter().map(|arg| arg.ty.clone()).collect();
    if let Some(result) = ctx.multi_ret_helpers.get(&types) {
        return result.clone();
    }

    let mut ty_name = "_ffi_ret_".to_string();
    append_type_name_hints(&mut ty_name, ast, &types);
    let ty_name = ctx.helper_names.create(&ty_name);

    // Source
    {
        let mut source = String::new();
        let mut source_deps = HashSet::new();
        _ = write!(source, "\nstruct {ty_name} {{\n");
        for (i, ty) in types.iter().enumerate() {
            source.push_str("    ");
            CppTypeCtx {
                cpp: &mut source,
                ast,
                deps: &mut source_deps,
                include_group: SourceGroup::Include,
                decl_groups: None,
                loc: TypeLoc::OutsideRustNamespace,
                ns: &ctx.namespace,
            }
            .append_cpp_type(ty, CppDecl::Full);
            _ = write!(source, " _{i};\n");
        }
        source.push_str("};\n");
        ctx.source_helpers
            .add_group(SourceGroup::MultiRet, &ty_name, source)
            .add_deps_group(source_deps)
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        _ = write!(rust, "\n#[repr(C)]\nstruct {ty_name}(");
        for (i, ty) in types.iter().enumerate() {
            if i > 0 {
                rust.push_str(", ");
            }
            append_rust_type(&mut rust, ast, ty);
        }
        rust.push_str(");\n");
        ctx.rust_helpers.add(&ty_name, rust).mark_used();
    }

    ctx.multi_ret_helpers.insert(types, ty_name.clone());
    ty_name
}

// For something like "std::vector<Foo> foo;" C++ just needs a forward
// declaration. The reason is something like this: The member "foo" only holds
// a pointer to "Foo", so the compiler doesn't need detailed information about
// "Foo" such as its size and alignment. We use "CppDecl::Forward" to model
// this, which we switch to when we're inside of "std::vector".
//
// However, for something like "Foo foo;" C++ needs a full declaration. The
// reason is something like this: The member "foo" is stored inline, so the
// compiler needs detailed information about "Foo" such as its size and
// alignment. We use "CppDecl::Full" to model this, which we use for top-level
// struct member declarations.
//
// This distinction can matter for mutually-recursive types. Consider the
// following types "Foo" and "Bar":
//
//     struct Foo {
//          Bar bar;
//     };
//
//     struct Bar {
//          std::vector<Foo> foo;
//     };
//
// We can't compile this by picking a declaration order. We also can't compile
// this by having each one just depend on the forward declaration of the other,
// "Foo" actually depends on the full declaration of "Bar". The end result
// needs to at least have the following things, in order:
//
//     struct Foo;
//
//     struct Bar {
//          std::vector<Foo> foo;
//     };
//
//     struct Foo {
//          Bar bar;
//     };
//
// Isn't C++ great?
#[derive(Clone, Copy)]
enum CppDecl {
    Forward,
    Full,
}

struct DeclGroups<G: Clone + Copy> {
    forward: G,
    full: G,
}

impl<G: Clone + Copy> DeclGroups<G> {
    fn select(&self, decl: CppDecl) -> G {
        match decl {
            CppDecl::Forward => self.forward,
            CppDecl::Full => self.full,
        }
    }
}

#[derive(Eq, PartialEq)]
enum TypeLoc {
    OutsideRustNamespace,
    InsideRustNamespace,
    ForConstant,
}

struct CppTypeCtx<'a, G: Clone + Copy + Eq + Hash> {
    cpp: &'a mut String,
    ast: &'a AST,
    deps: &'a mut HashSet<(G, String)>,
    include_group: G,
    decl_groups: Option<DeclGroups<G>>,
    loc: TypeLoc,
    ns: &'a str,
}

impl<G: Clone + Copy + Eq + Hash> CppTypeCtx<'_, G> {
    fn append_cpp_signature(
        &mut self,
        returns: Option<&RustType>,
        name: &str,
        receiver: Option<&RustArg>,
        args: &[RustArg],
    ) {
        // Emit the return type
        if let Some(returns) = returns {
            self.append_cpp_type(returns, CppDecl::Full);
            self.cpp.push(' ');
        } else {
            self.cpp.push_str("void ");
        }

        // The function name goes in the middle in C
        self.cpp.push_str(name);

        // Emit the arguments
        self.cpp.push('(');
        if let Some(receiver) = receiver {
            self.append_cpp_type(&receiver.ty, CppDecl::Full);
            self.cpp.push(' ');
            self.cpp.push_str(&receiver.name);
        }
        for (i, arg) in args.iter().enumerate() {
            if receiver.is_some() || i > 0 {
                self.cpp.push_str(", ");
            }
            self.append_cpp_type(&arg.ty, CppDecl::Full);
            self.cpp.push(' ');
            self.cpp.push_str(&arg.name);
        }
        self.cpp.push(')');
    }

    fn append_cpp_type(&mut self, ty: &RustType, decl: CppDecl) {
        use RustType::*;
        match ty {
            Pair { other, .. } => self.append_cpp_type(other, decl),
            Verbatim(text) => self.cpp.push_str(text),
            Bool => self.cpp.push_str("bool"),

            U8 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("uint8_t");
            }
            U16 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("uint16_t");
            }
            U32 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("uint32_t");
            }
            Usize => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("uintptr_t");
            }
            U64 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("uint64_t");
            }

            I8 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("int8_t");
            }
            I16 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("int16_t");
            }
            I32 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("int32_t");
            }
            Isize => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("intptr_t");
            }
            I64 => {
                self.deps.insert((self.include_group, "<stdint.h>".into()));
                self.cpp.push_str("int64_t");
            }

            F32 => self.cpp.push_str("float"),
            F64 => self.cpp.push_str("double"),
            ForeignHandle => self.cpp.push_str("const void*"),
            RefStr | OwnStr => {
                if self.loc == TypeLoc::ForConstant {
                    self.deps
                        .insert((self.include_group, "<string_view>".into()));
                    self.cpp.push_str("std::string_view");
                } else {
                    self.deps.insert((self.include_group, "<string>".into()));
                    self.cpp.push_str("std::string");
                }
            }

            Struct(index) => {
                let name = &self.ast.structs[*index].name;
                if let Some(groups) = &self.decl_groups {
                    self.deps.insert((groups.select(decl), name.into()));
                }
                if self.loc == TypeLoc::OutsideRustNamespace {
                    _ = write!(self.cpp, "{}::", self.ns);
                }
                self.cpp.push_str(name);
            }
            Enum(index) => {
                let name = &self.ast.enums[*index].name;
                if let Some(groups) = &self.decl_groups {
                    self.deps.insert((groups.select(decl), name.into()));
                }
                if self.loc == TypeLoc::OutsideRustNamespace {
                    _ = write!(self.cpp, "{}::", self.ns);
                }
                self.cpp.push_str(name);
            }
            DynTrait(index) => {
                let name = &self.ast.traits[*index].name;
                if let Some(groups) = &self.decl_groups {
                    self.deps.insert((groups.select(decl), name.into()));
                }
                if self.loc == TypeLoc::OutsideRustNamespace {
                    _ = write!(self.cpp, "{}::", self.ns);
                }
                self.cpp.push_str(name);
            }

            Tuple(types) => {
                self.deps.insert((self.include_group, "<tuple>".into()));
                self.cpp.push_str("std::tuple<");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.cpp.push_str(", ");
                    }
                    self.append_cpp_type(ty, CppDecl::Full);
                }
                self.cpp.push('>');
            }

            Ptr(kind, inner) => match kind {
                RustPtr::Box => {
                    self.deps.insert((self.include_group, "<memory>".into()));
                    self.cpp.push_str("std::unique_ptr<");
                    self.append_cpp_type(inner, CppDecl::Forward);
                    self.cpp.push('>');
                }
                RustPtr::Rc => {
                    self.deps.insert((self.include_group, "<memory>".into()));
                    self.cpp.push_str("std::shared_ptr<");
                    self.append_cpp_type(inner, CppDecl::Forward);
                    self.cpp.push('>');
                }
            },

            Vector(inner) => {
                self.deps.insert((self.include_group, "<vector>".into()));
                self.cpp.push_str("std::vector<");
                self.append_cpp_type(inner, CppDecl::Forward);
                self.cpp.push('>');
            }

            Optional(inner) => {
                self.deps.insert((self.include_group, "<optional>".into()));
                self.cpp.push_str("std::optional<");
                self.append_cpp_type(inner, CppDecl::Full);
                self.cpp.push('>');
            }
        }
    }
}

fn append_cpp_val(cpp: &mut String, val: &RustVal) {
    use RustVal::*;
    match val {
        Bool(x) => _ = write!(cpp, "{x}"),

        U8(x) => _ = write!(cpp, "{x}u"),
        U16(x) => _ = write!(cpp, "{x}u"),
        U32(x) => _ = write!(cpp, "{x}u"),
        U64(x) => _ = write!(cpp, "{x}ull"),

        I8(x) => _ = write!(cpp, "{x}"),
        I16(x) => _ = write!(cpp, "{x}"),
        I32(x) => {
            if *x == std::i32::MIN {
                // Avoid a Visual C++ warning
                _ = write!(cpp, "{} - 1", x + 1);
            } else {
                _ = write!(cpp, "{x}");
            }
        }
        I64(x) => {
            if *x == std::i64::MIN {
                // Avoid a clang warning
                _ = write!(cpp, "{}ll - 1", x + 1);
            } else {
                _ = write!(cpp, "{x}ll");
            }
        }

        F32(x) => _ = write!(cpp, "{x:?}f"),
        F64(x) => _ = write!(cpp, "{x:?}"),

        Str(x) => {
            cpp.push_str("std::string_view(");
            append_cpp_quoted(cpp, x);
            _ = write!(cpp, ", {})", x.len());
        }
    }
}

fn append_cpp_quoted(cpp: &mut String, text: &str) {
    cpp.push('"');
    for c in text.chars() {
        match c {
            '\t' => cpp.push_str("\\t"),
            '\n' => cpp.push_str("\\n"),
            '\r' => cpp.push_str("\\r"),
            '\\' => cpp.push_str("\\\\"),
            '\"' => cpp.push_str("\\\""),
            c if (c as u32) < 0x20 => {
                _ = write!(cpp, "\\{:03o}", c as u32);
            }
            _ => cpp.push(c),
        }
    }
    cpp.push('"');
}

impl FnBuilder {
    fn write_to_cpp(
        &mut self,
        ctx: &CppCtx,
        ast: &AST,
        source_deps: &mut HashSet<(SourceGroup, String)>,
        out: &mut String,
        base_indent: &str,
    ) {
        let mut indent = 0;
        let mut callback = |line: &str| {
            if line.starts_with(&['}', ']', ')']) {
                indent -= 1;
            }
            _ = write!(out, "{base_indent}{}{line}\n", "    ".repeat(indent));
            if line.ends_with(&['{', '[', '(']) {
                indent += 1;
            }
        };
        for line in self.take_lines() {
            match line {
                Line::Plain(text) => {
                    text.split('\n').for_each(&mut callback);
                }
                Line::PlainAlt(when_false, when_true, flag) => match flag.get() {
                    false if when_false.is_empty() => continue,
                    false => when_false.split('\n').for_each(&mut callback),
                    true => when_true.split('\n').for_each(&mut callback),
                },
                Line::Decl(name, ty, text) => {
                    let mut decl_ty = String::new();
                    match ty {
                        None => decl_ty.push_str("auto"),
                        Some(ty) => CppTypeCtx {
                            cpp: &mut decl_ty,
                            ast,
                            deps: source_deps,
                            include_group: SourceGroup::Include,
                            decl_groups: None,
                            loc: TypeLoc::OutsideRustNamespace,
                            ns: &ctx.namespace,
                        }
                        .append_cpp_type(&ty, CppDecl::Full),
                    }
                    let text = format!("{decl_ty} {name} = {text};");
                    text.split('\n').for_each(&mut callback);
                }
            }
        }
    }
}
