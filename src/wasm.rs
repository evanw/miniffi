use super::*;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Use this target when the host language is JavaScript or TypeScript.
///
/// This can either generate a `.js` file or a `.ts` file depending on how you
/// want to import the API. If you're using a `.js` file, you may also want to
/// generate a `.d.ts` file to improve type checking in your IDE. If no file is
/// explicitly requested, a file named `miniffi.js` will be written to the
/// directory containing your `Cargo.toml` file. You can customize these paths
/// before calling [`build`](Target::build):
///
/// ```no_run
/// use miniffi::*;
///
/// fn main() {
///     if true {
///         // You probably either want both `.js` and `.d.ts`:
///         WasmTarget::new()
///             .write_js_to("../app/rust.js")
///             .write_d_ts_to("../app/rust.d.ts")
///             .build();
///     } else {
///         // Or just a `.ts`:
///         WasmTarget::new()
///             .write_ts_to("../app/rust.ts")
///             .build();
///     }
/// }
/// ```
///
/// If your `src/lib.rs` looks like this:
///
/// ```no_run
/// pub fn add(left: u32, right: u32) -> u32 {
///     left + right
/// }
///
/// # macro_rules! env { ($a:expr) => { $a } }
/// # macro_rules! include { ($a:expr) => { $a } }
/// include!(concat!(env!("OUT_DIR"), "/miniffi.rs"));
/// ```
///
/// You can call that from JavaScript like this:
///
/// ```text
/// import * as rust from "./miniffi.js"
///
/// // First instantiate the WASM module
/// await rust.instantiate(...) // See below
///
/// // Then call your Rust code
/// console.log("1 + 2 =", rust.add(1, 2))
/// ```
///
/// One way to build your Rust code into a `.wasm` module is to use the
/// `cdylib` crate type and the `wasm32-unknown-unknown` cargo target. That can
/// be done from the command line like this:
///
/// ```text
/// cargo rustc --crate-type=cdylib --target=wasm32-unknown-unknown
/// ```
///
/// Instantiating a `.wasm` module may need to work differently depending on
/// the JavaScript environment, so miniffi doesn't attempt to do it for you.
/// How you do it depends on whether you are using node or the browser.
///
/// ## Using from node
///
/// If you're using JavaScript in node, you'll have to load the `.wasm` module
/// using node's file system APIs and call `instantiate`. That might look
/// like this:
///
/// ```js
/// import * as rust from "./miniffi.js"
/// import fs from "fs"
///
/// // First instantiate the WASM module
/// await rust.instantiate(fs.readFileSync(
///     "./target/wasm32-unknown-unknown/debug/example.wasm"))
///
/// // Then call your Rust code
/// console.log("1 + 2 =", rust.add(1, 2))
/// ```
///
/// The `instantiate` function exported by the FFI bindings deliberately
/// mirrors the standard [`WebAssembly.instantiate`][WASM_instantiate] API.
/// However, it's important to use the exported `instantiate` function as it
/// imports additional JavaScript helper code related to the FFI bindings.
///
/// [WASM_instantiate]: https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/instantiate_static
///
/// ## Using from the browser
///
/// If you're using JavaScript in the browser, you'll have to download the
/// `.wasm` module from the server and call `instantiateStreaming`. That
/// might look like this:
///
/// ```html
/// <script type="module">
/// import * as rust from "./miniffi.js"
///
/// // First instantiate the WASM module
/// await rust.instantiateStreaming(fetch(
///     "./target/wasm32-unknown-unknown/debug/example.wasm"))
///
/// // Then call your Rust code
/// console.log("1 + 2 =", rust.add(1, 2))
/// </script>
/// ```
///
/// The `instantiateStreaming` function exported by the FFI bindings
/// deliberately mirrors the standard
/// [`WebAssembly.instantiateStreaming`][WASM_instantiateStreaming] API.
/// However, it's important to use the exported `instantiateStreaming` function
/// as it imports additional JavaScript helper code related to the FFI bindings.
///
/// Also note that the `.wasm` file must be served using a MIME type of
/// `application/wasm` by the server or the browser won't compile it.
///
/// [WASM_instantiateStreaming]: https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/instantiateStreaming_static
///
/// ## Additional notes
///
/// The generated WebAssembly code is written assuming a single-threaded
/// environment, which is normally the case when using WebAssembly. Specifically
/// mutable global variables may be used to pass data across the FFI boundary to
/// avoid allocation overhead. This is different than miniffi's other targets,
/// which allocate temporary buffers for this instead since they need to work in
/// a multi-threaded environment.
pub struct WasmTarget {
    common_options: CommonOptions,
    js_path: Option<PathBuf>,
    ts_path: Option<PathBuf>,
    d_ts_path: Option<PathBuf>,
    set_panic_hook: bool,
}

impl WasmTarget {
    pub fn new() -> WasmTarget {
        WasmTarget {
            common_options: CommonOptions::default(),
            js_path: None,
            ts_path: None,
            d_ts_path: None,
            set_panic_hook: false,
        }
    }

    pub fn write_js_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.js_path = Some(path.into());
        self
    }

    pub fn write_ts_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.ts_path = Some(path.into());
        self
    }

    pub fn write_d_ts_to<T: Into<PathBuf>>(mut self, path: T) -> Self {
        self.d_ts_path = Some(path.into());
        self
    }

    /// Unfortunately Rust's standard library unconditionally deletes all code
    /// that writes to stdout and stderr when building for `wasm32`, which can
    /// make debugging difficult. One difficulty is that panics in Rust will no
    /// longer print any useful error message. Luckily Rust's panic feature can
    /// be customized with [`std::panic::set_hook`], so panic support can be
    /// added back.
    ///
    /// Call this to generate an appropriate panic hook and automatically
    /// register it when the WASM module is instantiated.
    ///
    /// Note that this will throw a JavaScript `Error` object, which will unwind
    /// the stack without Rust being able to catch it using
    /// [`std::panic::catch_unwind`]. However, Rust compiles `panic!()` to
    /// abort when building for `wasm32` anyway, so `catch_unwind` already
    /// doesn't work regardless of whatever this custom panic hook does.
    pub fn set_panic_hook(mut self) -> Self {
        self.set_panic_hook = true;
        self
    }
}

pub const JS_KEYWORDS: &[&str] = &[
    "abstract",
    "await",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "double",
    "else",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "float",
    "for",
    "function",
    "goto",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "int",
    "interface",
    "let",
    "long",
    "native",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "true",
    "try",
    "typeof",
    "undefined",
    "var",
    "void",
    "volatile",
    "while",
    "with",
    "yield",
];

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum JsGroup {
    Let,
    Imports,
    Exports,
    Other,
}

impl Compile for WasmTarget {
    fn common_options(&mut self) -> &mut CommonOptions {
        &mut self.common_options
    }

    fn compile(&self, mut ast: AST, rust_path: PathBuf) -> Vec<FileData> {
        let syntax = RustSyntax::with_edition(self.common_options.edition);
        let mut js_helpers = HelperSet::<JsGroup, JsString>::default();
        let mut rust_helpers = HelperSet::<(), String>::default();

        add_common_rust_helpers(&syntax, &mut rust_helpers);
        ast.rename_keywords(JS_KEYWORDS);

        js_helpers.add_group(
            JsGroup::Exports,
            "_ffi_alloc",
            JsString::from_template("«    _ffi_alloc: (len: number) => number,\n»"),
        );

        js_helpers.add_group(
            JsGroup::Exports,
            "_ffi_dealloc",
            JsString::from_template(
                "«    _ffi_dealloc: (ptr: number, capacity: number) => void,\n»",
            ),
        );

        js_helpers.add_group(
            JsGroup::Exports,
            "_ffi_set_panic_hook",
            JsString::from_template("«    _ffi_set_panic_hook: () => void,\n»"),
        );

        js_helpers.add_group(
            JsGroup::Imports,
            "_ffi_js_drop",
            JsString::from_template(
                "
    _ffi_js_drop(handle«: number»)«: void» {
        _ffi_handles.delete(handle);
    },
",
            ),
        );

        js_helpers.add_group(
            JsGroup::Other,
            "_ffi_WriteBuf",
            JsString::from_template(
                "«
interface _ffi_WriteBuf {
    u8: Uint8Array,
    dv: DataView | null,
    off: number,
}
»",
            ),
        );

        js_helpers.add_group(
            JsGroup::Other,
            "_ffi_ReadBuf",
            JsString::from_template(
                "«
interface _ffi_ReadBuf {
    dv: DataView,
    off: number,
}
»",
            ),
        );

        js_helpers
            .add_group(
                JsGroup::Let,
                "_ffi_new_WriteBuf",
                JsString::from_template(
                    "let _ffi_new_WriteBuf = ()«: _ffi_WriteBuf» => \
                        ({ u8: new Uint8Array(16), dv: null, off: 0 });\n",
                ),
            )
            .add_dep_group(JsGroup::Other, "_ffi_WriteBuf");

        js_helpers
            .add_group(
                JsGroup::Let,
                "_ffi_new_ReadBuf",
                JsString::from_template(
                    "let _ffi_new_ReadBuf = (off«: number»)«: _ffi_ReadBuf» => \
                        ({ dv: _ffi_update_dv(), off });\n",
                ),
            )
            .add_dep_group(JsGroup::Other, "_ffi_ReadBuf")
            .add_dep_group(JsGroup::Other, "_ffi_update_dv");

        js_helpers.add_group(
            JsGroup::Other,
            "_ffi_grow",
            JsString::from_template(
                "
function _ffi_grow(buf«: _ffi_WriteBuf», n«: number»)«: number» {
    let off = buf.off;
    let u8 = buf.u8;
    if (off + n > u8.length) {
        (buf.u8 = new Uint8Array((off + n) << 1)).set(u8);
        buf.dv = null;
    }
    buf.off += n;
    if (!buf.dv) buf.dv = new DataView(buf.u8.buffer);
    return off;
}
",
            ),
        );

        js_helpers
            .add_group(
                JsGroup::Other,
                "_ffi_buf_to_rust",
                JsString::from_template(
                    "
function _ffi_buf_to_rust({ u8, off }«: _ffi_WriteBuf»)«: number» {
    let ptr = _ffi_exports._ffi_alloc(off);
    _ffi_update_u8().set(u8.length > off ? u8.subarray(0, off) : u8, ptr);
    return ptr;
}
",
                ),
            )
            .add_dep_group(JsGroup::Exports, "_ffi_alloc")
            .add_dep_group(JsGroup::Other, "_ffi_update_u8");

        js_helpers.add_group(
            JsGroup::Let,
            "_ffi_next_handle",
            "let _ffi_next_handle = 0;\n",
        );
        js_helpers.add_group(
            JsGroup::Let,
            "_ffi_handles",
            JsString::from_template(
                "let _ffi_handles«: Map<number, any>» = /* @__PURE__ */ new Map;\n",
            ),
        );
        js_helpers.add_group(
            JsGroup::Let,
            "_ffi_encoder",
            "let _ffi_encoder = /* @__PURE__ */ new TextEncoder;\n",
        );
        js_helpers.add_group(
            JsGroup::Let,
            "_ffi_decoder",
            "let _ffi_decoder = /* @__PURE__ */ new TextDecoder;\n",
        );
        js_helpers.add_group(
            JsGroup::Let,
            "_ffi_u8",
            JsString::from_template("let _ffi_u8«: Uint8Array»;\n"),
        );
        js_helpers.add_group(
            JsGroup::Let,
            "_ffi_dv",
            JsString::from_template("let _ffi_dv«: DataView»;\n"),
        );
        js_helpers.add_group(JsGroup::Let, "_ffi_len", "let _ffi_len = 0;\n");

        js_helpers
            .add_group(
                JsGroup::Other,
                "_ffi_handle_alloc",
                JsString::from_template(
                    "
function _ffi_handle_alloc(obj«: any»)«: number» {
    _ffi_handles.set(++_ffi_next_handle, obj);
    return _ffi_next_handle;
}
",
                ),
            )
            .add_dep_group(JsGroup::Let, "_ffi_handles")
            .add_dep_group(JsGroup::Let, "_ffi_next_handle");

        js_helpers
            .add_group(
                JsGroup::Other,
                "_ffi_string_to_rust",
                JsString::from_template(
                    "
function _ffi_string_to_rust(str«: string»)«: number» {
    let buf = _ffi_encoder.encode(str);
    let ptr = _ffi_exports._ffi_alloc(_ffi_len = buf.length);
    _ffi_update_u8().set(buf, ptr);
    return ptr;
}
",
                ),
            )
            .add_dep_group(JsGroup::Let, "_ffi_len")
            .add_dep_group(JsGroup::Let, "_ffi_encoder")
            .add_dep_group(JsGroup::Exports, "_ffi_alloc")
            .add_dep_group(JsGroup::Other, "_ffi_update_u8");

        js_helpers
            .add_group(
                JsGroup::Other,
                "_ffi_string_from_rust",
                JsString::from_template(
                    "
function _ffi_string_from_rust(ptr«: number», len«: number», cap«: number»)«: string» {
    let str = _ffi_decoder.decode(new Uint8Array(_ffi_exports.memory.buffer, ptr, len));
    _ffi_exports._ffi_dealloc(ptr, cap);
    return str;
}
",
                ),
            )
            .add_dep_group(JsGroup::Let, "_ffi_decoder")
            .add_dep_group(JsGroup::Exports, "_ffi_dealloc");

        js_helpers
            .add_group(
                JsGroup::Other,
                "_ffi_update_u8",
                JsString::from_template(
                    "
function _ffi_update_u8()«: Uint8Array» {
    let buffer = _ffi_exports.memory.buffer;
    if (!_ffi_u8 || _ffi_u8.buffer !== buffer) _ffi_u8 = new Uint8Array(buffer);
    return _ffi_u8;
}
",
                ),
            )
            .add_dep_group(JsGroup::Let, "_ffi_u8");

        js_helpers
            .add_group(
                JsGroup::Other,
                "_ffi_update_dv",
                JsString::from_template(
                    "
function _ffi_update_dv()«: DataView» {
    let buffer = _ffi_exports.memory.buffer;
    if (!_ffi_dv || _ffi_dv.buffer !== buffer) _ffi_dv = new DataView(buffer);
    return _ffi_dv;
}
",
                ),
            )
            .add_dep_group(JsGroup::Let, "_ffi_dv");

        js_helpers
            .add_group(
                JsGroup::Imports,
                "_ffi_throw_panic",
                JsString::from_template(
                    r#"
    _ffi_throw_panic(ptr«: number», len«: number»)«: void» {
        let legacy_demangle = (_«: string», t«: string») => {
            let e«: any» = { SP: "@", BP: "*", RF: "&", LT: "<", GT: ">", LP: "(", RP: ")", C: "," }, p«: string[]» = [], i = 3;
            for (let n«: any»; n = /^\d+/.exec(t.slice(i)); )
                p.push(t.slice(i += n[0].length, i += +n[0]).replace(/^_\$/, "$").replace(/\.\./g, "::").replace(/\$(\w|\w\w|u[0-9a-f]+)\$/g,
                    (a, b) => b[0] == "u" ? String.fromCodePoint(parseInt(b.slice(1), 16)) : e[b] || a));
            if (/^h[0-9a-f]+$/.test(p[p.length - 1])) p.pop();
            return p.join("::") + (t[i] == "E" ? t.slice(i + 1).replace(/^\.llvm\.[0-9A-F@]+$/, "") : "");
        };
        let msg = _ffi_decoder.decode(new Uint8Array(_ffi_exports.memory.buffer, ptr, len)), Err«: any» = Error, old = Err.stackTraceLimit;
        Err.stackTraceLimit = 99;
        let err = Err(msg);
        Err.stackTraceLimit = old;
        let stack = (err.stack || "").replace(/\b(?:[\w\-]+[\.\[])*(_ZN[^ @\]]+)/g, legacy_demangle);
        if (stack.startsWith("Error: ")) err.stack = stack;
        else err = Err(msg + "\nstack backtrace:\n" + stack);
        throw err;
    },
"#,
                    ),
            )
            .add_dep_group(JsGroup::Let, "_ffi_decoder");

        rust_helpers.add(
            "_ffi_set_panic_hook",
            format!(
                r#"
{}
extern "C" fn _ffi_set_panic_hook() {{
    #[cfg(target_arch = "wasm32")]
    std::panic::set_hook(Box::new(|info| {{
        let location = info.location().unwrap();
        let thread = std::thread::current();
        let name = thread.name().unwrap_or("<unnamed>");
        let payload = info.payload();
        let payload = if let Some(&s) = payload.downcast_ref::<&'static str>() {{
            s
        }} else if let Some(s) = payload.downcast_ref::<String>() {{
            s.as_str()
        }} else {{
            "Box<dyn Any>"
        }};
        let text = format!("thread '{{name}}' panicked at {{location}}:\n{{payload}}");
        unsafe extern "C" {{
            fn _ffi_throw_panic(ptr: *const u8, len: usize);
        }}
        unsafe {{ _ffi_throw_panic(text.as_ptr(), text.len()) }}
    }}));
}}
"#,
                syntax.unsafe_no_mangle()
            ),
        );

        let js_path =
            if self.js_path.is_none() && self.ts_path.is_none() && self.d_ts_path.is_none() {
                Some("miniffi.js".into())
            } else {
                self.js_path.clone()
            };

        if self.set_panic_hook {
            js_helpers.mark_used_group(JsGroup::Imports, "_ffi_throw_panic");
            rust_helpers.mark_used("_ffi_set_panic_hook");
        }

        let mut rust = format!("// {DO_NOT_EDIT_COMMENT}\n");
        let mut dts = format!("// {DO_NOT_EDIT_COMMENT}\n");
        let mut js = JsString::default();
        _ = write!(js, "// {DO_NOT_EDIT_COMMENT}\n");

        // "instantiate" helper
        dts.push_str(
            "\nexport function instantiate(\
                bytes: BufferSource, \
                imports?: WebAssembly.ModuleImports\
            ): Promise<{ memory: WebAssembly.Memory }>;\n",
        );
        js.push_template(
            "
export async function instantiate(\
    bytes«: BufferSource», \
    imports«?: WebAssembly.ModuleImports»\
)«: Promise<{ memory: WebAssembly.Memory }>» {
    let env = Object.assign({}, imports, _ffi_imports);
    let promise = WebAssembly.instantiate(bytes, { env });
    _ffi_exports = (await promise).instance.exports« as any»;
",
        );
        if self.set_panic_hook {
            js.push_both("    _ffi_exports._ffi_set_panic_hook();\n");
            js_helpers.mark_used_group(JsGroup::Exports, "_ffi_set_panic_hook");
        }
        js.push_both("    return { memory: _ffi_exports.memory };\n}\n");

        // "instantiateStreaming" helper
        dts.push_str(
            "export function instantiateStreaming(\
                source: Response | PromiseLike<Response>, \
                imports?: WebAssembly.ModuleImports\
            ): Promise<{ memory: WebAssembly.Memory }>;\n",
        );
        js.push_template(
            "
export async function instantiateStreaming(\
    source«: Response | PromiseLike<Response>», \
    imports«?: WebAssembly.ModuleImports»\
)«: Promise<{ memory: WebAssembly.Memory }>» {
    let env = Object.assign({}, imports, _ffi_imports);
    let promise = WebAssembly.instantiateStreaming(source, { env });
    _ffi_exports = (await promise).instance.exports« as any»;
",
        );
        if self.set_panic_hook {
            js.push_both("    _ffi_exports._ffi_set_panic_hook();\n");
        }
        js.push_both("    return { memory: _ffi_exports.memory };\n}\n");

        // Traits
        for t in &ast.traits {
            let mut ts = JsString::default();
            ts.push_ts(&format!("\nexport interface {} {{\n", t.name));
            for f in &t.fns {
                ts.push_ts("    ");
                ts.push_ts(&f.name);
                ts.push_ts("(");
                for (i, arg) in f.args.iter().enumerate() {
                    if i > 0 {
                        ts.push_ts(", ");
                    }
                    ts.push_ts(&arg.name);
                    ts.push_ts(": ");
                    append_ts_type(&mut ts, &ast, &arg.ty);
                }
                ts.push_ts(")");
                if let Some(returns) = &f.returns {
                    ts.push_ts(": ");
                    append_ts_type(&mut ts, &ast, &returns.ty);
                } else {
                    ts.push_ts(": void");
                }
                ts.push_ts(";\n");
            }
            ts.push_ts("}\n");
            js.extend_from(&ts);
            dts.push_str(&ts.with_types);
        }

        // Enums
        for e in &ast.enums {
            let mut ts = JsString::default();
            if !e.has_fields() {
                // Enums without fields just map to TypeScript enums (i.e. integers)
                let mut discriminant = 0;
                ts.push_js(&format!("\nexport const {} = {{\n", e.name));
                ts.push_ts(&format!("\nexport const enum {} {{\n", e.name));
                for v in &e.variants {
                    ts.push_js(&format!("    {}: {},\n", v.name, v.discriminant));
                    if v.discriminant == discriminant {
                        ts.push_ts(&format!("    {},\n", v.name));
                    } else {
                        ts.push_ts(&format!("    {} = {},\n", v.name, v.discriminant));
                    }
                    discriminant = v.discriminant.wrapping_add(1);
                }
                ts.push_js("};\n");
                ts.push_ts("}\n");
            } else {
                // Enums with fields map to objects with a special "$" field
                ts.push_ts(&format!("\nexport type {} =\n", e.name));
                for v in &e.variants {
                    ts.push_ts(&format!("    | {{ readonly $: {:?}", v.name));
                    for f in &v.fields {
                        ts.push_ts(&format!(", {}: ", f.name));
                        append_ts_type(&mut ts, &ast, &f.ty);
                    }
                    ts.push_ts(" }\n");
                }
            }
            js.extend_from(&ts);
            dts.push_str(&ts.with_types);
        }

        // Structs
        for s in &ast.structs {
            let mut ts = JsString::default();
            ts.push_ts(&format!("\nexport interface {} {{\n", s.name));
            for f in &s.fields {
                ts.push_ts("    ");
                ts.push_ts(&f.name);
                ts.push_ts(": ");
                append_ts_type(&mut ts, &ast, &f.ty);
                ts.push_ts(",\n");
            }
            ts.push_ts("}\n");
            js.extend_from(&ts);
            dts.push_str(&ts.with_types);
        }

        // Constants
        if !ast.consts.is_empty() {
            js.push_both("\n");
            dts.push('\n');
            for c in &ast.consts {
                let mut ts_type = JsString::default();
                append_ts_type(&mut ts_type, &ast, &c.ty);
                _ = write!(dts, "export const {}: {};\n", c.name, ts_type.with_types);
                js.push_both("export const ");
                js.push_both(&c.name);
                js.push_ts(": ");
                js.extend_from(&ts_type);
                js.push_both(" = ");
                append_js_val(&mut js, &c.val);
                js.push_both(";\n");
            }
        }

        let mut ctx = WasmCtx {
            syntax,
            js_helpers,
            rust_helpers,
            ..WasmCtx::default()
        };

        // Functions
        for (i, f) in ast.fns.iter().enumerate() {
            generate_js_to_rust_fn(&ast, &mut ctx, f, &mut js, None);

            // Also generate a type declaration for the function
            let mut ts = JsString::default();
            if i == 0 {
                ts.push_both("\n");
            }
            _ = write!(ts, "export function {}(", f.name);
            for (j, arg) in f.args.iter().enumerate() {
                if j > 0 {
                    ts.push_ts(", ");
                }
                ts.push_both(&arg.name);
                ts.push_ts(": ");
                append_ts_type(&mut ts, &ast, &arg.ty);
            }
            ts.push_both(")");
            if let Some(returns) = &f.returns {
                ts.push_ts(": ");
                append_ts_type(&mut ts, &ast, &returns.ty);
            } else {
                ts.push_ts(": void");
            }
            ts.push_both(";\n");
            dts.push_str(&ts.with_types);
        }

        for it in ctx.rust_helpers.code_in_order() {
            rust.push_str(it);
        }

        let js_code = ctx.js_helpers.code_by_group_in_order();

        for group in [JsGroup::Let, JsGroup::Other] {
            if let Some(code) = js_code.get(&group) {
                if group == JsGroup::Let || group == JsGroup::Let {
                    js.push_both("\n");
                }
                for it in code {
                    js.extend_from(&it);
                }
            }
        }

        // Export object
        js.push_both("\nlet _ffi_exports");
        js.push_ts(": {\n");
        js.push_ts("    memory: WebAssembly.Memory,\n");
        if let Some(code) = js_code.get(&JsGroup::Exports) {
            for it in code {
                js.extend_from(&it);
            }
        }
        js.push_ts("}");
        js.push_both(";\n");

        // Import object
        js.push_both("\nconst _ffi_imports = {");
        if let Some(code) = js_code.get(&JsGroup::Imports) {
            for it in code {
                js.extend_from(&it);
            }
        }
        js.push_both("};\n");

        let mut output_files = vec![FileData {
            path: rust_path,
            contents: rust,
        }];
        if let Some(path) = js_path {
            output_files.push(FileData {
                path,
                contents: js.without_types,
            });
        }
        if let Some(path) = &self.ts_path {
            output_files.push(FileData {
                path: path.clone(),
                contents: js.with_types,
            });
        }
        if let Some(path) = &self.d_ts_path {
            output_files.push(FileData {
                path: path.clone(),
                contents: dts,
            });
        }
        output_files
    }
}

#[derive(Default)]
struct WasmCtx {
    syntax: RustSyntax,
    helper_names: NameSet,
    js_helpers: HelperSet<JsGroup, JsString>,
    rust_helpers: HelperSet<(), String>,
    multi_ret_helpers: HashMap<Vec<RustType>, (String, String)>,
    trait_to_rust_helpers: HashMap<usize, String>,
    trait_to_js_helpers: HashMap<(RustPtr, usize), String>,
    vec_to_rust_helpers: HashMap<RustType, (String, String)>,
    vec_to_js_helpers: HashMap<RustType, (String, String)>,
    box_to_rust_helpers: HashMap<RustType, (String, String)>,
    box_to_js_helpers: HashMap<RustType, (String, String)>,
    enum_to_rust_helpers: HashMap<usize, (String, String)>,
    enum_to_js_helpers: HashMap<usize, (String, String)>,
}

#[derive(Default)]
struct Transform {
    js: FnBuilder,
    rust: FnBuilder,
    ffi_args: Vec<RustArg>,
    buf: Option<Rc<SharedBuf>>,
    buf_status: BufStatus,
    buf_ref: &'static str,
}

#[derive(Default)]
struct JsString {
    without_types: String,
    with_types: String,
}

impl JsString {
    fn extend_from(&mut self, it: &JsString) {
        self.without_types.push_str(&it.without_types);
        self.with_types.push_str(&it.with_types);
    }

    fn push_js(&mut self, text: &str) {
        self.without_types.push_str(text);
    }

    fn push_ts(&mut self, text: &str) {
        self.with_types.push_str(text);
    }

    fn push_both(&mut self, text: &str) {
        self.without_types.push_str(text);
        self.with_types.push_str(text);
    }

    fn push_template(&mut self, text: &str) {
        for (i, part) in text.split(&['«', '»']).enumerate() {
            if i % 2 == 0 {
                self.without_types.push_str(part);
            }
            self.with_types.push_str(part);
        }
    }

    fn from_template(text: &str) -> JsString {
        let mut js = JsString::default();
        js.push_template(text);
        js
    }
}

impl From<&str> for JsString {
    fn from(value: &str) -> JsString {
        JsString {
            without_types: value.to_string(),
            with_types: value.to_string(),
        }
    }
}

impl std::fmt::Write for JsString {
    fn write_str(&mut self, text: &str) -> Result<(), std::fmt::Error> {
        self.push_both(text);
        Ok(())
    }
}

struct TraitInfo<'a> {
    t: &'a RustTrait,
    kind: RustPtr,
}

fn generate_js_to_rust_fn(
    ast: &AST,
    ctx: &mut WasmCtx,
    f: &RustFn,
    js: &mut JsString,
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
        arg_tfm.js.mark_pure(&arg.name);
        transform_to_rust(ast, ctx, &mut names, &mut arg_tfm, &arg.name, &arg.ty);
    }
    arg_tfm.js.insert_deferred_lines_here();

    // Generate the Rust call to the API function
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
        transform_to_js(ast, ctx, &mut names, &mut ret_tfm, &ret.name, &ret.ty);
    } else {
        rust_call.push(';');
        ret_tfm.rust.line(rust_call);
    }

    // Generate the JavaScript call to the FFI function
    let mut js_call = String::new();
    _ = write!(js_call, "_ffi_exports.{ffi_name}(");
    if trait_info.is_some() {
        js_call.push_str("this._");
        if !arg_tfm.ffi_args.is_empty() {
            js_call.push_str(", ");
        }
    }
    js_call.push_str(&arg_tfm.js.find_args(&arg_tfm.ffi_args, RefInline));
    js_call.push(')');

    // JavaScript
    {
        // Handle the return values
        let mut fb = arg_tfm.js;
        match &ret_tfm.ffi_args[..] {
            [] => {
                js_call.push(';');
                fb.line(js_call)
            }
            [arg] => fb.decl(&arg.name, js_call),
            _ => {
                let ret = names.create("multi_ret");
                let mut view = "_ffi_update_dv()";
                let mut offset = 0;
                fb.decl(&ret, js_call);
                for arg in &ret_tfm.ffi_args {
                    fb.decl(
                        &arg.name,
                        js_multi_ret_load(view, &ret, &arg.ty, &mut offset),
                    );
                    view = "_ffi_dv";
                    ctx.js_helpers
                        .mark_used_group(JsGroup::Other, "_ffi_update_dv");
                }
            }
        };

        // Return from the function
        fb.extend(ret_tfm.js);
        fb.insert_deferred_lines_here();
        if let Some(ret) = &f.returns {
            let code = fb.find(&ret.name, &ret.ty, RefInline).code;
            fb.line(format!("return {code};"));
        }

        // Write out the final function
        let (_, indent) = match &trait_info {
            None => (write!(js, "\nexport function {}(", f.name), "    "),
            Some(_) => (write!(js, "\n    {}(", f.name), "        "),
        };
        for (i, arg) in f.args.iter().enumerate() {
            if i > 0 {
                js.push_both(", ");
            }
            js.push_both(&arg.name);
            js.push_ts(": ");
            append_ts_type(js, ast, &arg.ty);
        }
        js.push_both(")");
        if let Some(returns) = &f.returns {
            js.push_ts(": ");
            append_ts_type(js, ast, &returns.ty);
        } else {
            js.push_ts(": void");
        }
        js.push_both(" {\n");
        fb.write_to_js(ast, js, indent);
        _ = write!(js, "{}}}\n", &indent[4..]);
    }

    // TypeScript (export)
    {
        let mut js = JsString::default();
        js.push_ts(&format!("    {ffi_name}: ("));
        if trait_info.is_some() {
            js.push_ts("_self: number");
        }
        for (i, arg) in arg_tfm.ffi_args.iter().enumerate() {
            if trait_info.is_some() || i > 0 {
                js.push_ts(", ");
            }
            js.push_ts(&arg.name);
            js.push_ts(": ");
            append_ts_type(&mut js, ast, &arg.ty);
        }
        js.push_ts(") => ");
        match &ret_tfm.ffi_args[..] {
            [] => js.push_ts("void"),
            [arg] => append_ts_type(&mut js, ast, &arg.ty),
            _ => js.push_ts("number"),
        }
        js.push_ts(",\n");
        ctx.js_helpers
            .add_group(JsGroup::Exports, &ffi_name, js)
            .mark_used();
    }

    // Rust
    {
        // Return from the function
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
                let (ty_name, static_name) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                let args = fb.find_args(&ret_tfm.ffi_args, RefInline);

                // Note: This must be formatted this way (both lines in one
                // unsafe block) to work correctly across different versions
                // of Rust. Older versions of Rust give an error if "unsafe"
                // is missing and newer versions of Rust give an error if
                // "unsafe" is present.
                fb.line("unsafe {".into());
                fb.line(format!("{static_name} = {ty_name}({args});"));
                fb.line(format!("std::ptr::addr_of!({static_name})"));
                fb.line("}".into());
            }
        }

        // Write out the final function
        let mut rust = String::new();
        _ = write!(rust, "\n{}\n", ctx.syntax.unsafe_no_mangle());
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
                let (ty_name, _) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(rust, " -> *const {ty_name}");
            }
        }
        rust.push_str(" {\n");
        fb.write_to_rust(ast, &mut rust, "    ");
        rust.push_str("}\n");
        ctx.rust_helpers.add(&ffi_name, rust).mark_used();
    }
}

fn generate_rust_to_js_fn(
    ast: &AST,
    ctx: &mut WasmCtx,
    t: &RustTrait,
    f: &RustFn,
    rust: &mut String,
) {
    let ffi_name = ctx
        .helper_names
        .create(&format!("_ffi_js_{}__{}", t.name, f.name));
    let mut names = NameSet::default();
    for arg in &f.args {
        names.add(arg.name.clone());
    }
    if let Some(ret) = &f.returns {
        names.add(ret.name.clone());
    }

    // Transform the arguments
    let mut arg_tfm = Transform::default();
    for arg in &f.args {
        arg_tfm.rust.mark_pure(&arg.name);
        transform_to_js(ast, ctx, &mut names, &mut arg_tfm, &arg.name, &arg.ty);
    }
    arg_tfm.rust.insert_deferred_lines_here();

    // Generate the JavaScript call to the API function
    let mut js_call = format!("_ffi_handles.get(self).{}(", f.name);
    js_call.push_str(&arg_tfm.js.find_args(&f.args, RefInline));
    js_call.push(')');

    // Transform the result
    let mut ret_tfm = Transform::default();
    if let Some(ret) = &f.returns {
        ret_tfm.js.decl(&ret.name, js_call);
        transform_to_rust(ast, ctx, &mut names, &mut ret_tfm, &ret.name, &ret.ty);
    } else {
        js_call.push(';');
        ret_tfm.js.line(js_call);
    }

    // Generate the Rust call to the FFI function
    let mut rust_call = format!("unsafe {{ {ffi_name}(self.0");
    if !arg_tfm.ffi_args.is_empty() {
        rust_call.push_str(", ");
    }
    rust_call.push_str(&arg_tfm.rust.find_args(&arg_tfm.ffi_args, RefInline));
    if ret_tfm.ffi_args.len() > 1 {
        let (_, static_name) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
        _ = write!(rust_call, ", std::ptr::addr_of!({static_name})");
    }
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
                let (_, static_name) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                fb.line(rust_call);
                for (i, arg) in ret_tfm.ffi_args.iter().enumerate() {
                    fb.decl(&arg.name, format!("unsafe {{ {static_name}.{i} }}"));
                }
            }
        };

        // Return from the function
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
            "        {} \"C\" {{ fn {ffi_name}(_: *const u8",
            ctx.syntax.unsafe_extern()
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
                let (ty_name, _) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                _ = write!(rust, ", _: *const {ty_name})");
            }
        }
        rust.push_str("; }\n");
        fb.write_to_rust(ast, rust, "        ");
        rust.push_str("    }\n");
    }

    // JavaScript
    {
        // Return from the function
        let mut fb = arg_tfm.js;
        fb.extend(ret_tfm.js);
        fb.insert_deferred_lines_here();
        match &ret_tfm.ffi_args[..] {
            [] => {}
            [arg] => {
                let code = fb.find(&arg.name, &arg.ty, RefInline).code;
                fb.line(format!("return {code};"));
            }
            _ => {
                let (ty_name, _) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
                let mut offset = 0;
                let mut offsets = Vec::new();
                for arg in &ret_tfm.ffi_args {
                    let size = js_multi_ret_size(&arg.ty);
                    offset = (offset + (size - 1)) & !(size - 1);
                    offsets.push(offset);
                    offset += size;
                }
                let mut stores = Vec::new();
                for (offset, arg) in offsets.iter().zip(ret_tfm.ffi_args.iter()).rev() {
                    let code = fb.find(&arg.name, &arg.ty, RefInline).code;
                    stores.push(js_multi_ret_store(
                        match offset {
                            0 => "_ffi_update_dv()",
                            _ => "_ffi_dv",
                        },
                        &ty_name,
                        &code,
                        &arg.ty,
                        *offset,
                    ));
                }
                for store in stores.into_iter().rev() {
                    fb.line(store);
                }
                ctx.js_helpers
                    .mark_used_group(JsGroup::Other, "_ffi_update_dv");
            }
        }

        // Write out the final function
        let mut js = JsString::default();
        _ = write!(js, "\n    {ffi_name}(self");
        js.push_ts(": number");
        for arg in &arg_tfm.ffi_args {
            js.push_both(", ");
            js.push_both(&arg.name);
            js.push_ts(": ");
            append_ts_type(&mut js, ast, &arg.ty);
        }
        if ret_tfm.ffi_args.len() > 1 {
            let (ty_name, _) = multi_ret_helper(ast, ctx, &ret_tfm.ffi_args);
            js.push_both(", ");
            js.push_both(&ty_name);
            js.push_ts(": number");
        }
        js.push_both(")");
        match &ret_tfm.ffi_args[..] {
            [arg] => {
                js.push_ts(": ");
                append_ts_type(&mut js, ast, &arg.ty);
            }
            _ => js.push_ts(": void"),
        }
        js.push_both(" {\n");
        fb.write_to_js(ast, &mut js, "        ");
        js.push_both("    },\n");
        ctx.js_helpers
            .add_group(JsGroup::Imports, &ffi_name, js)
            .mark_used();
    }
}

fn transform_to_rust(
    ast: &AST,
    ctx: &mut WasmCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    name: &str,
    ty: &RustType,
) {
    use RustType::*;

    fn add_ffi_arg(ast: &AST, ctx: &mut WasmCtx, tfm: &mut Transform, name: &str, ty: &RustType) {
        match tfm.buf_status {
            BufStatus::Outside => tfm.ffi_args.push(RustArg {
                name: name.to_string(),
                ty: ty.clone(),
            }),
            BufStatus::Inside => {
                let buf = tfm.buf.as_ref().unwrap();

                // JavaScript (write)
                let code = tfm.js.find(name, ty, RefInline).code;
                tfm.js.line(js_write(ctx, buf.buf_name(), &code, ty));

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
        | ForeignHandle => add_ffi_arg(ast, ctx, tfm, name, ty),

        RefStr | OwnStr => {
            let js_code = tfm.js.find(name, ty, RefInline).code;
            let ptr_name = names.create(&format!("{name}_ptr"));
            let len_name = names.create(&format!("{name}_len"));
            let opt_ref = match ty {
                RefStr => "&",
                _ => "",
            };
            tfm.js.line(format!(
                "let {ptr_name} = _ffi_string_to_rust({js_code}), {len_name} = _ffi_len;"
            ));
            ctx.js_helpers.mark_used_group(JsGroup::Let, "_ffi_len");
            ctx.js_helpers
                .mark_used_group(JsGroup::Other, "_ffi_string_to_rust");
            add_ffi_arg(ast, ctx, tfm, &ptr_name, &ForeignHandle);
            add_ffi_arg(ast, ctx, tfm, &len_name, &Usize);
            let len_code = tfm.rust.find(&len_name, &Usize, RefInline).code;
            let ptr_code = tfm.rust.find(&ptr_name, &ForeignHandle, RefInline).code;
            tfm.rust.decl(
                name,
                format!("{opt_ref}_ffi_string_from_host({ptr_code}, {len_code})"),
            );
            ctx.rust_helpers.mark_used("_ffi_string_from_host");
        }

        Enum(enum_index) => {
            let js = tfm.js.find(name, ty, RefInline);
            let e = &ast.enums[*enum_index];
            let (js_helper, rust_helper) = enum_to_rust_helper(ast, ctx, *enum_index);
            if !e.has_fields() {
                let raw_name = names.create(&format!("{name}_raw"));
                tfm.js.maybe_pure_decl(js.pure, &raw_name, js.code);
                add_ffi_arg(ast, ctx, tfm, &raw_name, &I32);
                tfm.rust.decl(name, format!("{rust_helper}({raw_name})"));
            } else {
                let buf = ensure_js_buf(ctx, names, tfm);
                tfm.js
                    .line(format!("{js_helper}({}, {});", js.code, buf.buf_name()));
                tfm.rust.decl(
                    name,
                    format!("{rust_helper}({}{})", tfm.buf_ref, buf.end_name()),
                );
            }
        }

        Struct(struct_index) => {
            let js = tfm.js.find(name, ty, RefMany);
            let s = &ast.structs[*struct_index];
            let mut item_names = Vec::new();
            for f in &s.fields {
                let item_name = names.create(&format!("{name}_{}", f.name));
                tfm.js
                    .maybe_pure_decl(js.pure, &item_name, member_access(&js.code, &f.name));
                transform_to_rust(ast, ctx, names, tfm, &item_name, &f.ty);
                item_names.push(item_name.into());
            }
            rust_decl_ctor(&mut tfm.rust, name, &s.name, &s.fields, item_names);
        }

        Tuple(types) => {
            let js = tfm.js.find(name, ty, RefMany);
            let mut item_args = Vec::new();
            for (i, item_ty) in types.iter().enumerate() {
                let item_name = names.create(&format!("{name}_{i}"));
                tfm.js
                    .maybe_pure_decl(js.pure, &item_name, format!("{}[{i}]", js.code));
                transform_to_rust(ast, ctx, names, tfm, &item_name, &item_ty);
                item_args.push(RustArg {
                    name: item_name,
                    ty: item_ty.clone(),
                });
            }
            let mut rust_code = tfm.rust.find_args(&item_args, RefInline);
            if types.len() == 1 {
                rust_code.push(',');
            }
            tfm.rust.decl(name, format!("({rust_code})"));
        }

        Ptr(kind, inner_ty) => {
            if let DynTrait(trait_index) = &**inner_ty {
                let js_code = tfm.js.find(name, ty, RefInline).code;
                let ptr_name = names.create(&format!("{name}_ptr"));
                let rust_helper = trait_to_rust_helper(ast, ctx, *trait_index);
                let js_code = format!("_ffi_handle_alloc({js_code})");
                ctx.js_helpers
                    .mark_used_group(JsGroup::Other, "_ffi_handle_alloc");
                tfm.js.decl(&ptr_name, js_code);
                add_ffi_arg(ast, ctx, tfm, &ptr_name, &ForeignHandle);
                let ptr_code = tfm.rust.find(&ptr_name, &ForeignHandle, RefInline).code;
                tfm.rust.decl(
                    name,
                    format!("{}::new({rust_helper}({ptr_code}))", kind.path()),
                );
            } else if *kind == RustPtr::Box {
                let js_code = tfm.js.find(name, ty, RefMany).code;
                let (js_helper, rust_helper) = box_to_rust_helper(ast, ctx, inner_ty);
                let buf = ensure_js_buf(ctx, names, tfm);
                tfm.js
                    .line(format!("{js_helper}({js_code}, {});", buf.buf_name()));
                tfm.rust.decl(
                    name,
                    format!("{rust_helper}({}{})", tfm.buf_ref, buf.end_name()),
                );
            } else {
                unreachable!()
            }
        }

        Vector(inner_ty) => {
            let js_code = tfm.js.find(name, ty, RefMany).code;
            let len_name = names.create(&format!("{name}_len"));
            let (js_helper, rust_helper) = vec_to_rust_helper(ast, ctx, inner_ty);
            let buf = ensure_js_buf(ctx, names, tfm);
            tfm.js.decl(&len_name, format!("{js_code}.length"));
            add_ffi_arg(ast, ctx, tfm, &len_name, &Usize);
            tfm.js
                .line(format!("{js_helper}({js_code}, {});", buf.buf_name()));
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
            let js_code = tfm.js.find(name, ty, RefMany).code;
            let has_name = names.create(&format!("has_{name}"));
            let val_name = names.create(&format!("{name}_val"));
            ensure_js_buf(ctx, names, tfm);

            tfm.js.decl(&has_name, format!("{js_code} !== null"));
            add_ffi_arg(ast, ctx, tfm, &has_name, &Bool);

            let mut rust = FnBuilder::default();
            tfm.js.line(format!("if ({js_code} !== null) {{"));
            tfm.js.decl(&val_name, format!("{js_code}"));
            {
                let old = tfm.buf_status;
                tfm.buf_status = BufStatus::Inside;
                std::mem::swap(&mut tfm.rust, &mut rust);
                transform_to_rust(ast, ctx, names, tfm, &val_name, inner_ty);
                std::mem::swap(&mut tfm.rust, &mut rust);
                tfm.buf_status = old;
            }
            tfm.js.line("}".into());

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

fn transform_to_js(
    ast: &AST,
    ctx: &mut WasmCtx,
    names: &mut NameSet,
    tfm: &mut Transform,
    name: &str,
    ty: &RustType,
) {
    use RustType::*;

    fn add_ffi_arg(ctx: &mut WasmCtx, tfm: &mut Transform, name: &str, ty: &RustType) {
        match tfm.buf_status {
            BufStatus::Outside => {
                tfm.ffi_args.push(RustArg {
                    name: name.to_string(),
                    ty: ty.clone(),
                });
                if let Bool = ty {
                    // This comes over the FFI as an integer and needs a cast
                    tfm.js.pure_decl(name, format!("!!{name}"));
                }
            }
            BufStatus::Inside => {
                let buf_name = tfm.buf.as_ref().unwrap().buf_name();

                // Rust (write)
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust
                    .line(format!("_ffi_write({code}, {}{buf_name});", tfm.buf_ref));
                ctx.rust_helpers.mark_used("_ffi_write");

                // JavaScript (read)
                tfm.js.decl(name, js_read(ctx, buf_name, ty));
            }
        }
    }

    match ty {
        Bool | U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 | F32 | F64
        | ForeignHandle => {
            add_ffi_arg(ctx, tfm, name, ty);
        }

        RefStr | OwnStr => {
            let mut rust_code = tfm.rust.find(name, ty, RefInline).code;
            let ptr_name = names.create(&format!("{name}_ptr"));
            let len_name = names.create(&format!("{name}_len"));
            let cap_name = names.create(&format!("{name}_cap"));
            if let RefStr = ty {
                rust_code.push_str(".into()");
            }
            tfm.rust.line(format!(
                "let ({ptr_name}, {len_name}, {cap_name}) = _ffi_string_to_host({rust_code});"
            ));
            ctx.rust_helpers.mark_used("_ffi_string_to_host");
            add_ffi_arg(ctx, tfm, &ptr_name, &ForeignHandle);
            add_ffi_arg(ctx, tfm, &len_name, &Usize);
            add_ffi_arg(ctx, tfm, &cap_name, &Usize);
            let cap_code = tfm.js.find(&cap_name, &Usize, RefInline).code;
            let len_code = tfm.js.find(&len_name, &Usize, RefInline).code;
            let ptr_code = tfm.js.find(&ptr_name, &ForeignHandle, RefInline).code;
            tfm.js.decl(
                name,
                format!("_ffi_string_from_rust({ptr_code}, {len_code}, {cap_code})"),
            );
            ctx.js_helpers
                .mark_used_group(JsGroup::Other, "_ffi_string_from_rust");
        }

        Enum(enum_index) => {
            let rust = tfm.rust.find(name, ty, RefInline);
            let e = &ast.enums[*enum_index];
            if !e.has_fields() {
                tfm.rust
                    .maybe_pure_decl(rust.pure, name, format!("{} as i32", rust.code));
                add_ffi_arg(ctx, tfm, name, &I32);
            } else {
                let (js_helper, rust_helper) = enum_to_js_helper(ast, ctx, *enum_index);
                let buf = ensure_rust_buf(ctx, names, tfm);
                tfm.rust.line(format!(
                    "{rust_helper}({}, {}{});",
                    rust.code,
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                tfm.js
                    .decl_ty(name, ty.clone(), format!("{js_helper}({})", buf.buf_name()));
            }
        }

        Struct(struct_index) => {
            let rust = tfm.rust.find(name, ty, RefMany);
            let s = &ast.structs[*struct_index];
            let mut item_names = Vec::new();
            for f in &s.fields {
                let item_name = names.create(&format!("{name}_{}", f.name));
                tfm.rust.maybe_pure_decl(
                    rust.pure,
                    &item_name,
                    format!("{}.{}", rust.code, f.name),
                );
                transform_to_js(ast, ctx, names, tfm, &item_name, &f.ty);
                item_names.push(item_name.into());
            }
            if s.fields.is_empty() {
                tfm.js.decl_ty(name, ty.clone(), "{}".to_string());

                // Avoid an unused variable warning in Rust
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!("_ = {code};"));
            } else {
                let fields = tfm
                    .js
                    .find_fields(&s.fields, item_names, RefInline, "{ ", " }");
                tfm.js.decl_ty(name, ty.clone(), fields);
            }
        }

        Tuple(types) => {
            let rust = tfm.rust.find(name, ty, RefMany);
            let mut item_args = Vec::new();
            for (i, item_ty) in types.iter().enumerate() {
                let item_name = names.create(&format!("{name}_{i}"));
                tfm.rust
                    .maybe_pure_decl(rust.pure, &item_name, format!("{}.{i}", rust.code));
                transform_to_js(ast, ctx, names, tfm, &item_name, &item_ty);
                item_args.push(RustArg {
                    name: item_name,
                    ty: item_ty.clone(),
                });
            }
            if types.is_empty() {
                tfm.js.pure_decl(name, "undefined".to_string());

                // Avoid an unused variable warning in Rust
                let code = tfm.rust.find(name, ty, RefInline).code;
                tfm.rust.line(format!("_ = {code};"));
            } else {
                let js_code = tfm.js.find_args(&item_args, RefInline);
                tfm.js.decl_ty(name, ty.clone(), format!("[{js_code}]"));
            }
        }

        Ptr(kind, inner_ty) => {
            if let DynTrait(trait_index) = &**inner_ty {
                let rust_code = tfm.rust.find(name, ty, RefInline).code;
                let ptr_name = names.create(&format!("{name}_ptr"));
                let js_helper = trait_to_js_helper(ast, ctx, *trait_index, *kind);
                tfm.rust.decl(
                    &ptr_name,
                    format!("Box::into_raw(Box::new({rust_code})) as *const u8"),
                );
                add_ffi_arg(ctx, tfm, &ptr_name, &ForeignHandle);
                let ptr_code = tfm.js.find(&ptr_name, &ForeignHandle, RefInline).code;
                tfm.js
                    .decl_ty(name, ty.clone(), format!("new {js_helper}({ptr_code})"));
            } else if *kind == RustPtr::Box {
                let rust_code = tfm.rust.find(name, ty, RefInline).code;
                let (js_helper, rust_helper) = box_to_js_helper(ast, ctx, inner_ty);
                let buf = ensure_rust_buf(ctx, names, tfm);
                tfm.rust.line(format!(
                    "{rust_helper}(*{rust_code}, {}{});",
                    tfm.buf_ref,
                    buf.buf_name()
                ));
                tfm.js
                    .decl(name, format!("{js_helper}({})", buf.buf_name()));
            } else {
                unreachable!()
            }
        }

        Vector(inner_ty) => {
            let rust_code = tfm.rust.find(name, ty, RefMany).code;
            let len_name = names.create(&format!("{name}_len"));
            let (js_helper, rust_helper) = vec_to_js_helper(ast, ctx, inner_ty);
            let buf = ensure_rust_buf(ctx, names, tfm);
            tfm.rust.decl(&len_name, format!("{rust_code}.len()"));
            add_ffi_arg(ctx, tfm, &len_name, &Usize);
            tfm.rust.line(format!(
                "{rust_helper}({rust_code}, {}{});",
                tfm.buf_ref,
                buf.buf_name()
            ));
            let len_code = tfm.js.find(&len_name, ty, RefInline).code;
            tfm.js
                .decl(name, format!("{js_helper}({len_code}, {})", buf.buf_name()));
        }

        Optional(inner_ty) => {
            let rust_code = tfm.rust.find(name, ty, RefMany).code;
            let has_name = names.create(&format!("has_{name}"));
            let val_name = names.create(&format!("{name}_val"));
            ensure_rust_buf(ctx, names, tfm);

            tfm.rust.decl(&has_name, format!("{rust_code}.is_some()"));
            add_ffi_arg(ctx, tfm, &has_name, &Bool);

            let mut js = FnBuilder::default();
            tfm.rust
                .line(format!("if let Some({val_name}) = {rust_code} {{"));
            {
                let old = tfm.buf_status;
                tfm.buf_status = BufStatus::Inside;
                std::mem::swap(&mut tfm.js, &mut js);
                transform_to_js(ast, ctx, names, tfm, &val_name, inner_ty);
                std::mem::swap(&mut tfm.js, &mut js);
                tfm.buf_status = old;
            }
            tfm.rust.line("}".into());

            let has_code = tfm.js.find(&has_name, ty, RefInline).code;
            let has_code = has_code.strip_prefix("!!").unwrap_or(&has_code);
            let val_code = js.find(&val_name, ty, RefInline).code;
            if js.is_empty() {
                tfm.js
                    .decl_ty(name, ty.clone(), format!("{has_code} ? {val_code} : null"));
            } else {
                js.insert_deferred_lines_here();
                js.line(format!("{name} = {val_code};"));
                tfm.js.decl_ty(name, ty.clone(), "null".into());
                tfm.js.line(format!("if ({has_code}) {{"));
                tfm.js.extend(js);
                tfm.js.line("}".to_string());
            }
        }

        Pair { .. } | Verbatim(_) | DynTrait(_) => unreachable!(),
    }
}

fn ensure_js_buf(ctx: &mut WasmCtx, names: &mut NameSet, tfm: &mut Transform) -> Rc<SharedBuf> {
    if let Some(buf) = &tfm.buf {
        return buf.clone();
    }

    let buf_name = names.create("buf");
    let ptr_name = names.create("buf_ptr");
    let end_name = names.create("buf_end");
    let buf = SharedBuf::new(&buf_name, &end_name);

    // JavaScript (write)
    tfm.js
        .line(format!("let {buf_name} = _ffi_new_WriteBuf();"));
    tfm.js
        .defer_decl(&ptr_name, format!("_ffi_buf_to_rust({buf_name})"));
    ctx.js_helpers
        .mark_used_group(JsGroup::Let, "_ffi_new_WriteBuf");
    ctx.js_helpers
        .mark_used_group(JsGroup::Other, "_ffi_buf_to_rust");

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

fn ensure_rust_buf(ctx: &mut WasmCtx, names: &mut NameSet, tfm: &mut Transform) -> Rc<SharedBuf> {
    if let Some(buf) = &tfm.buf {
        return buf.clone();
    }

    let buf_name = names.create("buf");
    let ptr_name = names.create("buf_ptr");
    let cap_name = names.create("buf_cap");
    let buf = SharedBuf::new(&buf_name, "");

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

    // JavaScript (read)
    tfm.js
        .line(format!("let {buf_name} = _ffi_new_ReadBuf({ptr_name});"));
    tfm.js.defer_line(format!(
        "_ffi_exports._ffi_dealloc({ptr_name}, {cap_name});"
    ));
    ctx.js_helpers
        .mark_used_group(JsGroup::Let, "_ffi_new_ReadBuf");
    ctx.js_helpers
        .mark_used_group(JsGroup::Exports, "_ffi_dealloc");

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

fn vec_to_rust_helper(ast: &AST, ctx: &mut WasmCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.vec_to_rust_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_vec_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let js_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_js"));

    // This must be done first to avoid a stack overflow
    ctx.vec_to_rust_helpers
        .insert(inner_ty.clone(), (js_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let vec_name = locals.create("items");
    let item_name = locals.create("item");
    let end_name = locals.create("end");
    let len_name = locals.create("len");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.js.mark_pure(&item_name);
    transform_to_rust(ast, ctx, &mut locals, &mut tfm, &item_name, &inner_ty);
    let item_code = tfm.rust.find(&item_name, inner_ty, RefInline).code;
    tfm.rust.line(format!("{vec_name}.push({item_code});"));

    // JavaScript
    {
        let mut js = JsString::default();
        _ = write!(js, "\nfunction {js_name}({vec_name}");
        js.push_ts(": ");
        append_ts_type(&mut js, ast, &RustType::Vector(inner_ty.clone().into()));
        _ = write!(js, ", {buf_name}");
        js.push_ts(": _ffi_WriteBuf");
        js.push_both(")");
        js.push_ts(": void");
        js.push_both(" {\n");
        if !tfm.js.is_empty() {
            _ = write!(js, "    for (const {item_name} of {vec_name}) {{\n");
            tfm.js.write_to_js(ast, &mut js, "        ");
            js.push_both("    }\n");
        }
        js.push_both("}\n");
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Other, "_ffi_WriteBuf")
            .mark_used();
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

    (js_name, rust_name)
}

fn vec_to_js_helper(ast: &AST, ctx: &mut WasmCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.vec_to_js_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_vec_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let js_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_js"));

    // This must be done first to avoid a stack overflow
    ctx.vec_to_js_helpers
        .insert(inner_ty.clone(), (js_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let vec_name = locals.create("items");
    let item_name = locals.create("item");
    let len_name = locals.create("len");
    let buf_name = locals.create("buf");

    // Transform the items
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, "");
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.rust.mark_pure(&item_name);
    transform_to_js(ast, ctx, &mut locals, &mut tfm, &item_name, &inner_ty);
    let item_code = tfm.js.find(&item_name, inner_ty, RefInline).code;
    tfm.js.line(format!("{vec_name}.push({item_code});"));

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

    // JavaScript
    {
        let mut js = JsString::default();
        let array_ty = RustType::Vector(inner_ty.clone().into());
        _ = write!(js, "\nfunction {js_name}({len_name}");
        js.push_ts(": number");
        _ = write!(js, ", {buf_name}");
        js.push_ts(": _ffi_ReadBuf");
        js.push_both(")");
        js.push_ts(": ");
        append_ts_type(&mut js, ast, &array_ty);
        js.push_both(" {\n");
        _ = write!(js, "    let {vec_name}");
        js.push_ts(": ");
        append_ts_type(&mut js, ast, &array_ty);
        js.push_both(" = [];\n");
        _ = write!(js, "    while ({vec_name}.length < {len_name}) {{\n");
        tfm.js.write_to_js(ast, &mut js, "        ");
        js.push_both("    }\n");
        _ = write!(js, "    return {vec_name};\n");
        js.push_both("}\n");
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Other, "_ffi_ReadBuf")
            .mark_used();
    }

    (js_name, rust_name)
}

fn trait_to_rust_helper(ast: &AST, ctx: &mut WasmCtx, trait_index: usize) -> String {
    if let Some(result) = ctx.trait_to_rust_helpers.get(&trait_index) {
        return result.clone();
    }

    let t = &ast.traits[trait_index];
    let rust_name = format!("_ffi_rs_{}", t.name);

    // This must be done first to avoid a stack overflow
    ctx.trait_to_rust_helpers
        .insert(trait_index, rust_name.clone());

    // Rust
    {
        let mut rust = String::new();
        allow_non_camel_case_types(&mut rust, &rust_name);
        _ = write!(rust, "\nstruct {rust_name}(*const u8);\n");
        _ = write!(rust, "\nimpl Drop for {rust_name} {{\n");
        rust.push_str("    fn drop(&mut self) {\n");
        _ = write!(
            rust,
            "        {} \"C\" {{ fn _ffi_js_drop(_: *const u8); }}\n",
            ctx.syntax.unsafe_extern()
        );
        _ = write!(rust, "        unsafe {{ _ffi_js_drop(self.0) }};\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        _ = write!(rust, "\nimpl {} for {rust_name} {{", t.name);
        for f in &t.fns {
            generate_rust_to_js_fn(ast, ctx, t, f, &mut rust);
        }
        rust.push_str("}\n");
        ctx.rust_helpers.add(&rust_name, rust).mark_used();
        ctx.js_helpers
            .mark_used_group(JsGroup::Imports, "_ffi_js_drop");
    }

    rust_name
}

fn trait_to_js_helper(ast: &AST, ctx: &mut WasmCtx, trait_index: usize, kind: RustPtr) -> String {
    if let Some(result) = ctx.trait_to_js_helpers.get(&(kind, trait_index)) {
        return result.clone();
    }

    let t = &ast.traits[trait_index];
    let drop_name = format!("_ffi_rs_drop_{kind:?}_{}", t.name);
    let reg_name = format!("_ffi_reg_{kind:?}_{}", t.name);
    let js_name = format!("_ffi_{kind:?}_{}", t.name);

    // This must be done first to avoid a stack overflow
    ctx.trait_to_js_helpers
        .insert((kind, trait_index), js_name.clone());

    // JavaScript
    {
        let reg_js = JsString::from_template(&format!(
            "let {reg_name} = new FinalizationRegistry((ptr«: number») => _ffi_exports.{drop_name}(ptr));\n"
        ));
        let mut js = JsString::from_template(&format!(
            r#"
const {js_name} = class {}« implements {}» {{
«    declare readonly _: number;

»    constructor(_«: number») {{
        Object.defineProperty(this, "_", {{ value: _ }});
        {reg_name}.register(this, _);
    }}
"#,
            t.name, t.name
        ));
        for f in &t.fns {
            let info = Some(TraitInfo { t, kind });
            generate_js_to_rust_fn(ast, ctx, f, &mut js, info);
        }
        js.push_both("};\n");
        ctx.js_helpers.add_group(JsGroup::Let, &reg_name, reg_js);
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Let, &reg_name)
            .mark_used();
    }

    // TypeScript (export)
    {
        ctx.js_helpers
            .add_group(
                JsGroup::Exports,
                &drop_name,
                JsString::from_template(&format!("«    {drop_name}: (ptr: number) => void,\n»")),
            )
            .mark_used();
    }

    // Rust
    {
        let mut rust = String::new();
        _ = write!(rust, "\n{}\n", ctx.syntax.unsafe_no_mangle());
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

    js_name
}

fn box_to_rust_helper(ast: &AST, ctx: &mut WasmCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.box_to_rust_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_box_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let js_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_js"));

    // This must be done first to avoid a stack overflow
    ctx.box_to_rust_helpers
        .insert(inner_ty.clone(), (js_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let end_name = locals.create("end");
    let buf_name = locals.create("buf");

    // Transform the value
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, &end_name);
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.js.mark_pure(&val_name);
    transform_to_rust(ast, ctx, &mut locals, &mut tfm, &val_name, &inner_ty);
    let val_code = tfm.rust.find(&val_name, inner_ty, RefInline).code;
    tfm.rust.line(format!("Box::new({val_code})"));

    // JavaScript
    {
        let mut js = JsString::default();
        _ = write!(js, "\nfunction {js_name}({val_name}");
        js.push_ts(": ");
        append_ts_type(&mut js, ast, &inner_ty);
        _ = write!(js, ", {buf_name}");
        js.push_ts(": _ffi_WriteBuf");
        js.push_both(")");
        js.push_ts(": void");
        js.push_both(" {\n");
        tfm.js.write_to_js(ast, &mut js, "    ");
        js.push_both("}\n");
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Other, "_ffi_WriteBuf")
            .mark_used();
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

    (js_name, rust_name)
}

fn box_to_js_helper(ast: &AST, ctx: &mut WasmCtx, inner_ty: &RustType) -> (String, String) {
    if let Some(result) = ctx.box_to_js_helpers.get(inner_ty) {
        return result.clone();
    }

    let mut base_name = "_ffi_box_".to_string();
    append_type_name_hint(&mut base_name, ast, inner_ty);
    let js_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_js"));

    // This must be done first to avoid a stack overflow
    ctx.box_to_js_helpers
        .insert(inner_ty.clone(), (js_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");

    // Transform the value
    let mut tfm = Transform::default();
    let buf = SharedBuf::new(&buf_name, "");
    tfm.buf = Some(buf.clone());
    tfm.buf_status = BufStatus::Inside;
    tfm.rust.mark_pure(&val_name);
    transform_to_js(ast, ctx, &mut locals, &mut tfm, &val_name, &inner_ty);
    let val_code = tfm.js.find(&val_name, inner_ty, RefInline).code;
    tfm.js.line(format!("return {val_code};"));

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

    // JavaScript
    {
        let mut js = JsString::default();
        _ = write!(js, "\nfunction {js_name}({buf_name}");
        js.push_ts(": _ffi_ReadBuf");
        js.push_both(")");
        js.push_ts(": ");
        append_ts_type(&mut js, ast, &inner_ty);
        js.push_both(" {\n");
        tfm.js.write_to_js(ast, &mut js, "    ");
        js.push_both("}\n");
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Other, "_ffi_ReadBuf")
            .mark_used();
    }

    (js_name, rust_name)
}

fn enum_to_rust_helper(ast: &AST, ctx: &mut WasmCtx, enum_index: usize) -> (String, String) {
    if let Some(result) = ctx.enum_to_rust_helpers.get(&enum_index) {
        return result.clone();
    }

    let e = &ast.enums[enum_index];
    let base_name = format!("_ffi_enum_{}", e.name);
    let rust_name = ctx.helper_names.create(&format!("{base_name}_from_js"));
    let js_name = ctx.helper_names.create(&format!("{base_name}_to_rust"));

    // This must be done first to avoid a stack overflow
    ctx.enum_to_rust_helpers
        .insert(enum_index, (js_name.clone(), rust_name.clone()));

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");
    let end_name = locals.create("end");
    let mut branches = Vec::new();

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
        return (js_name, rust_name);
    }

    struct Branch {
        tfm: Transform,
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
            tfm.js
                .pure_decl(field_name.clone(), member_access(&val_name, &f.name));
            transform_to_rust(ast, ctx, &mut branch_locals, &mut tfm, &field_name, &f.ty);
            fields.push(field_name);
        }
        rust_decl_ctor(
            &mut tfm.rust,
            &val_name,
            &format!("{}::{}", e.name, v.name),
            &v.fields,
            fields.iter().map(|x| Cow::Borrowed(x.as_str())).collect(),
        );
        branches.push(Branch { tfm });
    }

    // JavaScript
    {
        let mut js = JsString::from_template(&format!(
            "\nfunction {js_name}({val_name}«: {}», {buf_name}«: _ffi_WriteBuf»)«: void» {{\n",
            e.name
        ));
        _ = write!(js, "    switch ({val_name}.$) {{\n");
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            let write = js_write(
                ctx,
                &buf_name,
                &format!("{}", v.discriminant),
                &RustType::I32,
            );
            _ = write!(js, "        case {:?}:\n", v.name);
            _ = write!(js, "            {}\n", write);
            branch.tfm.js.write_to_js(ast, &mut js, "            ");
            js.push_both("            break;\n");
        }
        js.push_both("        default:\n");
        _ = write!(
            js,
            "            throw TypeError({:?});\n",
            format!("Invalid value for enum {:?}", e.name)
        );
        js.push_both("    }\n");
        js.push_both("}\n");
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Other, "_ffi_WriteBuf")
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
            let val_code = branch
                .tfm
                .rust
                .find(&val_name, &RustType::Enum(enum_index), RefInline)
                .code;
            if branch.tfm.rust.is_empty() {
                let val_code = format!("{} => {val_code},", v.discriminant);
                branch.tfm.rust.line(val_code);
                branch.tfm.rust.write_to_rust(ast, &mut rust, "        ");
            } else {
                _ = write!(rust, "        {} => {{", v.discriminant);
                branch.tfm.rust.line(val_code);
                branch
                    .tfm
                    .rust
                    .write_to_rust(ast, &mut rust, "            ");
                rust.push_str("        }\n");
            }
        }
        rust.push_str("        _ => panic!(),\n");
        rust.push_str("    }\n");
        rust.push_str("}\n");
        ctx.rust_helpers
            .add(&rust_name, rust)
            .add_dep("_ffi_read")
            .mark_used();
    }

    (js_name, rust_name)
}

fn enum_to_js_helper(ast: &AST, ctx: &mut WasmCtx, enum_index: usize) -> (String, String) {
    if let Some(result) = ctx.enum_to_js_helpers.get(&enum_index) {
        return result.clone();
    }

    let e = &ast.enums[enum_index];
    let base_name = format!("_ffi_enum_{}", e.name);
    let js_name = ctx.helper_names.create(&format!("{base_name}_from_rust"));
    let rust_name = ctx.helper_names.create(&format!("{base_name}_to_js"));

    // This must be done first to avoid a stack overflow
    ctx.enum_to_js_helpers
        .insert(enum_index, (js_name.clone(), rust_name.clone()));

    struct Branch {
        tfm: Transform,
        fields: Vec<String>,
    }

    let mut locals = NameSet::default();
    let val_name = locals.create("val");
    let buf_name = locals.create("buf");
    let mut branches = Vec::new();
    let mut js_deps = HashSet::new();

    // Transform all fields for each variant in a separate branch
    for v in &e.variants {
        let mut branch_locals = locals.clone();
        let mut fields = Vec::new();
        let mut tfm = Transform::default();
        let buf = SharedBuf::new(&buf_name, "");
        tfm.buf = Some(buf.clone());
        tfm.buf_status = BufStatus::Inside;
        for f in &v.fields {
            let field_name = branch_locals.create(&name_for_match(&f.name, v.fields.len()));
            tfm.rust.mark_pure(&field_name);
            transform_to_js(ast, ctx, &mut branch_locals, &mut tfm, &field_name, &f.ty);
            fields.push(field_name);
        }
        let mut val_fields = vec![RustField {
            name: "$".to_string(),
            ty: RustType::ForeignHandle,
        }];
        let mut val_names = vec![Cow::Owned(format!("{:?}", v.name))];
        val_fields.extend_from_slice(&v.fields);
        for name in &fields {
            val_names.push(Cow::Borrowed(name.as_str()));
        }
        let val_code = tfm
            .js
            .find_fields(&val_fields, val_names, RefInline, "{ ", " }");
        if v.fields.is_empty() {
            // Turn enums without fields into singletons to reduce garbage generation
            let singleton_name = ctx
                .helper_names
                .create(&format!("_ffi_enum_{}__{}", e.name, v.name));
            let mut js =
                JsString::from_template(&format!("let {}«: {}» = ", singleton_name, e.name));
            js.push_both(&val_code);
            js.push_both(";\n");
            ctx.js_helpers.add_group(JsGroup::Let, &singleton_name, js);
            tfm.js.pure_decl(&val_name, singleton_name.clone());
            js_deps.insert((JsGroup::Let, singleton_name));
        } else {
            tfm.js.decl(&val_name, val_code);
        }
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

    // JavaScript
    {
        let mut js = JsString::from_template(&format!(
            "\nfunction {js_name}({buf_name}«: _ffi_ReadBuf»)«: {}» {{\n",
            e.name
        ));
        let read = js_read(ctx, &buf_name, &RustType::I32);
        _ = write!(js, "    switch ({read}) {{\n",);
        for (v, branch) in e.variants.iter().zip(&mut branches) {
            let val_code = branch
                .tfm
                .js
                .find(&val_name, &RustType::Enum(enum_index), RefInline)
                .code;
            if branch.tfm.js.is_empty() {
                let val_code = format!("case {}: return {val_code};", v.discriminant);
                branch.tfm.js.line(val_code);
                branch.tfm.js.write_to_js(ast, &mut js, "        ");
            } else {
                _ = write!(js, "        case {}: {{\n", v.discriminant);
                branch.tfm.js.line(format!("return {val_code};"));
                branch.tfm.js.write_to_js(ast, &mut js, "            ");
                js.push_both("        }\n");
            }
        }
        js.push_both("        default: throw Error();\n");
        js.push_both("    }\n");
        js.push_both("}\n");
        ctx.js_helpers
            .add_group(JsGroup::Other, &js_name, js)
            .add_dep_group(JsGroup::Other, "_ffi_ReadBuf")
            .add_deps_group(js_deps)
            .mark_used();
    }

    (js_name, rust_name)
}

fn multi_ret_helper(ast: &AST, ctx: &mut WasmCtx, args: &[RustArg]) -> (String, String) {
    let types: Vec<_> = args.iter().map(|arg| arg.ty.clone()).collect();
    if let Some(result) = ctx.multi_ret_helpers.get(&types) {
        return result.clone();
    }

    let mut ty_name = "_ffi_ret_".to_string();
    append_type_name_hints(&mut ty_name, ast, &types);
    let ty_name = ctx.helper_names.create(&ty_name);
    let static_name = ty_name.to_uppercase();

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
        _ = write!(rust, "static mut {static_name}: {ty_name} = {ty_name}(");
        for (i, ty) in types.iter().enumerate() {
            if i > 0 {
                rust.push_str(", ");
            }
            append_rust_default_val(&mut rust, ast, ty);
        }
        rust.push_str(");\n");
        ctx.rust_helpers.add(&ty_name, rust).mark_used();
    }

    ctx.multi_ret_helpers
        .insert(types, (ty_name.clone(), static_name.clone()));
    (ty_name, static_name)
}

fn js_multi_ret_load(view: &str, ptr: &str, ty: &RustType, offset: &mut usize) -> String {
    use RustType::*;
    let (size, method) = match ty {
        Bool | U8 => (1, "getUint8"), // Note: the "!!" int-to-bool cast is added elsewhere
        I8 => (1, "getInt8"),
        U16 => (2, "getUint16"),
        I16 => (2, "getInt16"),
        U32 | Usize => (4, "getUint32"),
        I32 | Isize | ForeignHandle => (4, "getInt32"),
        U64 => (8, "setBigUint64"),
        I64 => (8, "setBigInt64"),
        F32 => (4, "getFloat32"),
        F64 => (8, "getFloat64"),
        _ => unreachable!(),
    };
    *offset = (*offset + (size - 1)) & !(size - 1);
    let load = match (*offset, size) {
        (0, 1) => format!("{view}.{method}({ptr})"),
        (0, _) => format!("{view}.{method}({ptr}, true)"),
        (_, 1) => format!("{view}.{method}({ptr} + {offset})"),
        (_, _) => format!("{view}.{method}({ptr} + {offset}, true)"),
    };
    *offset += size;
    load
}

fn js_multi_ret_size(ty: &RustType) -> u32 {
    use RustType::*;
    match ty {
        Bool | U8 | I8 => 1,
        U16 | I16 => 2,
        U32 | Usize | I32 | Isize | ForeignHandle => 4,
        U64 | I64 => 8,
        F32 => 4,
        F64 => 8,
        _ => unreachable!(),
    }
}

fn js_multi_ret_store(view: &str, ptr: &str, val: &str, ty: &RustType, offset: u32) -> String {
    use RustType::*;
    let method = match ty {
        Bool | U8 | I8 => "setInt8",
        U16 | I16 => "setInt16",
        U32 | Usize | I32 | Isize | ForeignHandle => "setInt32",
        U64 | I64 => "setBigInt64",
        F32 => "setFloat32",
        F64 => "setFloat64",
        _ => unreachable!(),
    };
    match (offset, method != "setInt8") {
        (0, false) => format!("{view}.{method}({ptr}, {val});"),
        (0, true) => format!("{view}.{method}({ptr}, {val}, true);"),
        (_, false) => format!("{view}.{method}({ptr} + {offset}, {val});"),
        (_, true) => format!("{view}.{method}({ptr} + {offset}, {val}, true);"),
    }
}

fn js_write(ctx: &mut WasmCtx, buf: &str, val: &str, ty: &RustType) -> String {
    use RustType::*;
    let mut cast = "";
    let (size, method, helper) = match ty {
        Pair { other, .. } => return js_write(ctx, buf, val, other),
        Bool => {
            cast = "+";
            (1, "setInt8", "_ffi_write_i8")
        }
        U8 | I8 => (1, "setInt8", "_ffi_write_i8"),
        U16 | I16 => (2, "setInt16", "_ffi_write_i16"),
        U32 | Usize | I32 | Isize | ForeignHandle => (4, "setInt32", "_ffi_write_i32"),
        U64 | I64 => (8, "setBigInt64", "_ffi_write_i64"),
        F32 => (4, "setFloat32", "_ffi_write_f32"),
        F64 => (8, "setFloat64", "_ffi_write_f64"),
        _ => unreachable!(),
    };
    let result = if !cast.is_empty() && val.chars().any(|c| !c.is_alphanumeric()) {
        format!("{helper}({buf}, {cast}({val}));")
    } else {
        format!("{helper}({buf}, {cast}{val});")
    };
    if ctx.js_helpers.was_added(JsGroup::Other, helper) {
        return result;
    }
    let val_ty = match ty {
        U64 | I64 => "bigint",
        _ => "number",
    };
    let is_le = if size == 1 { "" } else { ", true" };
    let js = JsString::from_template(&format!(
        "
function {helper}(buf«: _ffi_WriteBuf», val«: {val_ty}»)«: void» {{
    let ptr = _ffi_grow(buf, {size});
    buf.dv«!».{method}(ptr, val{is_le});
}}
"
    ));
    ctx.js_helpers
        .add_group(JsGroup::Other, helper, js)
        .add_dep_group(JsGroup::Other, "_ffi_WriteBuf")
        .add_dep_group(JsGroup::Other, "_ffi_grow")
        .mark_used();
    result
}

fn js_read(ctx: &mut WasmCtx, buf: &str, ty: &RustType) -> String {
    use RustType::*;
    let mut prefix = "";
    let (size, method, helper) = match ty {
        Pair { other, .. } => return js_read(ctx, buf, other),
        Bool => {
            prefix = "!!";
            (1, "getUint8", "_ffi_read_u8")
        }
        U8 => (1, "getUint8", "_ffi_read_u8"),
        I8 => (1, "getInt8", "_ffi_read_i8"),
        U16 => (2, "getUint16", "_ffi_read_u16"),
        I16 => (2, "getInt16", "_ffi_read_i16"),
        U32 | Usize => (4, "getUint32", "_ffi_read_u32"),
        I32 | Isize | ForeignHandle => (4, "getInt32", "_ffi_read_i32"),
        U64 => (8, "setBigUint64", "_ffi_read_u64"),
        I64 => (8, "setBigInt64", "_ffi_read_i64"),
        F32 => (4, "getFloat32", "_ffi_read_f32"),
        F64 => (8, "getFloat64", "_ffi_read_f64"),
        _ => unreachable!(),
    };
    let result = format!("{prefix}{helper}({buf})");
    if ctx.js_helpers.was_added(JsGroup::Other, helper) {
        return result;
    }
    let val_ty = match ty {
        U64 | I64 => "bigint",
        _ => "number",
    };
    let mut js = JsString::from_template(&format!(
        "\nfunction {helper}(buf«: _ffi_ReadBuf»)«: {val_ty}» {{\n"
    ));
    if size == 1 {
        _ = write!(js, "    return buf.dv.{method}({buf}.off++);\n");
    } else {
        let is_le = if size == 1 { "" } else { ", true" };
        _ = write!(js, "    let val = buf.dv.{method}({buf}.off{is_le});\n");
        _ = write!(js, "    buf.off += {size};\n");
        js.push_both("    return val;\n");
    }
    js.push_both("}\n");
    ctx.js_helpers
        .add_group(JsGroup::Other, helper, js)
        .add_dep_group(JsGroup::Other, "_ffi_ReadBuf")
        .mark_used();
    result
}

fn append_ts_type(ts: &mut JsString, ast: &AST, ty: &RustType) {
    use RustType::*;

    #[derive(Eq, PartialEq)]
    enum Parent {
        Top,
        Vector,
        Optional,
    }

    fn append(ts: &mut JsString, ast: &AST, ty: &RustType, parent: Parent) {
        match ty {
            Pair { other, .. } => append(ts, ast, other, parent),
            Verbatim(text) => ts.push_ts(text),
            Bool => ts.push_ts("boolean"),
            U8 | U16 | U32 | Usize => ts.push_ts("number"),
            I8 | I16 | I32 | Isize => ts.push_ts("number"),
            F32 | F64 | ForeignHandle => ts.push_ts("number"),
            U64 | I64 => ts.push_ts("bigint"),
            RefStr | OwnStr => ts.push_ts("string"),
            Struct(index) => ts.push_ts(&ast.structs[*index].name),
            Enum(index) => ts.push_ts(&ast.enums[*index].name),
            DynTrait(index) => ts.push_ts(&ast.traits[*index].name),
            Ptr(_, inner) => append(ts, ast, inner, parent),
            Vector(inner) => {
                append(ts, ast, &inner, Parent::Vector);
                ts.push_ts("[]");
            }
            Tuple(types) if types.is_empty() => ts.push_ts("undefined"),
            Tuple(types) => {
                ts.push_ts("[");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        ts.push_ts(", ");
                    }
                    append(ts, ast, ty, Parent::Top);
                }
                ts.push_ts("]");
            }
            Optional(inner) if parent == Parent::Optional => append(ts, ast, inner, parent),
            Optional(inner) => {
                let wrap = parent == Parent::Vector;
                if wrap {
                    ts.push_ts("(");
                }
                append(ts, ast, &inner, Parent::Optional);
                ts.push_ts(" | null");
                if wrap {
                    ts.push_ts(")");
                }
            }
        }
    }

    append(ts, ast, ty, Parent::Top)
}

fn append_js_val(js: &mut JsString, val: &RustVal) {
    use RustVal::*;
    match val {
        Bool(x) => _ = write!(js, "{x}"),

        U8(x) => _ = write!(js, "{x}"),
        U16(x) => _ = write!(js, "{x}"),
        U32(x) => _ = write!(js, "{x}"),
        U64(x) => _ = write!(js, "{x}n"),

        I8(x) => _ = write!(js, "{x}"),
        I16(x) => _ = write!(js, "{x}"),
        I32(x) => _ = write!(js, "{x}"),
        I64(x) => _ = write!(js, "{x}n"),

        F32(x) => _ = write!(js, "{}", *x as f64),
        F64(x) => _ = write!(js, "{x}"),

        Str(x) => append_js_quoted(js, x),
    }
}

fn append_js_quoted(js: &mut JsString, text: &str) {
    js.push_both("\"");
    for c in text.chars() {
        match c {
            '\t' => js.push_both("\\t"),
            '\n' => js.push_both("\\n"),
            '\r' => js.push_both("\\r"),
            '\\' => js.push_both("\\\\"),
            '\"' => js.push_both("\\\""),
            c if (c as u32) < 0x20 => _ = write!(js, "\\x{:02x}", c as u32),
            _ => _ = write!(js, "{c}"),
        }
    }
    js.push_both("\"");
}

fn member_access(object: &str, member: &str) -> String {
    match starts_with_digit(member) {
        false => format!("{object}.{member}"),
        true => format!("{object}[{member}]"),
    }
}

impl FnBuilder {
    fn write_to_js(&mut self, ast: &AST, out: &mut JsString, base_indent: &str) {
        let callback = |line: &str, out: &mut String, indent: &mut usize| {
            if line.starts_with(&['}', ']', ')']) {
                *indent -= 1;
            }
            _ = write!(out, "{base_indent}{}{line}\n", "    ".repeat(*indent));
            if line.ends_with(&['{', '[', '(']) {
                *indent += 1;
            }
        };
        let mut js_indent = 0;
        let mut ts_indent = 0;
        for line in self.take_lines() {
            match line {
                Line::Plain(text) => {
                    for line in text.split('\n') {
                        callback(line, &mut out.without_types, &mut js_indent);
                        callback(line, &mut out.with_types, &mut ts_indent);
                    }
                }
                Line::PlainAlt(when_false, when_true, flag) => {
                    let text = match flag.get() {
                        false if when_false.is_empty() => continue,
                        false => when_false,
                        true => when_true,
                    };
                    for line in text.split('\n') {
                        callback(line, &mut out.without_types, &mut js_indent);
                        callback(line, &mut out.with_types, &mut ts_indent);
                    }
                }
                Line::Decl(name, ty, text) => {
                    for line in format!("let {name} = {text};").split('\n') {
                        callback(line, &mut out.without_types, &mut js_indent);
                        if ty.is_none() {
                            callback(line, &mut out.with_types, &mut ts_indent);
                        }
                    }

                    // Handle optional type annotations separately
                    if let Some(ty) = ty {
                        let mut decl_ty = JsString::default();
                        append_ts_type(&mut decl_ty, ast, &ty);
                        let text = format!("let {name}: {} = {text};", decl_ty.with_types);
                        for line in text.split('\n') {
                            callback(line, &mut out.with_types, &mut js_indent);
                        }
                    }
                }
            }
        }
    }
}
