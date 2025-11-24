use super::*;
use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[derive(Clone, Default)]
pub struct NameSet {
    used: HashSet<String>,
}

impl NameSet {
    pub fn contains(&self, name: &str) -> bool {
        self.used.contains(name)
    }

    pub fn find(&self, base: &str) -> String {
        let mut name = base.to_string();
        let mut tries = match name.as_str() {
            "_" => 0,
            _ => 1,
        };
        while self.used.contains(&name) {
            tries += 1;
            name = format!("{base}{tries}");
        }
        name
    }

    pub fn add(&mut self, name: String) {
        self.used.insert(name);
    }

    pub fn create(&mut self, base: &str) -> String {
        let name = self.find(base);
        self.add(name.clone());
        name
    }
}

#[derive(Default)]
struct HelperFlags {
    is_used: bool,
    is_forward_decl: bool,
}

pub struct Helper<G: Copy + Debug + Eq + Hash + Ord, T> {
    code: T,
    deps: Vec<(G, String)>,
    forward_decls: Vec<(G, String)>,
    flags: RefCell<HelperFlags>,
}

impl<G: Copy + Debug + Eq + Hash + Ord, T> Helper<G, T> {
    pub fn add_dep_group(&mut self, group: G, name: &str) -> &mut Self {
        self.deps.push((group, name.into()));
        self
    }

    pub fn add_deps_group(&mut self, deps: HashSet<(G, String)>) -> &mut Self {
        let mut deps: Vec<_> = deps.into_iter().collect();
        deps.sort();
        self.deps.extend(deps.into_iter());
        self
    }

    pub fn add_forward_decl_group(&mut self, group: G, name: &str) -> &mut Self {
        self.forward_decls.push((group, name.into()));
        self
    }

    pub fn mark_used(&self) {
        self.flags.borrow_mut().is_used = true;
    }

    pub fn set_is_forward_decl(&self) {
        self.flags.borrow_mut().is_forward_decl = true;
    }
}

impl<T> Helper<(), T> {
    pub fn add_dep(&mut self, name: &str) -> &mut Self {
        self.deps.push(((), name.into()));
        self
    }
}

pub struct HelperSet<G: Copy + Debug + Eq + Hash + Ord, T> {
    map: HashMap<(G, String), Helper<G, T>>,
}

impl<G: Copy + Debug + Eq + Hash + Ord, T> Default for HelperSet<G, T> {
    fn default() -> HelperSet<G, T> {
        HelperSet {
            map: HashMap::new(),
        }
    }
}

impl<G: Copy + Debug + Eq + Hash + Ord, T> HelperSet<G, T> {
    pub fn was_added(&self, group: G, key: &str) -> bool {
        self.map.contains_key(&(group, key.into()))
    }

    pub fn add_group<I: Into<T>>(&mut self, group: G, name: &str, code: I) -> &mut Helper<G, T> {
        let key = (group, name.to_string());
        debug_assert!(
            !self.map.contains_key(&key),
            "{name:?} was added to group \"{group:?}\" more than once"
        );
        self.map.entry(key).or_insert(Helper {
            code: code.into(),
            deps: Vec::new(),
            forward_decls: Vec::new(),
            flags: HelperFlags::default().into(),
        })
    }

    pub fn mark_used_group(&mut self, group: G, name: &str) {
        get_key_in_map(&(group, name.into()), &self.map).mark_used();
    }

    pub fn mark_all_used_group(&mut self, all: HashSet<(G, String)>) {
        for key in all {
            get_key_in_map(&key, &self.map).mark_used();
        }
    }

    pub fn code_by_group_in_order(&self) -> HashMap<G, Vec<&T>> {
        enum VisitStatus {
            Visiting,
            Visited,
        }

        // Sort dependencies before dependents in case that matters
        fn visit<'a, G: Copy + Debug + Eq + Hash + Ord, T>(
            key: &'a (G, String),
            map: &'a HashMap<(G, String), Helper<G, T>>,
            result: &mut HashMap<G, Vec<&'a T>>,
            visits: &mut HashMap<&'a (G, String), VisitStatus>,
        ) {
            let helper = get_key_in_map(key, map);
            match visits.get(key) {
                Some(VisitStatus::Visited) => {}
                Some(VisitStatus::Visiting) => {
                    // Insert forward declarations as needed for C++
                    for dep in &helper.forward_decls {
                        visit(dep, map, result, visits);
                    }
                }
                None => {
                    visits.insert(key, VisitStatus::Visiting);
                    for dep in &helper.deps {
                        visit(dep, map, result, visits);
                    }
                    result
                        .entry(key.0)
                        .or_default()
                        .push(&map.get(key).unwrap().code);
                    visits.insert(key, VisitStatus::Visited);
                }
            }
        }

        let mut result = HashMap::new();
        let mut visits = HashMap::new();
        let mut keys: Vec<_> = self.map.keys().collect();
        keys.sort();
        for key in keys {
            if self.map.get(key).unwrap().flags.borrow().is_used {
                visit(key, &self.map, &mut result, &mut visits);
            }
        }
        result
    }
}

impl<T> HelperSet<(), T> {
    pub fn add<I: Into<T>>(&mut self, name: &str, code: I) -> &mut Helper<(), T> {
        self.add_group((), name, code)
    }

    pub fn mark_used(&mut self, name: &str) {
        self.mark_used_group((), name);
    }

    pub fn code_in_order(&self) -> Vec<&T> {
        self.code_by_group_in_order()
            .into_iter()
            .next()
            .unwrap_or_else(|| ((), Vec::new()))
            .1
    }
}

fn get_key_in_map<'a, G: Copy + Debug + Eq + Hash + Ord, T>(
    key: &(G, String),
    map: &'a HashMap<(G, String), Helper<G, T>>,
) -> &'a Helper<G, T> {
    debug_assert!(
        map.contains_key(key),
        "missing helper named {:?} in group \"{:?}\"",
        key.1,
        key.0
    );
    map.get(&key).unwrap()
}

// This is sort of like a language-agnostic AST (Abstract Syntax Tree). It lets
// us emit variable declarations in the "FnBuilder" and then optionally inline
// them into one or more of the following lines later on. For example, consider
// the following code:
//
//     let x = 1;
//     let y = 2;
//     let z = foo(x, y);
//     return z;
//
// That would be represented like this:
//
//     Decl("x", "1"),
//     Decl("y", "2"),
//     Decl("z", "foo(x, y)"),
//     Line("return z;"),
//
// When the code constructing each "Line" uses "RefInline", the result will
// become this (which is more concise and often easier to read):
//
//     return foo(1, 2);
//
// However, if some other code inserts something in the middle, then the sub-
// expressions will naturally be broken out into separate variable declarations.
// Consider something that needs to call "cleanup()" before returning:
//
//     Decl("x", "1"),
//     Decl("y", "2"),
//     Decl("z", "foo(x, y)"),
//     Line("cleanup();"),
//     Line("return z;"),
//
// Then when the code constructing each "Line" uses "RefInline", the result
// will become this instead:
//
//     let z = foo(1, 2);
//     cleanup();
//     return z;
//
#[derive(Eq, PartialEq)]
pub enum Line {
    Plain(String),

    // Two alternative lines (first when false, second when true)
    PlainAlt(String, String, Rc<Cell<bool>>),

    // Local declaration with an optional type annotation
    Decl(String, Option<RustType>, String),
}

pub struct Decl {
    pub code: String,
    pub pure: bool,
}

pub use RefKind::*;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum RefKind {
    // This should always be safe. Use this when a value may be used multiple
    // times. We can't inline the value into every use because that may have
    // side effects.
    RefMany,

    // Only use this when values are guaranteed to be used once in declaration
    // order. For example, this should not be used in C++ when there are
    // multiple sibling sub-expressions because C++ doesn't specify the
    // evaluation order of sub-expressions in a function call.
    RefInline,

    // C++ needs "std::move()" to handle non-copyable lvalues such as a local
    // variable holding a "std::unique_ptr".
    RefStdMove,
}

#[derive(Default)]
pub struct FnBuilder {
    lines: Vec<Line>,
    deferred_lines: Vec<Line>, // Deferred lines go at the end of the function
    pure: HashMap<String, String>,
}

impl FnBuilder {
    pub fn is_empty(&self) -> bool {
        self.lines.is_empty() && self.deferred_lines.is_empty()
    }

    pub fn take_lines(&mut self) -> Vec<Line> {
        self.lines.append(&mut self.deferred_lines);
        self.lines.drain(..).collect()
    }

    pub fn extend(&mut self, other: FnBuilder) {
        self.lines.extend(other.lines);
        self.deferred_lines.extend(other.deferred_lines);
        self.pure.extend(other.pure);
    }

    pub fn insert_deferred_lines_here(&mut self) {
        self.lines.append(&mut self.deferred_lines);
    }

    // Here "pure" means "can be duplicated, reordered, and/or removed" (e.g. a field access)
    pub fn mark_pure(&mut self, name: &str) {
        self.pure.insert(name.to_string(), name.to_string());
    }

    pub fn line(&mut self, text: String) {
        self.lines.push(Line::Plain(text));
    }

    pub fn line_alt(&mut self, when_false: String, when_true: String, flag: Rc<Cell<bool>>) {
        self.lines.push(Line::PlainAlt(when_false, when_true, flag));
    }

    pub fn decl<T: Into<String>>(&mut self, name: T, text: String) {
        self.lines.push(Line::Decl(name.into(), None, text));
    }

    pub fn decl_ty<T: Into<String>>(&mut self, name: T, ty: RustType, text: String) {
        self.lines.push(Line::Decl(name.into(), Some(ty), text));
    }

    pub fn pure_decl<T: Into<String>>(&mut self, name: T, text: String) {
        self.pure.insert(name.into(), text);
    }

    pub fn maybe_pure_decl<T: Debug + Into<String>>(&mut self, pure: bool, name: T, text: String) {
        if pure {
            self.pure_decl(name, text);
        } else {
            self.decl(name, text);
        }
    }

    pub fn defer_line(&mut self, text: String) {
        self.deferred_lines.push(Line::Plain(text));
    }

    pub fn defer_decl<T: Into<String>>(&mut self, name: T, text: String) {
        self.deferred_lines
            .push(Line::Decl(name.into(), None, text));
    }

    pub fn pop_line_if<F: FnOnce(&mut Line) -> bool>(&mut self, predicate: F) -> Option<Line> {
        self.lines.pop_if(predicate)
    }

    pub fn find(&mut self, name: &str, ty: &RustType, kind: RefKind) -> Decl {
        if let Some(code) = self.pure.get(name) {
            // This is an lvalue
            let code = if kind == RefStdMove && needs_std_move(ty) {
                format!("std::move({code})")
            } else {
                code.clone()
            };
            Decl { code, pure: true }
        } else if kind == RefInline
            && let Some(Line::Decl(last_name, _, last_code)) = self.lines.last()
            && last_name == name
        {
            // This is an rvalue
            let code = last_code.to_string();
            self.lines.pop();
            Decl { code, pure: false }
        } else {
            // This is an lvalue
            let code = if kind == RefStdMove && needs_std_move(ty) {
                format!("std::move({name})")
            } else {
                name.to_string()
            };
            Decl { code, pure: false }
        }
    }

    pub fn find_args(&mut self, args: &Vec<RustArg>, kind: RefKind) -> String {
        // Inline is ok if there's only one sub-expression
        let kind = if kind == RefMany && args.len() == 1 {
            RefInline
        } else {
            kind
        };

        // Inline in reverse so we pop the stack of args that were pushed earlier
        let mut parts = Vec::new();
        for arg in args.iter().rev() {
            parts.push(self.find(&arg.name, &arg.ty, kind).code);
        }
        parts.reverse();

        // Split up long multi-argument lists across multiple lines
        if parts.len() > 1 {
            maybe_split_across_lines(&parts, "", "")
        } else {
            parts.join(", ")
        }
    }

    pub fn find_fields(
        &mut self,
        fields: &Vec<RustField>,
        names: Vec<Cow<str>>,
        kind: RefKind,
        open: &str,
        close: &str,
    ) -> String {
        debug_assert_eq!(fields.len(), names.len());

        // Inline is ok if there's only one sub-expression
        let kind = if kind == RefMany && names.len() == 1 {
            RefInline
        } else {
            kind
        };

        // Inline in reverse so we pop the stack of args that were pushed earlier
        let mut parts = Vec::new();
        for (f, name) in fields.iter().rev().zip(names.iter().rev()) {
            let code = self.find(name, &f.ty, kind).code;
            parts.push(match f.name.as_str() {
                "" => code,
                _ => format!("{}: {}", f.name, code),
            });
        }
        parts.reverse();

        // Split up long lists of fields across multiple lines
        maybe_split_across_lines(&parts, open, close)
    }
}

fn maybe_split_across_lines(parts: &[String], mut open: &str, mut close: &str) -> String {
    let is_multi_line = parts.iter().any(|x| x.contains('\n'))
        || parts.iter().map(|x| x.len()).sum::<usize>() > 100;
    if is_multi_line {
        open = open.trim_ascii_end();
        close = close.trim_ascii_start();
    }
    let mut result: String = open.into();
    for (i, part) in parts.iter().enumerate() {
        result.push_str(match (is_multi_line, i > 0) {
            (false, false) => "",
            (false, true) => ", ",
            (true, false) => "\n",
            (true, true) => ",\n",
        });
        result.push_str(part);
    }
    if is_multi_line {
        result.push_str("\n");
    }
    result.push_str(close);
    result
}

fn needs_std_move(ty: &RustType) -> bool {
    use RustType::*;
    match ty {
        Pair { other, .. } => needs_std_move(other),
        Bool | U8 | U16 | U32 | Usize | U64 | I8 | I16 | I32 | Isize | I64 | F32 | F64
        | ForeignHandle => false,
        _ => true,
    }
}

// This is a copy of an old "Path" method in the Rust standard library that was
// never stabilized. It's copied here because there still isn't a way in Rust's
// standard library to compute the relative path between two paths.
pub fn path_relative_from(path: &Path, base: &Path) -> Option<PathBuf> {
    use std::path::Component;
    if path.is_absolute() != base.is_absolute() {
        if path.is_absolute() {
            Some(PathBuf::from(path))
        } else {
            None
        }
    } else {
        let mut ita = path.components();
        let mut itb = base.components();
        let mut comps: Vec<Component> = vec![];
        loop {
            match (ita.next(), itb.next()) {
                (None, None) => break,
                (Some(a), None) => {
                    comps.push(a);
                    comps.extend(ita.by_ref());
                    break;
                }
                (None, _) => comps.push(Component::ParentDir),
                (Some(a), Some(b)) if comps.is_empty() && a == b => (),
                (Some(a), Some(b)) if b == Component::CurDir => comps.push(a),
                (Some(_), Some(b)) if b == Component::ParentDir => return None,
                (Some(a), Some(_)) => {
                    comps.push(Component::ParentDir);
                    for _ in itb {
                        comps.push(Component::ParentDir);
                    }
                    comps.push(a);
                    comps.extend(ita.by_ref());
                    break;
                }
            }
        }
        Some(comps.iter().map(|c| c.as_os_str()).collect())
    }
}

pub struct SharedBuf {
    buf_name: String,
    end_name: String,
    is_buf_name_used: Rc<Cell<bool>>,
    is_end_name_used: Rc<Cell<bool>>,
}

impl SharedBuf {
    pub fn new(buf_name: &str, end_name: &str) -> Rc<SharedBuf> {
        Rc::new(SharedBuf {
            buf_name: buf_name.to_string(),
            end_name: end_name.to_string(),
            is_buf_name_used: Rc::new(Cell::new(false)),
            is_end_name_used: Rc::new(Cell::new(false)),
        })
    }

    pub fn buf_name(&self) -> &str {
        self.is_buf_name_used.set(true);
        &self.buf_name
    }

    pub fn end_name(&self) -> &str {
        self.is_end_name_used.set(true);
        &self.end_name
    }

    pub fn is_buf_name_used_flag(&self) -> Rc<Cell<bool>> {
        self.is_buf_name_used.clone()
    }

    pub fn is_end_name_used_flag(&self) -> Rc<Cell<bool>> {
        self.is_end_name_used.clone()
    }

    pub fn final_buf_name_for_rust(&self) -> &str {
        match self.is_buf_name_used.get() {
            false => "_", // Rust uses "_" to disable unused variable warnings
            true => &self.buf_name,
        }
    }

    pub fn final_end_name_for_rust(&self) -> &str {
        match self.is_end_name_used.get() {
            false => "_", // Rust uses "_" to disable unused variable warnings
            true => &self.end_name,
        }
    }
}

#[derive(Clone, Copy, Default, Eq, PartialEq)]
pub enum BufStatus {
    #[default]
    Outside,
    Inside,
}

pub fn starts_with_digit(name: &str) -> bool {
    name.chars().next().unwrap().is_ascii_digit()
}

pub fn with_digit_prefix(name: &str) -> Cow<'_, str> {
    if starts_with_digit(name) {
        Cow::Owned(format!("_{name}"))
    } else {
        Cow::Borrowed(name)
    }
}

pub fn name_for_match(name: &str, count: usize) -> Cow<'_, str> {
    match starts_with_digit(name) {
        false => Cow::Borrowed(name),
        true if count == 1 => Cow::Borrowed("x"),
        true => Cow::Owned(format!("x{}", name)),
    }
}
