use super::*;
use std::collections::HashSet;

const RUST_KEYWORDS: &[&str] = &[
    "abstract", "as", "async", "await", "become", "box", "break", "const", "continue", "crate",
    "do", "dyn", "else", "enum", "extern", "false", "final", "fn", "for", "gen", "if", "impl",
    "in", "let", "loop", "macro", "match", "mod", "move", "mut", "override", "priv", "pub", "ref",
    "return", "self", "Self", "static", "struct", "super", "trait", "true", "try", "type",
    "typeof", "unsafe", "unsized", "use", "virtual", "where", "while", "yield",
];

fn test_case() -> TestCase {
    let mut keywords = HashSet::<&str>::new();
    keywords.extend(crate::wasm::JS_KEYWORDS);
    keywords.extend(crate::cpp::CPP_KEYWORDS);
    keywords.extend(crate::swift::SWIFT_KEYWORDS);

    let mut keywords = keywords.into_iter().collect::<Vec<_>>();
    keywords.sort();

    let rust_keywords: HashSet<_> = RUST_KEYWORDS.iter().collect();
    let test_local = |k: &&&str| is_snake_case(k) && !rust_keywords.contains(k);
    let rust = keywords
        .iter()
        .filter(test_local)
        .map(|k| format!("pub fn test_{k}({k}: i32) -> i32 {{ {k} }}"))
        .collect::<Vec<_>>()
        .join("\n");

    let mut case = TestCase::default();
    case.rust = Box::new(rust).leak();

    for (i, k) in keywords.iter().filter(test_local).enumerate() {
        case.checks.push(AssertEqual(
            Call(
                Export(Box::new(format!("test_{k}")).leak()).into(),
                [I32(i as i32).into()].into(),
            )
            .into(),
            I32(i as i32).into(),
        ));
    }

    case
}

test_all!();
