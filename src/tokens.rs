use crate::tokenizer::TokenKind;
use std::collections::HashMap;
use std::sync::OnceLock;

pub fn keywords_map() -> &'static HashMap<&'static str, TokenKind> {
    static KEYWORDS: OnceLock<HashMap<&'static str, TokenKind>> = OnceLock::new();
    KEYWORDS.get_or_init(|| {
        let mut map = HashMap::new();
        map.insert("class", TokenKind::Class);
        map.insert("const", TokenKind::Const);
        map.insert("else", TokenKind::Else);
        map.insert("if", TokenKind::If);
        map.insert("let", TokenKind::Let);
        map.insert("function", TokenKind::Function);
        map.insert("var", TokenKind::Var);
        map.insert("for", TokenKind::For);
        map.insert("while", TokenKind::While);
        map.insert("do", TokenKind::Do);
        map.insert("switch", TokenKind::Switch);
        map.insert("case", TokenKind::Case);
        map.insert("default", TokenKind::Default);
        map.insert("break", TokenKind::Break);
        map.insert("continue", TokenKind::Continue);
        map.insert("return", TokenKind::Return);
        map.insert("try", TokenKind::Try);
        map.insert("catch", TokenKind::Catch);
        map.insert("finally", TokenKind::Finally);
        map.insert("throw", TokenKind::Throw);
        map.insert("new", TokenKind::New);
        map.insert("delete", TokenKind::Delete);
        map.insert("typeof", TokenKind::TypeOf);
        map.insert("instanceof", TokenKind::InstanceOf);
        map.insert("void", TokenKind::Void);
        map.insert("this", TokenKind::This);
        map.insert("super", TokenKind::Super);
        map.insert("import", TokenKind::Import);
        map.insert("export", TokenKind::Export);
        map.insert("async", TokenKind::Async);
        map
    })
}
