use std::sync::OnceLock;

use crate::tokenizer::TokenKind;

pub fn operators_map() -> &'static Vec<(&'static str, TokenKind)> {
    static OPERATORS: OnceLock<Vec<(&'static str, TokenKind)>> = OnceLock::new();
    OPERATORS.get_or_init(|| {
        let mut ops = vec![
            // Punctuators / Delimiters
            ("...", TokenKind::Ellipsis),
            ("(", TokenKind::LParen),
            (")", TokenKind::RParen),
            ("{", TokenKind::LBrace),
            ("}", TokenKind::RBrace),
            ("[", TokenKind::LBracket),
            ("]", TokenKind::RBracket),
            (".", TokenKind::Dot),
            (",", TokenKind::Comma),
            (";", TokenKind::Semicolon),
            (":", TokenKind::Colon),
            ("?", TokenKind::Question),
            // Operators
            ("===", TokenKind::EqEqEq),
            ("!==", TokenKind::BangEqEq),
            (">>>", TokenKind::GtGtGt),
            ("**=", TokenKind::StarStarEq),
            ("<<=", TokenKind::LtLtEq),
            (">>=", TokenKind::GtGtEq),
            (">>>=", TokenKind::GtGtGtEq),
            ("??=", TokenKind::QuestionQuestionEq),
            ("=>", TokenKind::Arrow),
            ("??", TokenKind::QuestionQuestion),
            ("**", TokenKind::StarStar),
            ("++", TokenKind::PlusPlus),
            ("--", TokenKind::MinusMinus),
            ("==", TokenKind::EqEq),
            ("!=", TokenKind::BangEq),
            ("<=", TokenKind::LtEq),
            (">=", TokenKind::GtEq),
            ("&&", TokenKind::AmpAmp),
            ("||", TokenKind::PipePipe),
            ("<<", TokenKind::LtLt),
            (">>", TokenKind::GtGt),
            ("+=", TokenKind::PlusEq),
            ("-=", TokenKind::MinusEq),
            ("*=", TokenKind::StarEq),
            ("/=", TokenKind::SlashEq),
            ("%=", TokenKind::PercentEq),
            ("&=", TokenKind::AmpEq),
            ("|=", TokenKind::PipeEq),
            ("^=", TokenKind::CaretEq),
            ("+", TokenKind::Plus),
            ("-", TokenKind::Minus),
            ("*", TokenKind::Star),
            ("/", TokenKind::Slash),
            ("%", TokenKind::Percent),
            ("!", TokenKind::Bang),
            ("&", TokenKind::Amp),
            ("|", TokenKind::Pipe),
            ("^", TokenKind::Caret),
            ("~", TokenKind::Tilde),
            ("<", TokenKind::Lt),
            (">", TokenKind::Gt),
            ("=", TokenKind::Eq),
        ];

        // Sort by length in descending order to prioritize longer matches
        ops.sort_unstable_by(|a, b| b.0.len().cmp(&a.0.len()));
        ops
    })
}

