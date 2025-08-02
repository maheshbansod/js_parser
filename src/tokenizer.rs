use std::str::CharIndices;

use crate::{operators::operators_map, tokens::keywords_map};

#[derive(Clone, Debug)]
pub struct Tokenizer<'a> {
    it: CharIndices<'a>,

    line: usize,
    column: usize,
    index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        let it = source.char_indices();
        Self {
            it,
            line: 0,
            column: 0,
            index: 0,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        // whitespace consumption
        self.consume_whitespace();
        self.consume_keyword()
            .or_else(|| self.consume_literal())
            .or_else(|| self.consume_identifier())
            .or_else(|| self.consume_operator())
    }

    fn consume_whitespace(&mut self) {
        self.consume_till(|c| !c.is_whitespace());
    }

    fn consume_keyword(&mut self) -> Option<Token> {
        for (keyword_str, keyword_token_kind) in keywords_map().iter() {
            if let Some(token) = self.consume_as_keyword(keyword_str, *keyword_token_kind) {
                return Some(token);
            }
        }
        None
    }

    fn consume_literal(&mut self) -> Option<Token> {
        let start = self.current_position();
        (if let Some(span) = self.consume_till(|c| !c.is_numeric()) {
            Some((span, TokenKind::NumberLiteral))
        } else if let Some((_quote_start_span, quote_str)) = self
            .consume_string("\"")
            .map(|s| (s, "\""))
            .or_else(|| self.consume_string("'").map(|s| (s, "'")))
        {
            let literal_span = self
                .consume_till(|c| quote_str.starts_with(c))
                .map(|_| {
                    let _end_quote = self.consume_string(quote_str);
                    let end = self.current_position();
                    Span {
                        start: start.clone(),
                        end,
                    }
                })
                .unwrap_or_else(|| Span {
                    start: start.clone(),
                    end: self.current_position(),
                });
            Some((literal_span, TokenKind::StringLiteral))
        } else if let Some(span) = self.consume_word("true") {
            Some((span, TokenKind::BooleanLiteral))
        } else if let Some(span) = self.consume_word("false") {
            Some((span, TokenKind::BooleanLiteral))
        } else {
            None
        })
        .map(|(span, token_kind)| Token {
            span,
            kind: token_kind,
        })
    }

    fn consume_identifier(&mut self) -> Option<Token> {
        self.consume_alphanumeric().map(|span| Token {
            span,
            kind: TokenKind::Identifier,
        })
    }

    fn consume_operator(&mut self) -> Option<Token> {
        for (operator_str, operator_token_kind) in operators_map().iter() {
            if let Some(token) = self.consume_as_operator(operator_str, *operator_token_kind) {
                return Some(token);
            }
        }
        None
    }

    fn consume_as_keyword(&mut self, s: &str, kind: TokenKind) -> Option<Token> {
        self.consume_word(s).map(|span| Token { span, kind })
    }

    fn consume_as_operator(&mut self, s: &str, kind: TokenKind) -> Option<Token> {
        self.consume_till_if_matches(|c| c.is_whitespace() || c.is_alphanumeric(), s)
            .map(|span| Token { span, kind })
    }

    fn consume_alphanumeric(&mut self) -> Option<Span> {
        self.consume_till(|c| !c.is_alphanumeric())
    }

    /// Consumes if the upcoming word (till word boundary reached)
    /// is equal to the given string
    fn consume_word(&mut self, s: &str) -> Option<Span> {
        self.consume_till_if_matches(|c| !c.is_alphanumeric() && c != '_', s)
    }

    fn consume_till_if_matches<C>(&mut self, condition: C, s: &str) -> Option<Span>
    where
        C: Fn(char) -> bool,
    {
        let mut it_clone = self.it.clone();
        for c in s.chars() {
            if let Some((_i, next_c)) = it_clone.next() {
                if condition(next_c) || c != next_c {
                    return None;
                }
            } else {
                return None;
            }
        }
        if let Some((_, next_c)) = it_clone.next() {
            if !condition(next_c) {
                return None;
            }
        }
        let move_by = s.len();
        let span = self.move_by(move_by);
        Some(span)
    }

    fn consume_string(&mut self, s: &str) -> Option<Span> {
        let mut it_clone = self.it.clone();
        for c in s.chars() {
            if let Some((_i, next_c)) = it_clone.next() {
                if c != next_c {
                    return None;
                }
            } else {
                return None;
            }
        }
        let move_by = s.len();
        let span = self.move_by(move_by);
        Some(span)
    }

    /// Consumes till the condition becomes true i.e. stops consuming when the given condition
    /// becomes true
    fn consume_till<C>(&mut self, condition: C) -> Option<Span>
    where
        C: Fn(char) -> bool,
    {
        let it_clone = self.it.clone();
        let mut characters_to_consume = 0;
        for (_i, c) in it_clone {
            if condition(c) {
                break;
            }
            characters_to_consume += 1;
        }
        (characters_to_consume > 0).then(|| self.move_by(characters_to_consume))
    }

    fn move_by(&mut self, by: usize) -> Span {
        let start = self.current_position();
        for _ in 0..by {
            if let Some((_i, c)) = self.it.next() {
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += 1;
                }
            } else {
                break;
            }
            self.index += 1;
        }
        let end = self.current_position();
        Span { start, end }
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
            index: self.index,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn concat(a: Span, b: Span) -> Self {
        Self {
            start: a.start,
            end: b.end,
        }
    }
    pub fn source<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start.index..self.end.index]
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[allow(dead_code)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum TokenKind {
    Class,
    Const,
    Else,
    If,
    Let,
    Function,
    Var,
    For,
    While,
    Do,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Return,
    Try,
    Catch,
    Finally,
    Throw,
    New,
    Delete,
    TypeOf,
    InstanceOf,
    Void,
    This,
    Super,
    Import,
    Export,
    Async,
    Await,
    Yield,
    Debugger,
    With,
    Extends,
    Implements,   // Reserved
    Interface,    // Reserved
    Package,      // Reserved
    Private,      // Reserved
    Protected,    // Reserved
    Public,       // Reserved
    Static,       // Reserved
    Abstract,     // Reserved
    Boolean,      // Reserved
    Byte,         // Reserved
    Char,         // Reserved
    Double,       // Reserved
    Final,        // Reserved
    Float,        // Reserved
    Goto,         // Reserved
    Int,          // Reserved
    Long,         // Reserved
    Native,       // Reserved
    Short,        // Reserved
    Synchronized, // Reserved
    Throws,       // Reserved
    Transient,    // Reserved
    Volatile,     // Reserved
    Enum,         // Reserved

    // Operators
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Percent,            // %
    StarStar,           // **
    PlusPlus,           // ++
    MinusMinus,         // --
    EqEq,               // ==
    BangEq,             // !=
    EqEqEq,             // ===
    BangEqEq,           // !==
    Lt,                 // <
    LtEq,               // <=
    Gt,                 // >
    GtEq,               // >=
    AmpAmp,             // &&
    PipePipe,           // ||
    Bang,               // !
    Amp,                // &
    Pipe,               // |
    Caret,              // ^
    Tilde,              // ~
    LtLt,               // <<
    GtGt,               // >>
    GtGtGt,             // >>>
    Eq,                 // =
    PlusEq,             // +=
    MinusEq,            // -=
    StarEq,             // *=
    SlashEq,            // /=
    PercentEq,          // %=
    StarStarEq,         // **=
    LtLtEq,             // <<=
    GtGtEq,             // >>=
    GtGtGtEq,           // >>>=
    AmpEq,              // &=
    PipeEq,             // |=
    CaretEq,            // ^=
    QuestionQuestion,   // ??
    QuestionQuestionEq, // ??=
    Arrow,              // =>

    // Punctuators / Delimiters
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Dot,       // .
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :
    Question,  // ?
    Ellipsis,  // ...

    // Literals
    Identifier,
    NumberLiteral,
    StringLiteral,
    BooleanLiteral,   // true, false
    NullLiteral,      // null
    UndefinedLiteral, // undefined
    RegExpLiteral,
    TemplateLiteral, // `...`

    // Special
    Eof, // End of file
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_tokenizer_basic_keywords_identifiers() {
        let source = "let foo = bar;";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "foo"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Eq);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "bar"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Semicolon);
        assert!(tokenizer.next().is_none()); // Eof
    }

    #[test]
    fn test_tokenizer_string_literal() {
        let source = "let foo = \"string\";";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Eq);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::StringLiteral);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Semicolon);
        assert!(tokenizer.next().is_none()); // Eof
    }

    #[test]
    fn test_tokenizer_number_literal() {
        let source = "let foo = 1234;";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Eq);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::NumberLiteral);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Semicolon);
        assert!(tokenizer.next().is_none()); // Eof
    }

    #[test]
    fn test_tokenizer_operators_and_whitespace() {
        let source = "a + b - c";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "a"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Plus);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "b"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Minus);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "c"
        assert!(tokenizer.next().is_none()); // Eof
    }

    #[test]
    fn test_tokenizer_longest_operator_match() {
        let source = "a === b";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "a"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::EqEqEq); // Should be '===' not '=' then '=='
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "b"
        assert!(tokenizer.next().is_none()); // Eof
    }

    #[test]
    fn test_tokenizer_keyword_vs_identifier() {
        let source = "if_condition";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // Should be 'if_condition' as one identifier
        assert!(tokenizer.next().is_none()); // Eof
    }

    #[test]
    fn test_tokenizer_function_declaration() {
        let source = "function add(a, b) { return a + b; }";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Function);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "add"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::LParen);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "a"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Comma);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "b"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::RParen);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::LBrace);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Return);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "a"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Plus);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "b"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Semicolon);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::RBrace);
        assert!(tokenizer.next().is_none()); // Eof
    }
    #[test]
    fn test_tokenizer_boolean_literal() {
        let source = "let x = true; const y = false;";
        let mut tokenizer = Tokenizer::new(source);

        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "x"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Eq);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::BooleanLiteral); // "true"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Semicolon);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Const);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Identifier); // "y"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Eq);
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::BooleanLiteral); // "false"
        assert_eq!(tokenizer.next().unwrap().kind, TokenKind::Semicolon);
        assert!(tokenizer.next().is_none()); // Eof
    }
}
