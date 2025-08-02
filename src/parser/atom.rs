use crate::{
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::Parser;

impl<'a> Parser<'a> {
    pub fn parse_atom(&mut self) -> Option<Atom> {
        self.tok_look_ahead().and_then(|token| {
            (match token.kind {
                TokenKind::Identifier => Some(AtomKind::Identifier),
                TokenKind::NumberLiteral => Some(AtomKind::NumberLiteral),
                TokenKind::StringLiteral => Some(AtomKind::StringLiteral),
                TokenKind::BooleanLiteral => Some(AtomKind::BooleanLiteral),
                _ => None,
            })
            .map(|atom_kind| {
                // consume the token
                self.tokenizer.next();
                Atom {
                    span: token.span,
                    kind: atom_kind,
                }
            })
        })
    }
}

#[derive(Debug)]
pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
}

#[derive(Debug)]
pub enum AtomKind {
    Identifier,
    NumberLiteral,
    StringLiteral,
    BooleanLiteral,
}

impl Node for Atom {
    fn span(&self) -> Span {
        self.span
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_atom_identifier() {
        let source = "myVar";
        let mut parser = Parser::new(source);
        let atom = parser.parse_atom().unwrap();

        assert_eq!(atom.span().start.index, 0);
        assert_eq!(atom.span().end.index, 5);
        assert!(matches!(atom.kind, AtomKind::Identifier));
    }

    #[test]
    fn test_parse_atom_literal_number() {
        let source = "123";
        let mut parser = Parser::new(source);
        let atom = parser.parse_atom().unwrap();

        assert_eq!(atom.span.start.index, 0);
        assert_eq!(atom.span.end.index, 3);
        assert!(matches!(atom.kind, AtomKind::NumberLiteral));
    }

    #[test]
    fn test_parse_atom_literal_string() {
        let source = "\"hello\"";
        let mut parser = Parser::new(source);
        let atom = parser.parse_atom().unwrap();
        assert_eq!(atom.span.start.index, 0);
        assert_eq!(atom.span.end.index, 7);
        assert!(matches!(atom.kind, AtomKind::StringLiteral));
    }
    #[test]
    fn test_parse_atom_literal_boolean() {
        let source = "true";
        let mut parser = Parser::new(source);
        let atom = parser.parse_atom().unwrap();
        assert_eq!(atom.span.start.index, 0);
        assert_eq!(atom.span.end.index, 4);
        assert!(matches!(atom.kind, AtomKind::BooleanLiteral));
    }
}
