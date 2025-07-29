use tokenizer::{Span, TokenKind, Tokenizer};

mod operators;
mod tokenizer;
mod tokens;

pub use tokenizer::Token;

pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::new(source),
        }
    }

    pub fn parse(&mut self) -> ParseTree {
        self.parse_atom();
        todo!()
    }

    fn parse_atom(&mut self) -> Option<Atom> {
        self.tok_look_ahead().and_then(|token| {
            (match token.kind {
                TokenKind::Identifier => Some(AtomKind::Identifier),
                TokenKind::NumberLiteral => Some(AtomKind::NumberLiteral),
                TokenKind::StringLiteral => Some(AtomKind::StringLiteral),
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

    fn tok_look_ahead(&self) -> Option<Token> {
        let mut it_clone = self.tokenizer.clone();
        it_clone.next()
    }
}

// will implement this when we get to it
type ParseTree = ();

pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
}

pub enum AtomKind {
    Identifier,
    NumberLiteral,
    StringLiteral,
}

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
    #[test]
    fn test_parse_atom_identifier() {
        let source = "myVar";
        let mut parser = Parser::new(source);
        let atom = parser.parse_atom().unwrap();

        assert_eq!(atom.span.start.index, 0);
        assert_eq!(atom.span.end.index, 5);
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
}
