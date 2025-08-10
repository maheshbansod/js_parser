mod atom;
mod expression;
mod statement;

use crate::{
    Token,
    tokenizer::{TokenKind, Tokenizer},
};

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
        let mut statements = Vec::new();
        while !self.tokenizer.is_end_of_file() {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            } else {
                // Handle unexpected tokens or errors
                self.tokenizer.next(); // Skip the unexpected token
            }
        }
    }

    fn tok_look_ahead(&self) -> Option<Token> {
        let mut it_clone = self.tokenizer.clone();
        it_clone.next()
    }

    fn consume_token_if(&mut self, token_kind: TokenKind) -> Option<Token> {
        self.tok_look_ahead()
            .filter(|t| t.kind == token_kind)
            .and_then(|_| self.tokenizer.next())
    }
}

// will implement this when we get to it
type ParseTree = ();

#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {
        assert_eq!(1 + 1, 2);
    }
}
