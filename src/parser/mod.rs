mod atom;
mod expression;

use crate::{Token, tokenizer::Tokenizer};

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
        self.parse_expression();
        todo!()
    }

    fn tok_look_ahead(&self) -> Option<Token> {
        let mut it_clone = self.tokenizer.clone();
        it_clone.next()
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
