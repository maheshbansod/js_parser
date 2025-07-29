use tokenizer::Tokenizer;

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

    pub fn parse(mut self) -> ParseTree {
        self.tokenizer.next();
        todo!()
    }
}

type ParseTree = ();

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
}
