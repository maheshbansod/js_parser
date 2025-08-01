use crate::tokenizer::Span;

pub trait Node {
    fn span(&self) -> Span;
}
