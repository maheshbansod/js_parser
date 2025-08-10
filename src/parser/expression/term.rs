use crate::{node::Node, parser::atom::Atom, Parser};

impl<'a> Parser<'a> {
    pub fn parse_term(&mut self) -> Option<Term> {
        self.parse_atom().map(Term::Atom)
    }
}

#[derive(Debug)]
pub enum Term {
    Atom(Atom),
}

impl Node for Term {
    fn span(&self) -> crate::tokenizer::Span {
        match self {
            Term::Atom(atom) => atom.span(),
        }
    }
}