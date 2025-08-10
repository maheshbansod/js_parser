use crate::{Parser, parser::atom::Atom};

impl<'a> Parser<'a> {
    pub fn parse_term(&mut self) -> Option<Term> {
        self.parse_atom().map(Term::Atom)
    }
}

#[derive(Debug)]
pub enum Term {
    Atom(Atom),
}
