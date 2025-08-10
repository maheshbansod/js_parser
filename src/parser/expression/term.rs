use crate::{node::Node, parser::{atom::Atom, expression::function_definition::FunctionDefinition}, Parser};

impl<'a> Parser<'a> {
    pub fn parse_term(&mut self) -> Option<Term> {
        self.parse_atom().map(Term::Atom).or_else(|| {
            self.parse_function_definition().map(Term::FunctionDefinition)
        })
    }
}

#[derive(Debug)]
pub enum Term {
    Atom(Atom),
    FunctionDefinition(FunctionDefinition),
}

impl Node for Term {
    fn span(&self) -> crate::tokenizer::Span {
        match self {
            Term::Atom(atom) => atom.span(),
            Term::FunctionDefinition(func_def) => func_def.span(),
        }
    }
}