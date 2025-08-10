use crate::{
    node::Node, parser::{atom::Atom, expression::{function_definition::FunctionDefinition, Expression}}, tokenizer::TokenKind, Parser
};

impl<'a> Parser<'a> {
    pub fn parse_term(&mut self) -> Option<Term> {
        self.parse_atom().map(Term::Atom).or_else(|| {
            self.parse_function_definition()
                .map(Term::FunctionDefinition)
        }).or_else(|| {
            self.tok_look_ahead().map(|token| {
                if token.kind == TokenKind::LParen {
                    self.tokenizer.next();
                    let expr = self.parse_expression();
                    self.consume_token_if(TokenKind::RParen);
                    expr.map(|expr| Term::BracketedExpression(Box::new(expr)))
                } else {
                    None
                }
            }).flatten()
        })
    }
}

#[derive(Debug)]
pub enum Term {
    Atom(Atom),
    FunctionDefinition(FunctionDefinition),
    /// Bracketed expression, e.g. (a + b)
    BracketedExpression(Box<Expression>),
}

impl Node for Term {
    fn span(&self) -> crate::tokenizer::Span {
        match self {
            Term::Atom(atom) => atom.span(),
            Term::FunctionDefinition(func_def) => func_def.span(),
            Term::BracketedExpression(expr) => expr.span(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::parser::expression::Expression;
    use crate::parser::expression::term::Term;

    #[test]
    fn test_parse_bracketed_expression() {
        let mut parser = Parser::new("(1 + 2)");
        let term = parser.parse_term().expect("No term found");
        match term {
            Term::BracketedExpression(expr) => {
                if let Expression::Binary(bin_expr) = expr.as_ref() {
                    assert!(
                        matches!(bin_expr.left.as_ref(), Expression::Term(Term::Atom(_)))
                    );
                    assert!(
                        matches!(bin_expr.right.as_ref(), Expression::Term(Term::Atom(_)))
                    );
                } else {
                    panic!("Expected binary expression inside brackets");
                }
            }
            _ => panic!("Expected BracketedExpression, got {:?}", term),
        }
    }
}