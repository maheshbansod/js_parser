use crate::{
    Parser, Token,
    node::Node,
    parser::expression::Expression,
    tokenizer::{Span, TokenKind},
};

use super::{Statement, StatementKind};

impl<'a> Parser<'a> {
    pub fn parse_if_statement(&mut self) -> Option<Statement> {
        let start_token = self.consume_token_if(TokenKind::If)?;
        self.consume_token_if(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume_token_if(TokenKind::RParen)?;
        let consequent = self.parse_statement()?;
        let alternate = self
            .consume_token_if(TokenKind::Else)
            .and_then(|_| self.parse_statement().map(Box::new));
        Some(Statement {
            kind: StatementKind::IfStatement(IfStatement {
                start_token,
                condition,
                consequent: Box::new(consequent),
                alternate,
            }),
        })
    }
}

impl Node for IfStatement {
    fn span(&self) -> crate::tokenizer::Span {
        let if_stmt = self;
        let start = if_stmt.start_token.span;
        let end = if_stmt
            .alternate
            .as_ref()
            .map(|a| a.span())
            .unwrap_or_else(|| if_stmt.consequent.span());
        Span::concat(start, end)
    }
}

#[derive(Debug)]
pub struct IfStatement {
    start_token: Token,
    pub condition: Expression,
    consequent: Box<Statement>,
    alternate: Option<Box<Statement>>,
}

#[cfg(test)]
mod tests {
    use crate::parser::atom::AtomKind;
    use crate::parser::expression::Expression;
    use crate::parser::expression::term::Term;
    use crate::parser::statement::StatementKind;
    use crate::parser::statement::tests::parse_and_check;
    #[test]
    fn test_parse_if_statement_basic() {
        let stmt = parse_and_check("if (true) {}", 0, 12);
        if let StatementKind::IfStatement(if_stmt) = stmt.kind {
            assert!(
                matches!(if_stmt.condition, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::BooleanLiteral))
            );
            if let StatementKind::BlockStatement(block) = if_stmt.consequent.kind {
                assert!(block.body.is_empty());
            } else {
                panic!("Expected BlockStatement for consequent");
            }
            assert!(if_stmt.alternate.is_none());
        } else {
            panic!("Expected IfStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_if_statement_with_body() {
        let stmt = parse_and_check("if (false) { 1; }", 0, 17);
        if let StatementKind::IfStatement(if_stmt) = stmt.kind {
            assert!(
                matches!(if_stmt.condition, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::BooleanLiteral))
            );
            if let StatementKind::BlockStatement(block) = if_stmt.consequent.kind {
                assert_eq!(block.body.len(), 1);
                assert!(matches!(
                    block.body[0].kind,
                    StatementKind::ExpressionStatement(_)
                ));
            } else {
                panic!("Expected BlockStatement for consequent");
            }
            assert!(if_stmt.alternate.is_none());
        } else {
            panic!("Expected IfStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_if_else_statement() {
        let stmt = parse_and_check("if (a) { b; } else { c; }", 0, 25);
        if let StatementKind::IfStatement(if_stmt) = stmt.kind {
            assert!(
                matches!(if_stmt.condition, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
            if let StatementKind::BlockStatement(consequent_block) = if_stmt.consequent.kind {
                assert_eq!(consequent_block.body.len(), 1);
            } else {
                panic!("Expected BlockStatement for consequent");
            }
            assert!(if_stmt.alternate.is_some());
            if let Some(alternate_stmt) = if_stmt.alternate {
                if let StatementKind::BlockStatement(alternate_block) = alternate_stmt.kind {
                    assert_eq!(alternate_block.body.len(), 1);
                } else {
                    panic!("Expected BlockStatement for alternate");
                }
            }
        } else {
            panic!("Expected IfStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_if_else_if_else_statement() {
        let stmt = parse_and_check("if (a) { b; } else if (c) { d; } else { e; }", 0, 44);
        if let StatementKind::IfStatement(if_stmt) = stmt.kind {
            assert!(
                matches!(if_stmt.condition, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
            if let StatementKind::BlockStatement(consequent_block) = if_stmt.consequent.kind {
                assert_eq!(consequent_block.body.len(), 1);
            } else {
                panic!("Expected BlockStatement for consequent");
            }

            assert!(if_stmt.alternate.is_some());
            if let Some(alternate_stmt) = if_stmt.alternate {
                if let StatementKind::IfStatement(nested_if_stmt) = alternate_stmt.kind {
                    assert!(
                        matches!(nested_if_stmt.condition, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::Identifier))
                    );
                    if let StatementKind::BlockStatement(nested_consequent_block) =
                        nested_if_stmt.consequent.kind
                    {
                        assert_eq!(nested_consequent_block.body.len(), 1);
                    } else {
                        panic!("Expected BlockStatement for nested consequent");
                    }
                    assert!(nested_if_stmt.alternate.is_some());
                    if let Some(final_alternate_stmt) = nested_if_stmt.alternate {
                        if let StatementKind::BlockStatement(final_alternate_block) =
                            final_alternate_stmt.kind
                        {
                            assert_eq!(final_alternate_block.body.len(), 1);
                        } else {
                            panic!("Expected BlockStatement for final alternate");
                        }
                    }
                } else {
                    panic!("Expected nested IfStatement, got {:?}", alternate_stmt.kind);
                }
            }
        } else {
            panic!("Expected IfStatement, got {:?}", stmt.kind);
        }
    }
}
