use crate::{
    Parser, Token,
    node::Node,
    parser::expression::Expression,
    tokenizer::{Span, TokenKind},
};

use super::{Statement, StatementKind};

impl<'a> Parser<'a> {
    pub fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_token = self.consume_token_if(TokenKind::Return)?;
        let value = self.parse_expression();
        Some(Statement {
            kind: StatementKind::ReturnStatement(ReturnStatement {
                return_token,
                value,
            }),
        })
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_token: Token,
    pub value: Option<Expression>,
}

impl Node for ReturnStatement {
    fn span(&self) -> crate::tokenizer::Span {
        if let Some(value) = self.value.as_ref() {
            Span::concat(self.return_token.span, value.span())
        } else {
            self.return_token.span
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::parser::atom::AtomKind;
    use crate::parser::expression::{BinaryExpression, BinaryOperatorKind, Expression};
    use crate::parser::statement::StatementKind;
    use crate::parser::statement::tests::parse_and_check;

    #[test]
    fn test_parse_return_statement_simple() {
        let source = "return;";
        let stmt = parse_and_check(source, 0, 6);
        if let StatementKind::ReturnStatement(return_stmt) = stmt.kind {
            assert!(return_stmt.value.is_none());
        } else {
            panic!("Expected ReturnStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_return_statement_with_literal() {
        let source = "return 123;";
        let stmt = parse_and_check(source, 0, 10);
        if let StatementKind::ReturnStatement(return_stmt) = stmt.kind {
            assert!(return_stmt.value.is_some());
            if let Some(expr) = return_stmt.value {
                assert!(
                    matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            }
        } else {
            panic!("Expected ReturnStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_return_statement_with_expression() {
        let source = "return a + b;";
        let stmt = parse_and_check(source, 0, 12);
        if let StatementKind::ReturnStatement(return_stmt) = stmt.kind {
            assert!(return_stmt.value.is_some());
            if let Some(expr) = return_stmt.value {
                if let Expression::Binary(binary_expr) = expr {
                    assert!(matches!(binary_expr.operator.kind, BinaryOperatorKind::Add));
                    assert!(
                        matches!(binary_expr.left.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
                    );
                    assert!(
                        matches!(binary_expr.right.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
                    );
                } else {
                    panic!(
                        "Expected Binary expression for return argument, got {:?}",
                        expr
                    );
                }
            }
        } else {
            panic!("Expected ReturnStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_return_statement_with_function_call() {
        let source = "return foo(1, 2);";
        let stmt = parse_and_check(source, 0, 16);
        if let StatementKind::ReturnStatement(return_stmt) = stmt.kind {
            assert!(return_stmt.value.is_some());
            if let Some(expr) = return_stmt.value {
                assert!(matches!(expr, Expression::FunctionCall(_)));
            }
        } else {
            panic!("Expected ReturnStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_return_statement_with_member_access() {
        let source = "return this.a;";
        let stmt = parse_and_check(source, 0, 13);
        if let StatementKind::ReturnStatement(return_stmt) = stmt.kind {
            assert!(return_stmt.value.is_some());
            if let Some(Expression::Binary(BinaryExpression {
                left,
                right,
                operator,
            })) = return_stmt.value
            {
                assert!(matches!(operator.kind, BinaryOperatorKind::MemberAccess));
                if let Expression::Atom(atom) = *left {
                    assert_eq!(atom.kind, AtomKind::This);
                } else {
                    panic!("left is supposed to be this, got {:?}", left);
                }
                if let Expression::Atom(atom) = *right {
                    assert_eq!(atom.kind, AtomKind::Identifier);
                } else {
                    panic!("right is supposed to be this, got {:?}", right);
                }
            } else {
                panic!("Supposed to be binary expression");
            }
        } else {
            panic!("Expected ReturnStatement, got {:?}", stmt.kind);
        }
    }
}
