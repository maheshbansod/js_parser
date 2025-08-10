use crate::{Parser, tokenizer::TokenKind};

use super::{Statement, StatementKind};

impl<'a> Parser<'a> {
    pub fn parse_expression_statement(&mut self) -> Option<Statement> {
        self.parse_expression().map(|exp| {
            let _end_token = self
                .tok_look_ahead()
                .filter(|t| matches!(t.kind, TokenKind::Semicolon))
                .and_then(|_| self.tokenizer.next());
            Statement {
                kind: StatementKind::ExpressionStatement(exp),
            }
        })
    }
}
#[cfg(test)]
mod tests {
    use crate::parser::{
        atom::AtomKind,
        expression::{BinaryOperatorKind, Expression, UnaryOperatorKind, term::Term},
        statement::tests::parse_and_check,
    };

    use super::*;

    #[test]
    fn test_parse_expression_statement_identifier() {
        let stmt = parse_and_check("myVar;", 0, 5); // Span of "myVar"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_number_literal() {
        let stmt = parse_and_check("123;", 0, 3); // Span of "123"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_binary_expression() {
        let stmt = parse_and_check("1 + 2;", 0, 5); // Span of "1 + 2"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            if let Expression::Binary(binary_expr) = expr {
                assert!(matches!(binary_expr.operator.kind, BinaryOperatorKind::Add));
                assert!(
                    matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected Binary expression, got {:?}", expr);
            }
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_unary_expression() {
        let stmt = parse_and_check("-foo;", 0, 4); // Span of "-foo"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            if let Expression::Unary(unary_expr) = expr {
                assert!(matches!(
                    unary_expr.operator.kind,
                    UnaryOperatorKind::Negate
                ));
                assert!(
                    matches!(unary_expr.operand.as_ref(), Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::Identifier))
                );
            } else {
                panic!("Expected Unary expression, got {:?}", expr);
            }
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_no_semicolon() {
        let stmt = parse_and_check("myVar", 0, 5); // Span of "myVar"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Term(Term::Atom( atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }
}
