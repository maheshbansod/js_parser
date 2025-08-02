use crate::{
    Token,
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::{Parser, expression::Expression};

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_expression_statement()
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        self.parse_expression().map(|exp| {
            let end_token = self
                .tok_look_ahead()
                .filter(|t| matches!(t.kind, TokenKind::Semicolon))
                .and_then(|_| self.tokenizer.next());
            Statement {
                kind: StatementKind::ExpressionStatement(exp),
                end_token,
            }
        })
    }
}

pub struct Statement {
    kind: StatementKind,
    end_token: Option<Token>,
}

#[derive(Debug)]
pub enum StatementKind {
    ExpressionStatement(Expression),
    // FunctionDefinition,
    // IfStatement,
}

impl Node for Statement {
    fn span(&self) -> crate::tokenizer::Span {
        match &self.kind {
            StatementKind::ExpressionStatement(expression) => {
                let exp_span = expression.span();
                self.end_token
                    .as_ref()
                    .map(|token| Span::concat(exp_span, token.span))
                    .unwrap_or_else(|| exp_span)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Statement, StatementKind};
    use crate::node::Node;
    use crate::parser::Parser;
    use crate::parser::atom::AtomKind;
    use crate::parser::expression::{BinaryOperatorKind, Expression, UnaryOperatorKind};
    use crate::tokenizer::TokenKind;

    // Helper to create a parser and parse a statement, checking its span
    fn parse_and_check(source: &str, expected_start: usize, expected_end: usize) -> Statement {
        let mut parser = Parser::new(source);
        let stmt = parser.parse_statement().expect("No statement found");
        println!("stmt: {:?}", stmt.span());
        assert_eq!(stmt.span().start.index, expected_start);
        assert_eq!(stmt.span().end.index, expected_end);
        stmt
    }

    #[test]
    fn test_parse_expression_statement_identifier() {
        let stmt = parse_and_check("myVar;", 0, 6); // Span of "myVar"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert!(stmt.end_token.is_some());
            assert_eq!(stmt.end_token.unwrap().kind, TokenKind::Semicolon);
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_number_literal() {
        let stmt = parse_and_check("123;", 0, 4); // Span of "123"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(stmt.end_token.is_some());
            assert_eq!(stmt.end_token.unwrap().kind, TokenKind::Semicolon);
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_binary_expression() {
        let stmt = parse_and_check("1 + 2;", 0, 6); // Span of "1 + 2"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            if let Expression::Binary(binary_expr) = expr {
                assert!(matches!(binary_expr.operator.kind, BinaryOperatorKind::Add));
                assert!(
                    matches!(binary_expr.left.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(binary_expr.right.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected Binary expression, got {:?}", expr);
            }
            assert!(stmt.end_token.is_some());
            assert_eq!(stmt.end_token.unwrap().kind, TokenKind::Semicolon);
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_unary_expression() {
        let stmt = parse_and_check("-foo;", 0, 5); // Span of "-foo"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            if let Expression::Unary(unary_expr) = expr {
                assert!(matches!(
                    unary_expr.operator.kind,
                    UnaryOperatorKind::Negate
                ));
                assert!(
                    matches!(unary_expr.operand.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
                );
            } else {
                panic!("Expected Unary expression, got {:?}", expr);
            }
            assert!(stmt.end_token.is_some());
            assert_eq!(stmt.end_token.unwrap().kind, TokenKind::Semicolon);
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_expression_statement_no_semicolon() {
        let stmt = parse_and_check("myVar", 0, 5); // Span of "myVar"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert!(stmt.end_token.is_none()); // Expect no semicolon
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }
}
