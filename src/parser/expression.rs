use crate::{
    Token,
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::{Parser, atom::Atom};

/**
 * Term = Atom | BracketedExp
 * UnaryExp = (UnaryOp Term) | Term
 * BinaryExp
 *  = (UnaryExp BinaryOp BinaryExp) | UnaryExp
 * */

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_expression()
    }

    fn parse_binary_expression(&mut self) -> Option<Expression> {
        self.parse_unary_expression().and_then(|left_operand| {
            if let Some((op_token, op_kind, right_operand)) =
                self.tok_look_ahead().and_then(|token| {
                    match token.kind {
                        TokenKind::Plus => Some(BinaryOperatorKind::Add),
                        TokenKind::Minus => Some(BinaryOperatorKind::Subtract),
                        _ => None,
                    }
                    .and_then(|op_kind| {
                        let op_token = self.tokenizer.next().unwrap();
                        self.parse_expression()
                            .map(|right_operand| (op_token, op_kind, right_operand))
                    })
                })
            {
                Some(Expression::Binary(BinaryExpression {
                    left: Box::new(left_operand),
                    right: Box::new(right_operand),
                    operator: BinaryOperator {
                        token: op_token,
                        kind: op_kind,
                    },
                }))
            } else {
                Some(left_operand)
            }
        })
    }

    fn parse_unary_expression(&mut self) -> Option<Expression> {
        self.tok_look_ahead()
            .and_then(|token| {
                if let Some(kind) = match token.kind {
                    TokenKind::Plus => Some(UnaryOperatorKind::Plus),
                    TokenKind::Minus => Some(UnaryOperatorKind::Negate),
                    _ => None,
                } {
                    Some(UnaryOperator { kind, token })
                } else {
                    None
                }
            })
            .and_then(|operator| {
                self.tokenizer.next();
                self.parse_expression().map(|operand| {
                    Expression::Unary(UnaryExpression {
                        operand: Box::new(operand),
                        operator,
                    })
                })
            })
            .or_else(|| self.parse_atom().map(Expression::Atom))
    }
}

#[derive(Debug)]
pub enum Expression {
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Atom(Atom),
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: BinaryOperator,
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub operand: Box<Expression>,
    pub operator: UnaryOperator,
}

#[derive(Debug)]
pub struct BinaryOperator {
    pub token: Token,
    pub kind: BinaryOperatorKind,
}

#[derive(Debug)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
}

#[derive(Debug)]
pub struct UnaryOperator {
    pub token: Token,
    pub kind: UnaryOperatorKind,
}

#[derive(Debug)]
pub enum UnaryOperatorKind {
    Negate,
    Plus,
}

impl Operator for UnaryOperator {
    fn token(&self) -> &Token {
        &self.token
    }
}

impl Operator for BinaryOperator {
    fn token(&self) -> &Token {
        &self.token
    }
}

trait Operator {
    fn token(&self) -> &Token;
}

impl<T> Node for T
where
    T: Operator,
{
    fn span(&self) -> crate::tokenizer::Span {
        self.token().span
    }
}

impl Node for Expression {
    fn span(&self) -> crate::tokenizer::Span {
        match self {
            Expression::Binary(binary_expression) => binary_expression.span(),
            Expression::Unary(unary_expression) => unary_expression.span(),
            Expression::Atom(atom) => atom.span(),
        }
    }
}

impl Node for UnaryExpression {
    fn span(&self) -> crate::tokenizer::Span {
        Span::concat(self.operator.span(), self.operand.span())
    }
}

impl Node for BinaryExpression {
    fn span(&self) -> Span {
        // left operand is to the left of the operator
        // and right operand is to the right of the operator
        // so it makes sense to concat these and ignore
        // the operator
        Span::concat(self.left.span(), self.right.span())
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorKind, Expression, Node, UnaryOperatorKind};
    use crate::parser::Parser;
    use crate::parser::atom::AtomKind;
    use crate::tokenizer::{Span, TokenKind};

    // Helper to create a parser and parse an expression, checking its span
    fn parse_and_check(source: &str, expected_start: usize, expected_end: usize) -> Expression {
        let mut parser = Parser::new(source);
        let expr = parser.parse_expression().expect("No expression found");
        assert_eq!(expr.span().start.index, expected_start);
        assert_eq!(expr.span().end.index, expected_end);
        expr
    }

    #[test]
    fn test_parse_expression_atom_identifier() {
        let expr = parse_and_check("myVar", 0, 5);
        assert!(
            matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
        );
    }

    #[test]
    fn test_parse_expression_atom_number_literal() {
        let expr = parse_and_check("123", 0, 3);
        assert!(
            matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
        );
    }

    #[test]
    fn test_parse_expression_atom_string_literal() {
        let expr = parse_and_check("\"hello\"", 0, 7);
        assert!(
            matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::StringLiteral))
        );
    }

    #[test]
    fn test_parse_expression_unary_negate() {
        let expr = parse_and_check("-10", 0, 3);
        if let Expression::Unary(unary_expr) = expr {
            assert!(matches!(
                unary_expr.operator.kind,
                UnaryOperatorKind::Negate
            ));
            assert_eq!(unary_expr.operator.token.kind, TokenKind::Minus);
            assert!(
                matches!(unary_expr.operand.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Unary expression, got {:?}", expr.span()); // Added debug print for clarity on failure
        }
    }

    #[test]
    fn test_parse_expression_unary_plus() {
        let expr = parse_and_check("+foo", 0, 4);
        if let Expression::Unary(unary_expr) = expr {
            assert!(matches!(unary_expr.operator.kind, UnaryOperatorKind::Plus));
            assert_eq!(unary_expr.operator.token.kind, TokenKind::Plus);
            assert!(
                matches!(unary_expr.operand.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
        } else {
            panic!("Expected Unary expression, got {:?}", expr.span());
        }
    }

    #[test]
    fn test_parse_expression_binary_add() {
        let expr = parse_and_check("1 + 2", 0, 5);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(binary_expr.operator.kind, BinaryOperatorKind::Add));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::Plus);
            assert!(
                matches!(binary_expr.left.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr.span());
        }
    }

    #[test]
    fn test_parse_expression_binary_subtract() {
        let expr = parse_and_check("foo - bar", 0, 9);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(
                binary_expr.operator.kind,
                BinaryOperatorKind::Subtract
            ));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::Minus);
            assert!(
                matches!(binary_expr.left.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr.span());
        }
    }
}
