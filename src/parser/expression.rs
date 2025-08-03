use crate::{
    Token,
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::{Parser, atom::Atom};

/**
 * Term = Atom | BracketedExp
 * UnaryExp = (UnaryOp Term) | Term
 * BinaryExp = UnaryExp (BinaryOp BinaryExp)*
 * Exp = BinaryExp
 * FunctionExp = Exp LParen ( Exp (Comma Exp)* ) RParen
 * */

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_expression()
    }

    fn parse_binary_expression(&mut self) -> Option<Expression> {
        let left_operand = self.parse_unary_expression()?;
        let next_token = self.tok_look_ahead();
        if next_token.is_none() {
            return Some(left_operand);
        }
        let next_token = next_token.unwrap();
        if let Some(op_kind) = BinaryOperatorKind::try_from_token_kind(next_token.kind) {
            let op_token = self.tokenizer.next().unwrap();
            if let Some(right_operand) = self.parse_expression() {
                return Some(Expression::Binary(BinaryExpression {
                    left: Box::new(left_operand),
                    right: Box::new(right_operand),
                    operator: BinaryOperator {
                        token: op_token,
                        kind: op_kind,
                    },
                }));
            } else {
                return Some(left_operand);
            }
        } else if let Some(_lparen_token) = self.consume_token_if(TokenKind::LParen) {
            let callee = left_operand;
            let mut args = vec![];
            while !matches!(
                self.tok_look_ahead().map(|t| t.kind),
                Some(TokenKind::RParen)
            ) {
                if let Some(arg) = self.parse_expression() {
                    args.push(arg);
                    self.consume_token_if(TokenKind::Comma);
                } else {
                    break;
                }
            }
            let end_token = self.consume_token_if(TokenKind::RParen);
            Some(Expression::FunctionCall(FunctionCallExpression {
                callee: Box::new(callee),
                args,
                end_token,
            }))
        } else {
            return Some(left_operand);
        }
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
    FunctionCall(FunctionCallExpression),
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

impl BinaryOperatorKind {
    fn try_from_token_kind(token_kind: TokenKind) -> Option<Self> {
        match token_kind {
            TokenKind::Plus => Some(BinaryOperatorKind::Add),
            TokenKind::Minus => Some(BinaryOperatorKind::Subtract),
            _ => None,
        }
    }
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
            Expression::FunctionCall(function_call) => function_call.span(),
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

#[derive(Debug)]
pub struct FunctionCallExpression {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
    end_token: Option<Token>,
}

impl Node for FunctionCallExpression {
    fn span(&self) -> Span {
        let start_span = self.callee.span();
        if let Some(end_span) = self
            .end_token
            .as_ref()
            .map(|end_token| end_token.span)
            .or_else(|| self.args.last().map(|exp| exp.span()))
        {
            Span::concat(start_span, end_span)
        } else {
            start_span
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperatorKind, Expression, Node, UnaryOperatorKind};
    use crate::parser::Parser;
    use crate::parser::atom::{Atom, AtomKind};
    use crate::tokenizer::TokenKind;

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
    #[test]
    fn test_parse_expression_function_call_no_args() {
        let expr = parse_and_check("foo()", 0, 5);
        if let Expression::FunctionCall(call_expr) = expr {
            assert!(
                matches!(call_expr.callee.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 0);
            assert!(call_expr.end_token.is_some());
            assert_eq!(call_expr.end_token.unwrap().kind, TokenKind::RParen);
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_function_call_one_arg() {
        let expr = parse_and_check("bar(123)", 0, 8);
        if let Expression::FunctionCall(call_expr) = expr {
            assert!(
                matches!(call_expr.callee.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 1);
            assert!(
                matches!(&call_expr.args[0], Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(call_expr.end_token.is_some());
            assert_eq!(call_expr.end_token.unwrap().kind, TokenKind::RParen);
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_function_call_multiple_args() {
        let expr = parse_and_check("baz(1, \"hello\", x)", 0, 18);
        if let Expression::FunctionCall(call_expr) = expr {
            assert!(
                matches!(call_expr.callee.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 3);
            assert!(matches!(
                &call_expr.args[0],
                &Expression::Atom(Atom {
                    span: _,
                    kind: AtomKind::NumberLiteral,
                })
            ));
            assert!(matches!(
                &call_expr.args[1],
                Expression::Atom(Atom {
                    kind: AtomKind::StringLiteral,
                    span: _
                })
            ));
            assert!(matches!(
                &call_expr.args[2],
                Expression::Atom(Atom {
                    span: _,
                    kind: AtomKind::Identifier
                })
            ));
            assert!(call_expr.end_token.is_some());
            assert_eq!(call_expr.end_token.unwrap().kind, TokenKind::RParen);
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_function_call_nested_args() {
        let expr = parse_and_check("calc(1 + 2, -3)", 0, 15);
        if let Expression::FunctionCall(call_expr) = expr {
            assert!(
                matches!(call_expr.callee.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 2);
            assert!(matches!(call_expr.args[0], Expression::Binary(_)));
            assert!(matches!(call_expr.args[1], Expression::Unary(_)));
            assert!(call_expr.end_token.is_some());
            assert_eq!(call_expr.end_token.unwrap().kind, TokenKind::RParen);
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }

    // #[test]
    // fn test_parse_expression_function_call_callee_is_expression() {
    //     // This tests a call like (1 + 2)() or foo()()
    //     let expr = parse_and_check("foo()()", 0, 7);
    //     if let Expression::FunctionCall(outer_call) = expr {
    //         assert_eq!(outer_call.args.len(), 0);
    //         assert!(outer_call.end_token.is_some());
    //         assert_eq!(outer_call.end_token.unwrap().kind, TokenKind::RParen);
    //
    //         if let Expression::FunctionCall(inner_call) = outer_call.callee.as_ref() {
    //             assert!(
    //                 matches!(inner_call.callee.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
    //             );
    //             assert_eq!(inner_call.args.len(), 0);
    //             assert!(inner_call.end_token.is_some());
    //             assert_eq!(
    //                 inner_call.end_token.as_ref().map(|t| t.kind),
    //                 Some(TokenKind::RParen)
    //             );
    //         } else {
    //             panic!(
    //                 "Expected inner callee to be FunctionCall, got {:?}",
    //                 outer_call.callee
    //             );
    //         }
    //     } else {
    //         panic!("Expected FunctionCall expression, got {:?}", expr);
    //     }
    // }

    #[test]
    fn test_parse_expression_function_call_missing_rparen() {
        let expr = parse_and_check("foo(1", 0, 5); // Span should end at the last parsed token
        if let Expression::FunctionCall(call_expr) = expr {
            assert!(
                matches!(call_expr.callee.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 1);
            assert!(call_expr.end_token.is_none()); // Expect no closing parenthesis
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }
}
