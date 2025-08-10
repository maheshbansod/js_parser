pub mod function_definition;
pub mod term;

use crate::{
    Token,
    node::Node,
    parser::expression::term::Term,
    tokenizer::{Span, TokenKind},
};

use super::Parser;

/**
 * Pratt parsing gang lets go
 *
 * */
impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_expression_with_priority(0)
    }

    /// parses expresions with given priority operator or higher
    fn parse_expression_with_priority(&mut self, priority: u8) -> Option<Expression> {
        let term = self.parse_term();
        let mut current_exp = if let Some(term) = term {
            Expression::Term(term)
        } else {
            // could be an operator next
            if let Some(operator) = self.peek_unary_operator() {
                let priority = operator.priority();
                let operator = self.consume_unary_operator(operator);
                let operand = self.parse_expression_with_priority(priority)?;
                Expression::Unary(UnaryExpression {
                    operand: Box::new(operand),
                    operator,
                })
            } else {
                return None;
            }
        };
        if self.consume_token_if(TokenKind::LParen).is_some() {
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
            current_exp = Expression::FunctionCall(FunctionCallExpression {
                callee: Box::new(current_exp),
                args,
                end_token,
            });
        }
        loop {
            let operator = self.peek_binary_operator();
            if operator.is_none() {
                return Some(current_exp);
            }
            let operator = operator.unwrap();
            let (left_priority, right_priority) = operator.priority();
            if right_priority >= priority && right_priority > left_priority {
                let operator = self.consume_binary_operator(operator);
                if let Some(next_term) = self.parse_expression_with_priority(right_priority) {
                    current_exp = Expression::Binary(BinaryExpression {
                        left: Box::new(current_exp),
                        right: Box::new(next_term),
                        operator,
                    });
                }
            } else if left_priority > priority {
                let operator = self.consume_binary_operator(operator);
                if let Some(next_term) = self.parse_expression_with_priority(left_priority) {
                    current_exp = Expression::Binary(BinaryExpression {
                        left: Box::new(current_exp),
                        right: Box::new(next_term),
                        operator,
                    });
                }
            } else {
                return Some(current_exp);
            }
        }
    }

    fn consume_unary_operator(&mut self, kind: UnaryOperatorKind) -> UnaryOperator {
        let op_token = self.tokenizer.next().unwrap();
        UnaryOperator {
            token: op_token,
            kind,
        }
    }

    fn consume_binary_operator(&mut self, kind: BinaryOperatorKind) -> BinaryOperator {
        let op_token = self.tokenizer.next().unwrap();
        BinaryOperator {
            token: op_token,
            kind,
        }
    }

    fn peek_unary_operator(&mut self) -> Option<UnaryOperatorKind> {
        self.tok_look_ahead()
            .and_then(|token| UnaryOperatorKind::try_from_token_kind(token.kind))
    }

    fn peek_binary_operator(&mut self) -> Option<BinaryOperatorKind> {
        self.tok_look_ahead()
            .and_then(|token| BinaryOperatorKind::try_from_token_kind(token.kind))
    }
}

#[derive(Debug)]
pub enum Expression {
    FunctionCall(FunctionCallExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Term(Term),
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

impl BinaryOperatorKind {}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperatorKind {
    Add,
    Assign,
    Divide,
    Exponentiation,
    MemberAccess,
    Modulo,
    Multiply,
    Subtract,
    EqEq,
    BangEq,
    EqEqEq,
    BangEqEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    AmpAmp,
    PipePipe,
    Amp,
    Pipe,
    Caret,
    LtLt,
    GtGt,
    GtGtGt,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    StarStarEq,
    LtLtEq,
    GtGtEq,
    GtGtGtEq,
    AmpEq,
    PipeEq,
    CaretEq,
    QuestionQuestion,
    QuestionQuestionEq,
}

impl BinaryOperatorKind {
    fn priority(&self) -> (u8, u8) {
        match self {
            BinaryOperatorKind::Assign
            | BinaryOperatorKind::PlusEq
            | BinaryOperatorKind::MinusEq
            | BinaryOperatorKind::StarEq
            | BinaryOperatorKind::SlashEq
            | BinaryOperatorKind::PercentEq
            | BinaryOperatorKind::StarStarEq
            | BinaryOperatorKind::LtLtEq
            | BinaryOperatorKind::GtGtEq
            | BinaryOperatorKind::GtGtGtEq
            | BinaryOperatorKind::AmpEq
            | BinaryOperatorKind::PipeEq
            | BinaryOperatorKind::CaretEq
            | BinaryOperatorKind::QuestionQuestionEq => (1, 2),
            BinaryOperatorKind::PipePipe => (3, 2),
            BinaryOperatorKind::AmpAmp => (4, 3),
            BinaryOperatorKind::Pipe => (5, 4),
            BinaryOperatorKind::Caret => (6, 5),
            BinaryOperatorKind::Amp => (7, 6),
            BinaryOperatorKind::EqEq
            | BinaryOperatorKind::BangEq
            | BinaryOperatorKind::EqEqEq
            | BinaryOperatorKind::BangEqEq => (8, 7),
            BinaryOperatorKind::Lt
            | BinaryOperatorKind::LtEq
            | BinaryOperatorKind::Gt
            | BinaryOperatorKind::GtEq => (9, 8),
            BinaryOperatorKind::LtLt | BinaryOperatorKind::GtGt | BinaryOperatorKind::GtGtGt => {
                (10, 9)
            }
            BinaryOperatorKind::Add | BinaryOperatorKind::Subtract => (11, 10),
            BinaryOperatorKind::Multiply
            | BinaryOperatorKind::Divide
            | BinaryOperatorKind::Modulo => (12, 11),
            BinaryOperatorKind::Exponentiation => (13, 14),
            BinaryOperatorKind::MemberAccess => (15, 16),
            BinaryOperatorKind::QuestionQuestion => (17, 18),
        }
    }

    fn try_from_token_kind(token_kind: TokenKind) -> Option<Self> {
        match token_kind {
            TokenKind::Plus => Some(BinaryOperatorKind::Add),
            TokenKind::Minus => Some(BinaryOperatorKind::Subtract),
            TokenKind::Percent => Some(BinaryOperatorKind::Modulo),
            TokenKind::Star => Some(BinaryOperatorKind::Multiply),
            TokenKind::StarStar => Some(BinaryOperatorKind::Exponentiation),
            TokenKind::Slash => Some(BinaryOperatorKind::Divide),
            TokenKind::Dot => Some(BinaryOperatorKind::MemberAccess),
            TokenKind::Eq => Some(BinaryOperatorKind::Assign),
            TokenKind::EqEq => Some(BinaryOperatorKind::EqEq),
            TokenKind::BangEq => Some(BinaryOperatorKind::BangEq),
            TokenKind::EqEqEq => Some(BinaryOperatorKind::EqEqEq),
            TokenKind::BangEqEq => Some(BinaryOperatorKind::BangEqEq),
            TokenKind::Lt => Some(BinaryOperatorKind::Lt),
            TokenKind::LtEq => Some(BinaryOperatorKind::LtEq),
            TokenKind::Gt => Some(BinaryOperatorKind::Gt),
            TokenKind::GtEq => Some(BinaryOperatorKind::GtEq),
            TokenKind::AmpAmp => Some(BinaryOperatorKind::AmpAmp),
            TokenKind::PipePipe => Some(BinaryOperatorKind::PipePipe),
            TokenKind::Amp => Some(BinaryOperatorKind::Amp),
            TokenKind::Pipe => Some(BinaryOperatorKind::Pipe),
            TokenKind::Caret => Some(BinaryOperatorKind::Caret),
            TokenKind::LtLt => Some(BinaryOperatorKind::LtLt),
            TokenKind::GtGt => Some(BinaryOperatorKind::GtGt),
            TokenKind::GtGtGt => Some(BinaryOperatorKind::GtGtGt),
            TokenKind::PlusEq => Some(BinaryOperatorKind::PlusEq),
            TokenKind::MinusEq => Some(BinaryOperatorKind::MinusEq),
            TokenKind::StarEq => Some(BinaryOperatorKind::StarEq),
            TokenKind::SlashEq => Some(BinaryOperatorKind::SlashEq),
            TokenKind::PercentEq => Some(BinaryOperatorKind::PercentEq),
            TokenKind::StarStarEq => Some(BinaryOperatorKind::StarStarEq),
            TokenKind::LtLtEq => Some(BinaryOperatorKind::LtLtEq),
            TokenKind::GtGtEq => Some(BinaryOperatorKind::GtGtEq),
            TokenKind::GtGtGtEq => Some(BinaryOperatorKind::GtGtGtEq),
            TokenKind::AmpEq => Some(BinaryOperatorKind::AmpEq),
            TokenKind::PipeEq => Some(BinaryOperatorKind::PipeEq),
            TokenKind::CaretEq => Some(BinaryOperatorKind::CaretEq),
            TokenKind::QuestionQuestion => Some(BinaryOperatorKind::QuestionQuestion),
            TokenKind::QuestionQuestionEq => Some(BinaryOperatorKind::QuestionQuestionEq),
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

impl UnaryOperatorKind {
    fn priority(&self) -> u8 {
        15
    }
    fn try_from_token_kind(token_kind: TokenKind) -> Option<Self> {
        match token_kind {
            TokenKind::Plus => Some(UnaryOperatorKind::Plus),
            TokenKind::Minus => Some(UnaryOperatorKind::Negate),
            _ => None,
        }
    }
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
            Expression::Term(term) => term.span(),
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
    use crate::parser::expression::term::Term;
    use crate::tokenizer::TokenKind;

    // Helper to create a parser and parse an expression, checking its span
    pub fn parse_and_check(source: &str, expected_start: usize, expected_end: usize) -> Expression {
        let mut parser = Parser::new(source);
        let expr = parser.parse_expression().expect("No expression found");
        println!("{expr:?}");
        assert_eq!(expr.span().start.index, expected_start);
        assert_eq!(expr.span().end.index, expected_end);
        expr
    }

    #[test]
    fn test_parse_expression_atom_identifier() {
        let expr = parse_and_check("myVar", 0, 5);
        assert!(
            matches!(expr, Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
        );
    }

    #[test]
    fn test_parse_expression_atom_number_literal() {
        let expr = parse_and_check("123", 0, 3);
        assert!(
            matches!(expr, Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
        );
    }

    #[test]
    fn test_parse_expression_atom_string_literal() {
        let expr = parse_and_check("\"hello\"", 0, 7);
        assert!(
            matches!(expr, Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::StringLiteral))
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
                matches!(unary_expr.operand.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
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
                matches!(unary_expr.operand.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr.span());
        }
    }

    #[test]
    fn test_parse_expression_binary_add_multiple() {
        let expr = parse_and_check("1 + 2 + 3", 0, 9);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(binary_expr.operator.kind, BinaryOperatorKind::Add));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::Plus);
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            if let Expression::Binary(bin_exp) = binary_expr.left.as_ref() {
                assert!(matches!(bin_exp.operator.kind, BinaryOperatorKind::Add));
                assert_eq!(bin_exp.operator.token.kind, TokenKind::Plus);
                assert!(
                    matches!(bin_exp.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(bin_exp.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected binary expression on the left of the expression");
            }
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
                matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(call_expr.callee.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(call_expr.callee.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 1);
            assert!(
                matches!(&call_expr.args[0], Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
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
                matches!(call_expr.callee.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 3);
            assert!(matches!(
                &call_expr.args[0],
                &Expression::Term(Term::Atom(Atom {
                    span: _,
                    kind: AtomKind::NumberLiteral,
                }))
            ));
            assert!(matches!(
                &call_expr.args[1],
                Expression::Term(Term::Atom(Atom {
                    kind: AtomKind::StringLiteral,
                    span: _
                }))
            ));
            assert!(matches!(
                &call_expr.args[2],
                Expression::Term(Term::Atom(Atom {
                    span: _,
                    kind: AtomKind::Identifier
                }))
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
                matches!(call_expr.callee.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(call_expr.callee.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
            );
            assert_eq!(call_expr.args.len(), 1);
            assert!(call_expr.end_token.is_none()); // Expect no closing parenthesis
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }
    #[test]
    fn test_parse_expression_binary_multiply() {
        let expr = parse_and_check("2 * 3", 0, 5);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(
                binary_expr.operator.kind,
                BinaryOperatorKind::Multiply
            ));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::Star);
            assert!(
                matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_binary_divide() {
        let expr = parse_and_check("10 / 2", 0, 6);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(
                binary_expr.operator.kind,
                BinaryOperatorKind::Divide
            ));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::Slash);
            assert!(
                matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_binary_modulo() {
        let expr = parse_and_check("10 % 3", 0, 6);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(
                binary_expr.operator.kind,
                BinaryOperatorKind::Modulo
            ));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::Percent);
            assert!(
                matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_precedence_mul_add() {
        let expr = parse_and_check("1 + 2 * 3", 0, 9);
        if let Expression::Binary(add_expr) = expr {
            assert!(matches!(add_expr.operator.kind, BinaryOperatorKind::Add));
            assert!(
                matches!(add_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            if let Expression::Binary(mul_expr) = add_expr.right.as_ref() {
                assert!(matches!(
                    mul_expr.operator.kind,
                    BinaryOperatorKind::Multiply
                ));
                assert!(
                    matches!(mul_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(mul_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected multiplication on right side of addition");
            }
        } else {
            panic!("Expected Binary expression (addition), got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_precedence_mul_sub() {
        let expr = parse_and_check("10 - 2 * 3", 0, 10);
        if let Expression::Binary(sub_expr) = expr {
            assert!(matches!(
                sub_expr.operator.kind,
                BinaryOperatorKind::Subtract
            ));
            assert!(
                matches!(sub_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            if let Expression::Binary(mul_expr) = sub_expr.right.as_ref() {
                assert!(matches!(
                    mul_expr.operator.kind,
                    BinaryOperatorKind::Multiply
                ));
                assert!(
                    matches!(mul_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(mul_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected multiplication on right side of subtraction");
            }
        } else {
            panic!("Expected Binary expression (subtraction), got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_precedence_unary_mul() {
        let expr = parse_and_check("-2 * 3", 0, 6);
        if let Expression::Binary(mul_expr) = expr {
            assert!(matches!(
                mul_expr.operator.kind,
                BinaryOperatorKind::Multiply
            ));
            if let Expression::Unary(unary_expr) = mul_expr.left.as_ref() {
                assert!(matches!(
                    unary_expr.operator.kind,
                    UnaryOperatorKind::Negate
                ));
                assert!(
                    matches!(unary_expr.operand.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected unary expression on left side of multiplication");
            }
            assert!(
                matches!(mul_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!(
                "Expected Binary expression (multiplication), got {:?}",
                expr
            );
        }
    }

    #[test]
    fn test_parse_expression_precedence_mul_unary() {
        let expr = parse_and_check("2 * -3", 0, 6);
        if let Expression::Binary(mul_expr) = expr {
            assert!(matches!(
                mul_expr.operator.kind,
                BinaryOperatorKind::Multiply
            ));
            assert!(
                matches!(mul_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            if let Expression::Unary(unary_expr) = mul_expr.right.as_ref() {
                assert!(matches!(
                    unary_expr.operator.kind,
                    UnaryOperatorKind::Negate
                ));
                assert!(
                    matches!(unary_expr.operand.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected unary expression on right side of multiplication");
            }
        } else {
            panic!(
                "Expected Binary expression (multiplication), got {:?}",
                expr
            );
        }
    }

    #[test]
    fn test_parse_expression_associativity_mul_div() {
        let expr = parse_and_check("10 * 2 / 4", 0, 10);
        if let Expression::Binary(div_expr) = expr {
            assert!(matches!(div_expr.operator.kind, BinaryOperatorKind::Divide));
            assert!(
                matches!(div_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            if let Expression::Binary(mul_expr) = div_expr.left.as_ref() {
                assert!(matches!(
                    mul_expr.operator.kind,
                    BinaryOperatorKind::Multiply
                ));
                assert!(
                    matches!(mul_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(mul_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected multiplication on left side of division");
            }
        } else {
            panic!("Expected Binary expression (division), got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_empty_string() {
        let mut parser = Parser::new("");
        assert!(parser.parse_expression().is_none());
    }

    #[test]
    fn test_parse_expression_only_whitespace() {
        let mut parser = Parser::new("   ");
        assert!(parser.parse_expression().is_none());
    }

    #[test]
    fn test_parse_expression_missing_left_operand_binary_only() {
        let mut parser = Parser::new("* 2");
        assert!(parser.parse_expression().is_none());
    }

    #[test]
    fn test_parse_expression_function_call_complex_args() {
        let expr = parse_and_check("calculate(1 + 2 * 3, -4 / 2)", 0, 28);
        if let Expression::FunctionCall(call_expr) = expr {
            assert_eq!(call_expr.args.len(), 2);
            if let Expression::Binary(arg0) = &call_expr.args[0] {
                assert!(matches!(arg0.operator.kind, BinaryOperatorKind::Add));
                if let Expression::Binary(mul_expr) = arg0.right.as_ref() {
                    assert!(matches!(
                        mul_expr.operator.kind,
                        BinaryOperatorKind::Multiply
                    ));
                } else {
                    panic!("Expected multiplication in first arg's right operand");
                }
            } else {
                panic!("Expected binary expression for first arg");
            }
            if let Expression::Binary(arg1) = &call_expr.args[1] {
                assert!(matches!(arg1.operator.kind, BinaryOperatorKind::Divide));
                if let Expression::Unary(neg_expr) = arg1.left.as_ref() {
                    assert!(matches!(neg_expr.operator.kind, UnaryOperatorKind::Negate));
                } else {
                    panic!("Expected unary expression in second arg's left operand");
                }
            } else {
                panic!("Expected binary expression for second arg");
            }
        } else {
            panic!("Expected FunctionCall expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_binary_exponentiation() {
        let expr = parse_and_check("2 ** 3", 0, 6);
        if let Expression::Binary(binary_expr) = expr {
            assert!(matches!(
                binary_expr.operator.kind,
                BinaryOperatorKind::Exponentiation
            ));
            assert_eq!(binary_expr.operator.token.kind, TokenKind::StarStar);
            assert!(
                matches!(binary_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            assert!(
                matches!(binary_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
        } else {
            panic!("Expected Binary expression, got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_expression_associativity_exponentiation() {
        let expr = parse_and_check("2 ** 3 ** 2", 0, 11);
        if let Expression::Binary(outer_exp_expr) = expr {
            assert!(matches!(
                outer_exp_expr.operator.kind,
                BinaryOperatorKind::Exponentiation
            ));
            assert!(
                matches!(outer_exp_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
            );
            if let Expression::Binary(inner_exp_expr) = outer_exp_expr.right.as_ref() {
                assert!(matches!(
                    inner_exp_expr.operator.kind,
                    BinaryOperatorKind::Exponentiation
                ));
                assert!(
                    matches!(inner_exp_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(inner_exp_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
            } else {
                panic!("Expected inner exponentiation on right side of outer exponentiation");
            }
        } else {
            panic!(
                "Expected Binary expression (exponentiation), got {:?}",
                expr
            );
        }
    }
    #[test]
    fn test_parse_expression_member_access_assignment() {
        let expr = parse_and_check("this.a = a", 0, 10);
        if let Expression::Binary(assign_expr) = expr {
            assert!(matches!(
                assign_expr.operator.kind,
                BinaryOperatorKind::Assign
            ));
            assert_eq!(assign_expr.operator.token.kind, TokenKind::Eq);

            // Check the left side of the assignment (this.a)
            if let Expression::Binary(member_access_expr) = assign_expr.left.as_ref() {
                assert!(matches!(
                    member_access_expr.operator.kind,
                    BinaryOperatorKind::MemberAccess
                ));
                assert_eq!(member_access_expr.operator.token.kind, TokenKind::Dot);
                assert!(
                    matches!(member_access_expr.left.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::This))
                ); // "this"
                assert!(
                    matches!(member_access_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
                ); // "a"
            } else {
                panic!(
                    "Expected Binary expression (MemberAccess) on left side of assignment, got {:?}",
                    assign_expr.left
                );
            }

            // Check the right side of the assignment (a)
            assert!(
                matches!(assign_expr.right.as_ref(), Expression::Term(Term::Atom(atom)) if matches!(atom.kind, AtomKind::Identifier))
            ); // "a"
        } else {
            panic!("Expected Binary expression (Assignment), got {:?}", expr);
        }
    }

    #[test]
    fn test_parse_all_possible_binary_operators() {
        use crate::tokenizer::TokenKind;

        let sources = [
            ("1 + 2", Some(BinaryOperatorKind::Add), TokenKind::Plus),
            (
                "3 - 4",
                Some(BinaryOperatorKind::Subtract),
                TokenKind::Minus,
            ),
            ("5 * 6", Some(BinaryOperatorKind::Multiply), TokenKind::Star),
            ("7 / 8", Some(BinaryOperatorKind::Divide), TokenKind::Slash),
            (
                "9 % 2",
                Some(BinaryOperatorKind::Modulo),
                TokenKind::Percent,
            ),
            (
                "2 ** 3",
                Some(BinaryOperatorKind::Exponentiation),
                TokenKind::StarStar,
            ),
            (
                "foo.bar",
                Some(BinaryOperatorKind::MemberAccess),
                TokenKind::Dot,
            ),
            ("a = b", Some(BinaryOperatorKind::Assign), TokenKind::Eq),
            ("1 == 2", Some(BinaryOperatorKind::EqEq), TokenKind::EqEq),
            (
                "1 != 2",
                Some(BinaryOperatorKind::BangEq),
                TokenKind::BangEq,
            ),
            (
                "1 === 2",
                Some(BinaryOperatorKind::EqEqEq),
                TokenKind::EqEqEq,
            ),
            (
                "1 !== 2",
                Some(BinaryOperatorKind::BangEqEq),
                TokenKind::BangEqEq,
            ),
            ("1 < 2", Some(BinaryOperatorKind::Lt), TokenKind::Lt),
            ("1 <= 2", Some(BinaryOperatorKind::LtEq), TokenKind::LtEq),
            ("1 > 2", Some(BinaryOperatorKind::Gt), TokenKind::Gt),
            ("1 >= 2", Some(BinaryOperatorKind::GtEq), TokenKind::GtEq),
            (
                "1 && 2",
                Some(BinaryOperatorKind::AmpAmp),
                TokenKind::AmpAmp,
            ),
            (
                "1 || 2",
                Some(BinaryOperatorKind::PipePipe),
                TokenKind::PipePipe,
            ),
            ("1 & 2", Some(BinaryOperatorKind::Amp), TokenKind::Amp),
            ("1 | 2", Some(BinaryOperatorKind::Pipe), TokenKind::Pipe),
            ("1 ^ 2", Some(BinaryOperatorKind::Caret), TokenKind::Caret),
            ("1 << 2", Some(BinaryOperatorKind::LtLt), TokenKind::LtLt),
            ("1 >> 2", Some(BinaryOperatorKind::GtGt), TokenKind::GtGt),
            (
                "1 >>> 2",
                Some(BinaryOperatorKind::GtGtGt),
                TokenKind::GtGtGt,
            ),
            (
                "a += b",
                Some(BinaryOperatorKind::PlusEq),
                TokenKind::PlusEq,
            ),
            (
                "a -= b",
                Some(BinaryOperatorKind::MinusEq),
                TokenKind::MinusEq,
            ),
            (
                "a *= b",
                Some(BinaryOperatorKind::StarEq),
                TokenKind::StarEq,
            ),
            (
                "a /= b",
                Some(BinaryOperatorKind::SlashEq),
                TokenKind::SlashEq,
            ),
            (
                "a %= b",
                Some(BinaryOperatorKind::PercentEq),
                TokenKind::PercentEq,
            ),
            (
                "a **= b",
                Some(BinaryOperatorKind::StarStarEq),
                TokenKind::StarStarEq,
            ),
            (
                "a <<= b",
                Some(BinaryOperatorKind::LtLtEq),
                TokenKind::LtLtEq,
            ),
            (
                "a >>= b",
                Some(BinaryOperatorKind::GtGtEq),
                TokenKind::GtGtEq,
            ),
            (
                "a >>>= b",
                Some(BinaryOperatorKind::GtGtGtEq),
                TokenKind::GtGtGtEq,
            ),
            ("a &= b", Some(BinaryOperatorKind::AmpEq), TokenKind::AmpEq),
            (
                "a |= b",
                Some(BinaryOperatorKind::PipeEq),
                TokenKind::PipeEq,
            ),
            (
                "a ^= b",
                Some(BinaryOperatorKind::CaretEq),
                TokenKind::CaretEq,
            ),
            (
                "a ?? b",
                Some(BinaryOperatorKind::QuestionQuestion),
                TokenKind::QuestionQuestion,
            ),
            (
                "a ??= b",
                Some(BinaryOperatorKind::QuestionQuestionEq),
                TokenKind::QuestionQuestionEq,
            ),
            // Add more as needed
        ];

        for (src, expected_kind, expected_token) in sources {
            let expr = parse_and_check(src, 0, src.len());
            match expected_kind {
                Some(kind) => {
                    if let Expression::Binary(bin_expr) = expr {
                        assert_eq!(
                            bin_expr.operator.kind, kind,
                            "Operator kind mismatch for '{}'",
                            src
                        );
                        assert_eq!(
                            bin_expr.operator.token.kind, expected_token,
                            "Token kind mismatch for '{}'",
                            src
                        );
                    } else {
                        panic!("Expected Binary expression for '{}', got {:?}", src, expr);
                    }
                }
                None => {
                    // Not supported: should NOT parse as Binary
                    assert!(
                        !matches!(expr, Expression::Binary(_)),
                        "Unexpectedly parsed unsupported operator as Binary for '{}'",
                        src
                    );
                }
            }
        }
    }
}
