use crate::{
    Token,
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::{Parser, expression::Expression};

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_block_statement()
            .or_else(|| self.parse_if_statement())
            .or_else(|| self.parse_expression_statement())
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        if matches!(
            self.tok_look_ahead().map(|t| t.kind),
            Some(TokenKind::LBrace),
        ) {
            let start_token = self.tokenizer.next().unwrap(); // consumes `{`
            let mut statements = vec![];
            let mut should_parse_end_token = true;
            while !matches!(
                self.tok_look_ahead().map(|t| t.kind),
                Some(TokenKind::RBrace),
            ) {
                let statement = self.parse_statement();
                if let Some(statement) = statement {
                    statements.push(statement);
                } else {
                    should_parse_end_token = false;
                    break;
                }
            }
            let end_token = if should_parse_end_token {
                self.tokenizer.next()
            } else {
                None
            };
            Some(Statement {
                kind: StatementKind::BlockStatement(BlockStatement {
                    body: statements,
                    end_token,
                    start_token,
                }),
            })
        } else {
            None
        }
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        let start_token = self.consume_token_if(TokenKind::If)?;
        self.consume_token_if(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume_token_if(TokenKind::RParen)?;
        let consequent = self.parse_statement()?;
        let alternate = self
            .consume_token_if(TokenKind::Else)
            .and_then(|_| self.parse_statement().map(|s| Box::new(s)));
        Some(Statement {
            kind: StatementKind::IfStatement(IfStatement {
                start_token,
                condition,
                consequent: Box::new(consequent),
                alternate,
            }),
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
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

#[derive(Debug)]
pub struct Statement {
    kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    ExpressionStatement(Expression),
    BlockStatement(BlockStatement),
    // FunctionDefinition,
    IfStatement(IfStatement),
}

impl Node for Statement {
    fn span(&self) -> crate::tokenizer::Span {
        match &self.kind {
            StatementKind::ExpressionStatement(expression) => expression.span(),
            StatementKind::IfStatement(if_stmt) => {
                let start = if_stmt.start_token.span;
                let end = if_stmt
                    .alternate
                    .as_ref()
                    .map(|a| a.span())
                    .unwrap_or_else(|| if_stmt.consequent.span());
                Span::concat(start, end)
            }
            StatementKind::BlockStatement(block_stmt) => block_stmt
                .end_token
                .as_ref()
                .map(|end_token| Span::concat(block_stmt.start_token.span, end_token.span))
                .unwrap_or_else(|| block_stmt.start_token.span),
        }
    }
}

#[derive(Debug)]
struct IfStatement {
    start_token: Token,
    condition: Expression,
    consequent: Box<Statement>,
    alternate: Option<Box<Statement>>,
}

#[derive(Debug)]
struct BlockStatement {
    body: Vec<Statement>,
    end_token: Option<Token>,
    start_token: Token,
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
        println!("stmt: {:?}", stmt);
        assert_eq!(stmt.span().start.index, expected_start);
        assert_eq!(stmt.span().end.index, expected_end);
        stmt
    }

    #[test]
    fn test_parse_expression_statement_identifier() {
        let stmt = parse_and_check("myVar;", 0, 5); // Span of "myVar"
        if let StatementKind::ExpressionStatement(expr) = stmt.kind {
            assert!(
                matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
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
                    matches!(binary_expr.left.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
                );
                assert!(
                    matches!(binary_expr.right.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::NumberLiteral))
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
                    matches!(unary_expr.operand.as_ref(), Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(expr, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
            );
        } else {
            panic!("Expected ExpressionStatement, got {:?}", stmt.kind);
        }
    }
    #[test]
    fn test_parse_if_statement_basic() {
        let stmt = parse_and_check("if (true) {}", 0, 12);
        if let StatementKind::IfStatement(if_stmt) = stmt.kind {
            assert!(
                matches!(if_stmt.condition, Expression::Atom(atom) if matches!(atom.kind, AtomKind::BooleanLiteral))
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
                matches!(if_stmt.condition, Expression::Atom(atom) if matches!(atom.kind, AtomKind::BooleanLiteral))
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
                matches!(if_stmt.condition, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
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
                matches!(if_stmt.condition, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
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
                        matches!(nested_if_stmt.condition, Expression::Atom(atom) if matches!(atom.kind, AtomKind::Identifier))
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
    #[test]
    fn test_parse_block_statement_empty() {
        let stmt = parse_and_check("{}", 0, 2);
        if let StatementKind::BlockStatement(block) = stmt.kind {
            assert!(block.body.is_empty());
            assert_eq!(block.start_token.kind, TokenKind::LBrace);
            assert_eq!(block.end_token.unwrap().kind, TokenKind::RBrace);
        } else {
            panic!("Expected BlockStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_block_statement_single_expression() {
        let stmt = parse_and_check("{ 1; }", 0, 6);
        if let StatementKind::BlockStatement(block) = stmt.kind {
            assert_eq!(block.body.len(), 1);
            assert!(matches!(
                block.body[0].kind,
                StatementKind::ExpressionStatement(_)
            ));
            assert_eq!(block.start_token.kind, TokenKind::LBrace);
            assert_eq!(block.end_token.unwrap().kind, TokenKind::RBrace);
        } else {
            panic!("Expected BlockStatement, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_block_statement_multiple_expressions() {
        let stmt = parse_and_check("{ 1; 2; }", 0, 9);
        if let StatementKind::BlockStatement(block) = stmt.kind {
            assert_eq!(block.body.len(), 2);
            assert!(matches!(
                block.body[0].kind,
                StatementKind::ExpressionStatement(_)
            ));
            assert!(matches!(
                block.body[1].kind,
                StatementKind::ExpressionStatement(_)
            ));
            assert_eq!(block.start_token.kind, TokenKind::LBrace);
            assert_eq!(block.end_token.unwrap().kind, TokenKind::RBrace);
        } else {
            panic!("Expected BlockStatement, got {:?}", stmt.kind);
        }
    }
}
