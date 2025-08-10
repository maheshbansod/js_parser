use crate::{
    Parser, Token,
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::{Statement, StatementKind};

impl<'a> Parser<'a> {
    pub fn parse_block_statement(&mut self) -> Option<Statement> {
        let start_token = self.consume_token_if(TokenKind::LBrace)?;
        let mut statements = vec![];
        while !matches!(
            self.tok_look_ahead().map(|t| t.kind),
            Some(TokenKind::RBrace),
        ) {
            let statement = self.parse_statement();
            if let Some(statement) = statement {
                statements.push(statement);
            } else {
                break;
            }
        }
        let end_token = self.consume_token_if(TokenKind::RBrace);
        Some(Statement {
            kind: StatementKind::BlockStatement(BlockStatement {
                body: statements,
                end_token,
                start_token,
            }),
        })
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
    end_token: Option<Token>,
    start_token: Token,
}

impl Node for BlockStatement {
    fn span(&self) -> Span {
        self.end_token
            .as_ref()
            .map(|end_token| Span::concat(self.start_token.span, end_token.span))
            .unwrap_or_else(|| self.start_token.span)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::atom::AtomKind;
    use crate::parser::expression::term::Term;
    use crate::parser::expression::{BinaryOperatorKind, Expression};
    use crate::parser::statement::StatementKind;
    use crate::parser::statement::tests::parse_and_check;
    use crate::tokenizer::TokenKind;

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

    #[test]
    fn test_parse_block_statement_member_access() {
        let stmt = parse_and_check("{ return this._value; }", 0, 23);
        if let StatementKind::BlockStatement(block) = stmt.kind {
            assert_eq!(block.body.len(), 1);
            let mut statements = block.body;
            let first_statement = statements.remove(0);
            if let StatementKind::ReturnStatement(stmt) = first_statement.kind {
                if let Some(Expression::Binary(exp)) = stmt.value {
                    let left = exp.left;
                    let right = exp.right;
                    let operator = exp.operator;
                    assert!(matches!(operator.kind, BinaryOperatorKind::MemberAccess));
                    if let Expression::Term(Term::Atom(atom)) = *left {
                        assert_eq!(atom.kind, AtomKind::This);
                    } else {
                        panic!("left is supposed to be this, got {:?}", left);
                    }
                    if let Expression::Term(Term::Atom(atom)) = *right {
                        assert_eq!(atom.kind, AtomKind::Identifier);
                    } else {
                        panic!("right is supposed to be this, got {:?}", right);
                    }
                } else {
                    panic!("expected retunr value. got: {:?}", stmt);
                }
            } else {
                panic!("supposed to be return. got idk ");
            }
            assert_eq!(block.start_token.kind, TokenKind::LBrace);
            assert_eq!(block.end_token.unwrap().kind, TokenKind::RBrace);
        } else {
            panic!("Expected BlockStatement, got smn else");
        }
    }
}
