mod block;
mod class_definition;
mod expression;
mod if_statement;
mod return_statement;

pub use block::BlockStatement;
use class_definition::ClassDefinition;
use if_statement::IfStatement;
use return_statement::ReturnStatement;

use crate::{node::Node, tokenizer::TokenKind};

use super::{Parser, expression::Expression};

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_empty_statements();
        let statement = self
            .parse_block_statement()
            .or_else(|| self.parse_return_statement())
            .or_else(|| self.parse_class_definition())
            .or_else(|| self.parse_if_statement())
            .or_else(|| self.parse_expression_statement());
        self.parse_empty_statements();
        statement
    }

    fn parse_empty_statements(&mut self) {
        while self.consume_token_if(TokenKind::Semicolon).is_some() {}
    }
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    ExpressionStatement(Expression),
    BlockStatement(BlockStatement),
    ClassDefinition(ClassDefinition),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
}

impl Node for Statement {
    fn span(&self) -> crate::tokenizer::Span {
        match &self.kind {
            StatementKind::ExpressionStatement(expression) => expression.span(),
            StatementKind::IfStatement(if_stmt) => if_stmt.span(),
            StatementKind::BlockStatement(block_stmt) => block_stmt.span(),
            StatementKind::ClassDefinition(class_def) => class_def.span(),
            StatementKind::ReturnStatement(ret_stmt) => ret_stmt.span(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Statement;
    use crate::node::Node;
    use crate::parser::Parser;

    // Helper to create a parser and parse a statement, checking its span
    pub fn parse_and_check(source: &str, expected_start: usize, expected_end: usize) -> Statement {
        let mut parser = Parser::new(source);
        let stmt = parser.parse_statement().expect("No statement found");
        println!("stmt: {:?}", stmt);
        assert_eq!(stmt.span().start.index, expected_start);
        assert_eq!(stmt.span().end.index, expected_end);
        stmt
    }
}
