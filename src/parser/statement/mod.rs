mod block;
mod expression;
mod function_definition;
mod if_statement;

use block::BlockStatement;
use function_definition::FunctionDefinition;
use if_statement::IfStatement;

use crate::node::Node;

use super::{Parser, expression::Expression};

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_block_statement()
            .or_else(|| self.parse_function_definition())
            .or_else(|| self.parse_if_statement())
            .or_else(|| self.parse_expression_statement())
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
    FunctionDefinition(FunctionDefinition),
    IfStatement(IfStatement),
}

impl Node for Statement {
    fn span(&self) -> crate::tokenizer::Span {
        match &self.kind {
            StatementKind::ExpressionStatement(expression) => expression.span(),
            StatementKind::IfStatement(if_stmt) => if_stmt.span(),
            StatementKind::BlockStatement(block_stmt) => block_stmt.span(),
            StatementKind::FunctionDefinition(def) => def.span(),
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
