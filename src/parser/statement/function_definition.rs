use crate::{
    Parser, Token,
    node::Node,
    tokenizer::{Span, TokenKind},
};

use super::{Statement, StatementKind, block::BlockStatement};

impl<'a> Parser<'a> {
    pub fn parse_function_definition(&mut self) -> Option<Statement> {
        // todo: is async should be on an outer level and probably passed to this function
        // so we can consume arrow functions the same way
        let async_token = self.consume_token_if(TokenKind::Async);
        let function_token = self.consume_token_if(TokenKind::Function)?;
        let generator_token = self.consume_token_if(TokenKind::Star);
        let name = self.consume_token_if(TokenKind::Identifier);
        self.consume_token_if(TokenKind::LParen);
        let mut params = vec![];
        while !matches!(
            self.tok_look_ahead().map(|t| t.kind),
            Some(TokenKind::RParen)
        ) {
            if let Some(token) = self.consume_token_if(TokenKind::Identifier) {
                params.push(token);
                self.consume_token_if(TokenKind::Comma);
            } else {
                break;
            }
        }
        self.consume_token_if(TokenKind::RParen);
        let block = self.parse_block_statement()?;
        if let StatementKind::BlockStatement(block) = block.kind {
            Some(Statement {
                kind: StatementKind::FunctionDefinition(FunctionDefinition {
                    function_token,
                    name,
                    parameters: params,
                    body: block,
                    async_token,
                    generator_token,
                }),
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    function_token: Token,
    /// function name is always an identifier
    pub name: Option<Token>,
    /// Comma separated identifiers for now
    /// todo: support default parameters and types and everything
    pub parameters: Vec<Token>,
    pub body: BlockStatement,
    pub async_token: Option<Token>,
    pub generator_token: Option<Token>,
}

impl FunctionDefinition {
    pub fn is_async(&self) -> bool {
        self.async_token.is_some()
    }
    pub fn is_generator(&self) -> bool {
        self.generator_token.is_some()
    }
}

impl Node for FunctionDefinition {
    fn span(&self) -> crate::tokenizer::Span {
        let start_span = self
            .async_token
            .as_ref()
            .map(|t| t.span)
            .unwrap_or_else(|| self.function_token.span);
        let end_span = self.body.span();
        Span::concat(start_span, end_span)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::statement::StatementKind;
    use crate::parser::statement::tests::parse_and_check;
    use crate::tokenizer::TokenKind;

    #[test]
    fn test_parse_function_definition_simple() {
        let source = "function foo() {}";
        let stmt = parse_and_check(source, 0, 17);
        if let StatementKind::FunctionDefinition(func_def) = stmt.kind {
            assert!(!func_def.is_async());
            assert!(!func_def.is_generator());
            assert_eq!(func_def.function_token.kind, TokenKind::Function);
            assert!(func_def.name.is_some());
            assert_eq!(func_def.name.unwrap().span.source(source), "foo");
            assert!(func_def.parameters.is_empty());
            assert!(func_def.body.body.is_empty());
        } else {
            panic!("Expected FunctionDefinition, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_function_definition_with_params() {
        let source = "function bar(a, b) {}";
        let stmt = parse_and_check(source, 0, 21);
        if let StatementKind::FunctionDefinition(func_def) = stmt.kind {
            assert!(!func_def.is_async());
            assert!(!func_def.is_generator());
            assert_eq!(func_def.function_token.kind, TokenKind::Function);
            assert!(func_def.name.is_some());
            assert_eq!(func_def.name.unwrap().span.source(source), "bar");
            assert_eq!(func_def.parameters.len(), 2);
            assert_eq!(func_def.parameters[0].span.source(source), "a");
            assert_eq!(func_def.parameters[1].span.source(source), "b");
            assert!(func_def.body.body.is_empty());
        } else {
            panic!("Expected FunctionDefinition, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_function_definition_with_body() {
        let source = "function baz() { 1; }";
        let stmt = parse_and_check(source, 0, 21);
        if let StatementKind::FunctionDefinition(func_def) = stmt.kind {
            assert!(!func_def.is_async());
            assert!(!func_def.is_generator());
            assert_eq!(func_def.function_token.kind, TokenKind::Function);
            assert!(func_def.name.is_some());
            assert_eq!(func_def.name.unwrap().span.source(source), "baz");
            assert!(func_def.parameters.is_empty());
            let block = func_def.body;
            assert_eq!(block.body.len(), 1);
            assert!(matches!(
                block.body[0].kind,
                StatementKind::ExpressionStatement(_)
            ));
        } else {
            panic!("Expected FunctionDefinition, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_async_function_definition() {
        let source = "async function qux() {}";
        let stmt = parse_and_check(source, 0, 23);
        if let StatementKind::FunctionDefinition(func_def) = stmt.kind {
            assert!(func_def.is_async());
            assert!(!func_def.is_generator());
            assert_eq!(func_def.function_token.kind, TokenKind::Function);
            assert!(func_def.name.is_some());
            assert_eq!(func_def.name.unwrap().span.source(source), "qux");
            assert!(func_def.parameters.is_empty());
            assert!(func_def.body.body.is_empty());
        } else {
            panic!("Expected FunctionDefinition, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_generator_function_definition() {
        let source = "function* gen() {}";
        let stmt = parse_and_check(source, 0, 18);
        if let StatementKind::FunctionDefinition(func_def) = stmt.kind {
            assert!(!func_def.is_async());
            assert!(func_def.is_generator());
            assert_eq!(func_def.function_token.kind, TokenKind::Function);
            assert!(func_def.name.is_some());
            assert_eq!(func_def.name.unwrap().span.source(source), "gen");
            assert!(func_def.parameters.is_empty());
            assert!(func_def.body.body.is_empty());
        } else {
            panic!("Expected FunctionDefinition, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_async_generator_function_definition() {
        let source = "async function* agen() {}";
        let stmt = parse_and_check(source, 0, 25);
        if let StatementKind::FunctionDefinition(func_def) = stmt.kind {
            assert!(func_def.is_async());
            assert!(func_def.is_generator());
            assert_eq!(func_def.function_token.kind, TokenKind::Function);
            assert!(func_def.name.is_some());
            assert_eq!(func_def.name.unwrap().span.source(source), "agen");
            assert!(func_def.parameters.is_empty());
        } else {
            panic!("Expected FunctionDefinition, got {:?}", stmt.kind);
        }
    }
}
