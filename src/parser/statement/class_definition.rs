use crate::{
    Parser, Token,
    node::Node,
    parser::{expression::Expression, statement::StatementKind},
    tokenizer::{Span, TokenKind},
};

use super::{Statement, block::BlockStatement, function_definition::Parameter};

impl<'a> Parser<'a> {
    pub fn parse_class_definition(&mut self) -> Option<Statement> {
        let class_token = self.consume_token_if(TokenKind::Class)?;
        let name = self.consume_token_if(TokenKind::Identifier);
        let (extends_token, super_class_name) = if name.is_some() {
            let extends_token = self.consume_token_if(TokenKind::Extends);
            if extends_token.is_some() {
                let super_class_name = self.consume_token_if(TokenKind::Identifier);
                (extends_token, super_class_name)
            } else {
                (None, None)
            }
        } else {
            (None, None)
        };
        let _l_brace = self.consume_token_if(TokenKind::LBrace);
        let mut members = vec![];

        while !matches!(
            self.tok_look_ahead().map(|t| t.kind),
            Some(TokenKind::RBrace)
        ) {
            if let Some(member) = self.parse_class_member() {
                members.push(member);
            } else {
                break;
            }
        }
        let end_token = self.consume_token_if(TokenKind::RBrace)?;
        Some(Statement {
            kind: StatementKind::ClassDefinition(ClassDefinition {
                class_token,
                class_name: name,
                extends_token,
                super_class_name,
                members,
                end_token,
            }),
        })
    }
    fn parse_class_member(&mut self) -> Option<ClassMember> {
        let getter_token = self.consume_token_if(TokenKind::Get);
        let setter_token = if getter_token.is_none() {
            self.consume_token_if(TokenKind::Set)
        } else {
            None
        };
        let async_token = self.consume_token_if(TokenKind::Async);
        let generator_token = self.consume_token_if(TokenKind::Star);
        if let Some(identifier) = self.consume_token_if(TokenKind::Identifier) {
            if self.consume_token_if(TokenKind::Semicolon).is_some() {
                return Some(ClassMember::Property(PropertyDefinition {
                    name: identifier,
                    value: None,
                }));
            } else if self.consume_token_if(TokenKind::Eq).is_some() {
                let value = self.parse_expression();
                self.consume_token_if(TokenKind::Semicolon);
                return Some(ClassMember::Property(PropertyDefinition {
                    name: identifier,
                    value,
                }));
            } else if self.consume_token_if(TokenKind::LParen).is_some() {
                let parameters = self.parse_parameters();
                self.consume_token_if(TokenKind::RParen);
                if let Some(block) = self.parse_block_statement() {
                    if let StatementKind::BlockStatement(block) = block.kind {
                        if let Some(set_token) = setter_token {
                            return Some(ClassMember::Setter(SetterDefinition {
                                set_token,
                                name: identifier,
                                body: block,
                            }));
                        } else if let Some(get_token) = getter_token {
                            return Some(ClassMember::Getter(GetterDefinition {
                                get_token,
                                name: identifier,
                                body: block,
                            }));
                        } else {
                            return Some(ClassMember::Method(MethodDefinition {
                                async_token,
                                generator_token,
                                name: identifier,
                                parameters,
                                body: block,
                            }));
                        }
                    }
                }
            }
        } else if let Some(_constructor_token) = self.consume_token_if(TokenKind::Constructor) {
            self.consume_token_if(TokenKind::LParen)?;
            let parameters = self.parse_parameters();
            self.consume_token_if(TokenKind::RParen);
            if let Some(block) = self.parse_block_statement() {
                if let StatementKind::BlockStatement(block) = block.kind {
                    return Some(ClassMember::Constructor(ConstructorDefinition {
                        parameters,
                        block,
                    }));
                }
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct ClassDefinition {
    class_token: Token,
    class_name: Option<Token>,
    extends_token: Option<Token>,
    super_class_name: Option<Token>,
    members: Vec<ClassMember>,
    end_token: Token,
}

impl Node for ClassDefinition {
    fn span(&self) -> crate::tokenizer::Span {
        Span::concat(self.class_token.span, self.end_token.span)
    }
}

#[derive(Debug)]
pub enum ClassMember {
    Constructor(ConstructorDefinition),
    Method(MethodDefinition),
    Property(PropertyDefinition),
    Getter(GetterDefinition),
    Setter(SetterDefinition),
}

#[derive(Debug)]
pub struct ConstructorDefinition {
    parameters: Vec<Parameter>,
    block: BlockStatement,
}

#[derive(Debug)]
pub struct MethodDefinition {
    async_token: Option<Token>,
    generator_token: Option<Token>,
    name: Token,
    parameters: Vec<Parameter>,
    body: BlockStatement,
}

#[derive(Debug)]
pub struct PropertyDefinition {
    name: Token,
    value: Option<Expression>,
}

#[derive(Debug)]
pub struct SetterDefinition {
    set_token: Token,
    name: Token,
    body: BlockStatement,
}

#[derive(Debug)]
pub struct GetterDefinition {
    get_token: Token,
    name: Token,
    body: BlockStatement,
}

#[cfg(test)]
mod tests {
    use crate::parser::statement::StatementKind;
    use crate::parser::statement::block::BlockStatement;
    use crate::parser::statement::class_definition::{
        ClassDefinition, ClassMember, ConstructorDefinition, GetterDefinition, MethodDefinition,
        PropertyDefinition, SetterDefinition,
    };
    use crate::parser::statement::tests::parse_and_check;
    use crate::tokenizer::TokenKind; // Needed for method/constructor bodies

    // Helper to extract ClassDefinition
    fn get_class_def(stmt: crate::parser::statement::Statement) -> ClassDefinition {
        if let StatementKind::ClassDefinition(class_def) = stmt.kind {
            class_def
        } else {
            panic!("Expected ClassDefinition, got {:?}", stmt.kind);
        }
    }

    #[test]
    fn test_parse_class_definition_simple() {
        let source = "class MyClass {}";
        let stmt = parse_and_check(source, 0, 16);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.class_token.kind, TokenKind::Class);
        assert!(class_def.class_name.is_some());
        assert_eq!(class_def.class_name.unwrap().span.source(source), "MyClass");
        assert!(class_def.extends_token.is_none());
        assert!(class_def.super_class_name.is_none());
        assert!(class_def.members.is_empty());
    }

    #[test]
    fn test_parse_class_definition_with_extends() {
        let source = "class MyClass extends BaseClass {}";
        let stmt = parse_and_check(source, 0, 34);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.class_token.kind, TokenKind::Class);
        assert!(class_def.class_name.is_some());
        assert_eq!(class_def.class_name.unwrap().span.source(source), "MyClass");
        assert!(class_def.extends_token.is_some());
        assert_eq!(class_def.extends_token.unwrap().kind, TokenKind::Extends);
        assert!(class_def.super_class_name.is_some());
        assert_eq!(
            class_def.super_class_name.unwrap().span.source(source),
            "BaseClass"
        );
        assert!(class_def.members.is_empty());
    }

    #[test]
    fn test_parse_class_definition_with_constructor() {
        let source = "class MyClass { constructor(a) { this.a = a; } }";
        let stmt = parse_and_check(source, 0, 48);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Constructor(ctor) = &class_def.members[0] {
            assert_eq!(ctor.parameters.len(), 1);
            assert_eq!(ctor.parameters[0].span.source(source), "a");
            assert_eq!(ctor.block.body.len(), 1); // Assuming `this.a = a;` parses as one statement
        } else {
            panic!("Expected Constructor, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_method() {
        let source = "class MyClass { myMethod(x) { return x; } }";
        let stmt = parse_and_check(source, 0, 43);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Method(method) = &class_def.members[0] {
            assert_eq!(method.name.span.source(source), "myMethod");
            assert_eq!(method.parameters.len(), 1);
            assert_eq!(method.parameters[0].span.source(source), "x");
            assert!(!method.async_token.is_some());
            assert!(!method.generator_token.is_some());
            assert_eq!(method.body.body.len(), 1); // Assuming `return x;` parses as one statement
        } else {
            panic!("Expected Method, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_async_method() {
        let source = "class MyClass { async myAsyncMethod() {} }";
        let stmt = parse_and_check(source, 0, 42);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Method(method) = &class_def.members[0] {
            assert_eq!(method.name.span.source(source), "myAsyncMethod");
            assert!(method.async_token.is_some());
            assert!(!method.generator_token.is_some());
        } else {
            panic!("Expected Method, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_generator_method() {
        let source = "class MyClass { *myGeneratorMethod() {} }";
        let stmt = parse_and_check(source, 0, 41);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Method(method) = &class_def.members[0] {
            assert_eq!(method.name.span.source(source), "myGeneratorMethod");
            assert!(!method.async_token.is_some());
            assert!(method.generator_token.is_some());
        } else {
            panic!("Expected Method, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_property() {
        let source = "class MyClass { myField = 10; }";
        let stmt = parse_and_check(source, 0, 31);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Property(prop) = &class_def.members[0] {
            assert_eq!(prop.name.span.source(source), "myField");
            assert!(prop.value.is_some()); // Check if value is parsed
        } else {
            panic!("Expected Property, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_getter() {
        let source = "class MyClass { get value() { return this._value; } }";
        let stmt = parse_and_check(source, 0, 53);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Getter(getter) = &class_def.members[0] {
            assert_eq!(getter.name.span.source(source), "value");
            assert_eq!(getter.get_token.kind, TokenKind::Get);
            assert_eq!(getter.body.body.len(), 1);
        } else {
            panic!("Expected Getter, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_setter() {
        let source = "class MyClass { set value(v) { this._value = v; } }";
        let stmt = parse_and_check(source, 0, 51);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 1);
        if let ClassMember::Setter(setter) = &class_def.members[0] {
            assert_eq!(setter.name.span.source(source), "value");
            assert_eq!(setter.set_token.kind, TokenKind::Set);
            assert_eq!(setter.body.body.len(), 1);
        } else {
            panic!("Expected Setter, got {:?}", class_def.members[0]);
        }
    }

    #[test]
    fn test_parse_class_definition_with_mixed_members() {
        let source = "class MyClass { constructor() {} myMethod() {} get prop() {} set prop(v) {} myField = 1; }";
        let stmt = parse_and_check(source, 0, 90);
        let class_def = get_class_def(stmt);

        assert_eq!(class_def.members.len(), 5); // Constructor, Method, Getter, Setter, Property

        // Check types of members (order might vary based on parsing logic, but usually declaration order)
        assert!(matches!(class_def.members[0], ClassMember::Constructor(_)));
        assert!(matches!(class_def.members[1], ClassMember::Method(_)));
        assert!(matches!(class_def.members[2], ClassMember::Getter(_)));
        assert!(matches!(class_def.members[3], ClassMember::Setter(_)));
        assert!(matches!(class_def.members[4], ClassMember::Property(_)));
    }
}
