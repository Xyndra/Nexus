//! Parser for the Nexus programming language.
//!
//! Transforms a stream of tokens into an Abstract Syntax Tree (AST).
//! Note: There are no binary or unary operators - all operations are function calls.

use crate::ast::*;
use nexus_core::{FunctionColor, NexusError, Span, VarModifiers};
use nexus_lexer::{Token, TokenKind};

/// The parser for Nexus source code.
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Create a new parser with the given tokens.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Parse a complete program.
    pub fn parse_program(&mut self) -> Result<Program, NexusError> {
        let start_span = self.current_span();
        let mut items = Vec::new();

        self.skip_terminators();

        while !self.is_at_end() {
            let item = self.parse_item()?;
            items.push(item);
            self.skip_terminators();
        }

        let end_span = self.previous_span();
        Ok(Program::new(items, start_span.to(&end_span)))
    }

    /// Parse a list of top-level items (for macro expansion).
    /// Unlike parse_program, this returns a Vec<Item> instead of a Program.
    pub fn parse_items(&mut self) -> Result<Vec<Item>, NexusError> {
        let mut items = Vec::new();

        self.skip_terminators();

        while !self.is_at_end() {
            let item = self.parse_item()?;
            items.push(item);
            self.skip_terminators();
        }

        Ok(items)
    }

    /// Parse a top-level item.
    fn parse_item(&mut self) -> Result<Item, NexusError> {
        match self.peek_kind() {
            TokenKind::Use => self.parse_use_statement().map(Item::Use),
            TokenKind::Struct => self.parse_struct().map(Item::Struct),
            TokenKind::Interface => self.parse_interface().map(Item::Interface),
            TokenKind::Impl => self.parse_impl_block().map(Item::Impl),
            TokenKind::Dollar => self.parse_macro_def_or_call(),
            TokenKind::Std | TokenKind::Compat | TokenKind::Plat => self.parse_function_or_method(),
            _ => Err(self.error(
                "Expected top-level item (use, struct, interface, impl, function, or macro)",
            )),
        }
    }

    /// Parse a use statement.
    /// Syntax: `use { symbol1, symbol2 } from module.path` or `use module.path`
    fn parse_use_statement(&mut self) -> Result<UseStatement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Use)?;

        // Check if this is a module-level import (use module.path) or symbol import (use { ... } from ...)
        if self.check(TokenKind::LeftBrace) {
            // Symbol import: use { symbol1, symbol2 } from module.path
            self.advance();
            let mut symbols = Vec::new();

            if !self.check(TokenKind::RightBrace) {
                symbols.push(self.expect_identifier()?);
                while self.check(TokenKind::Comma) {
                    self.advance();
                    if self.check(TokenKind::RightBrace) {
                        break; // Allow trailing comma
                    }
                    symbols.push(self.expect_identifier()?);
                }
            }

            self.expect(TokenKind::RightBrace)?;

            // Expect 'from' keyword
            self.expect(TokenKind::From)?;

            // Parse module path: compat.io
            // Note: module path components can be keywords like 'compat' or 'plat'
            let mut module_path = Vec::new();
            module_path.push(self.expect_module_path_component()?);

            while self.check(TokenKind::Dot) {
                self.advance();
                module_path.push(self.expect_module_path_component()?);
            }

            Ok(UseStatement {
                symbols,
                module_path,
                span: start_span.to(&self.previous_span()),
            })
        } else {
            // Module-level import: use module.path
            let mut module_path = Vec::new();
            module_path.push(self.expect_module_path_component()?);

            while self.check(TokenKind::Dot) {
                self.advance();
                module_path.push(self.expect_module_path_component()?);
            }

            // Empty symbols list indicates module-level import
            Ok(UseStatement {
                symbols: Vec::new(),
                module_path,
                span: start_span.to(&self.previous_span()),
            })
        }
    }

    /// Parse a struct definition.
    fn parse_struct(&mut self) -> Result<StructDefAst, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Struct)?;

        let name_span = self.current_span();
        let name = self.expect_identifier()?;
        let name_span = name_span.to(&self.previous_span());

        self.expect(TokenKind::LeftBrace)?;
        self.skip_terminators();

        let mut fields = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            fields.push(self.parse_field_def()?);
            self.skip_terminators();
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(StructDefAst {
            name,
            name_span,
            fields,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse an impl block.
    fn parse_impl_block(&mut self) -> Result<ImplBlockAst, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Impl)?;

        let struct_name = self.expect_identifier()?;

        // Optional interface: impl StructName : InterfaceName { ... }
        let interface_name = if self.check(TokenKind::Colon) {
            self.advance();
            Some(self.expect_identifier()?)
        } else {
            None
        };

        self.expect(TokenKind::LeftBrace)?;
        self.skip_terminators();

        let mut methods = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            methods.push(self.parse_impl_method()?);
            self.skip_terminators();
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(ImplBlockAst {
            struct_name,
            interface_name,
            methods,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a method inside an impl block.
    fn parse_impl_method(&mut self) -> Result<ImplMethod, NexusError> {
        let start_span = self.current_span();

        let color = self.parse_function_color()?;

        // Check for mutable self marker
        let mutable_self = if self.check_identifier("m") {
            self.advance();
            true
        } else {
            false
        };

        let name = self.expect_identifier()?;

        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        let return_contracts = self.parse_contracts()?;

        let body = self.parse_block()?;

        Ok(ImplMethod {
            color,
            mutable_self,
            name,
            params,
            return_type,
            return_contracts,
            body,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a field definition in a struct.
    fn parse_field_def(&mut self) -> Result<FieldDef, NexusError> {
        let start_span = self.current_span();

        let ty = self.parse_type()?;
        let name = self.expect_identifier()?;

        // Optional default value
        let default = if self.check(TokenKind::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Optional annotations
        let annotations = self.parse_annotations()?;

        Ok(FieldDef {
            name,
            ty,
            default,
            annotations,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse an interface definition.
    fn parse_interface(&mut self) -> Result<InterfaceDefAst, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Interface)?;

        let name_span = self.current_span();
        let name = self.expect_identifier()?;
        let name_span = name_span.to(&self.previous_span());

        // Optional extends clause
        let extends = if self.check_identifier("extends") {
            self.advance();
            self.parse_identifier_list()?
        } else {
            Vec::new()
        };

        self.expect(TokenKind::LeftBrace)?;
        self.skip_terminators();

        let mut methods = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            methods.push(self.parse_method_signature()?);
            self.skip_terminators();
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(InterfaceDefAst {
            name,
            name_span,
            extends,
            methods,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a method signature in an interface.
    fn parse_method_signature(&mut self) -> Result<MethodSignature, NexusError> {
        let start_span = self.current_span();

        // Check for mutable receiver: (m) or ()
        let receiver_mutable = if self.check(TokenKind::LeftParen) {
            self.advance();
            let is_mutable = if self.check_identifier("m") {
                self.advance();
                true
            } else {
                false
            };
            self.expect(TokenKind::RightParen)?;
            is_mutable
        } else {
            false
        };

        let name = self.expect_identifier()?;

        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        Ok(MethodSignature {
            receiver_mutable,
            name,
            params,
            return_type,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a function or method definition.
    fn parse_function_or_method(&mut self) -> Result<Item, NexusError> {
        let start_span = self.current_span();
        let color = self.parse_function_color()?;

        // Check if this is a method (has receiver in parentheses)
        if self.check(TokenKind::LeftParen) {
            return self.parse_method(start_span, color).map(Item::Method);
        }

        // Regular function
        let name_span = self.current_span();
        let name = self.expect_identifier()?;
        let name_span = name_span.to(&self.previous_span());

        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        let return_contracts = self.parse_contracts()?;

        let body = self.parse_block()?;

        Ok(Item::Function(FunctionDef {
            color,
            name,
            name_span,
            params,
            return_type,
            return_contracts,
            body,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse a method definition (function on a struct).
    fn parse_method(
        &mut self,
        start_span: Span,
        color: FunctionColor,
    ) -> Result<MethodDef, NexusError> {
        self.expect(TokenKind::LeftParen)?;

        // Parse receiver: (m TypeName name) or (TypeName name)
        let receiver_mutable = if self.check_identifier("m") {
            self.advance();
            true
        } else {
            false
        };

        let receiver_type = self.expect_identifier()?;
        let receiver_name = self.expect_identifier()?;

        self.expect(TokenKind::RightParen)?;

        let name_span = self.current_span();
        let name = self.expect_identifier()?;
        let name_span = name_span.to(&self.previous_span());

        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        let return_contracts = self.parse_contracts()?;

        let body = self.parse_block()?;

        Ok(MethodDef {
            color,
            receiver_mutable,
            receiver_type,
            receiver_name,
            name,
            name_span,
            params,
            return_type,
            return_contracts,
            body,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a macro definition or top-level macro call.
    /// Macro definition: `$name(...): macro { ... }`
    /// Top-level macro call: `$name(...)`
    fn parse_macro_def_or_call(&mut self) -> Result<Item, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Dollar)?;

        let name = self.expect_identifier()?;

        self.expect(TokenKind::LeftParen)?;

        // Look ahead to see if this is a definition (has typed params) or a call (has expressions)
        // For now, we check if the next token after closing paren is `:` for definition
        // We need to parse the content first, then decide based on what follows

        // Save position to potentially backtrack
        let params_start = self.current;

        // Try to determine if this is a definition by looking for type patterns
        // A definition has parameters like `[dyn]rune name` while a call has expressions like `"string"`
        let is_definition = self.looks_like_macro_params();

        // Reset position
        self.current = params_start;

        if is_definition {
            // Parse as definition
            let params = self.parse_parameters()?;
            self.expect(TokenKind::RightParen)?;
            self.expect(TokenKind::Colon)?;
            self.expect(TokenKind::Macro)?;
            let body = self.parse_block()?;

            Ok(Item::Macro(MacroDef {
                name,
                params,
                body,
                span: start_span.to(&self.previous_span()),
            }))
        } else {
            // Parse as top-level macro call
            let args = self.parse_macro_call_args()?;
            self.expect(TokenKind::RightParen)?;

            Ok(Item::TopLevelMacroCall(TopLevelMacroCall {
                name,
                args,
                span: start_span.to(&self.previous_span()),
            }))
        }
    }

    /// Parse arguments for a macro call (comma-separated expressions)
    fn parse_macro_call_args(&mut self) -> Result<Vec<Expression>, NexusError> {
        let mut args = Vec::new();

        if !self.check(TokenKind::RightParen) {
            args.push(self.parse_expression()?);
            while self.check(TokenKind::Comma) {
                self.advance();
                if self.check(TokenKind::RightParen) {
                    break; // Allow trailing comma
                }
                args.push(self.parse_expression()?);
            }
        }

        Ok(args)
    }

    /// Check if the current position looks like macro parameters (for a definition)
    /// rather than expression arguments (for a call).
    /// Returns true if it looks like `[dyn]Type name` or `Type name` patterns.
    fn looks_like_macro_params(&mut self) -> bool {
        // If empty params, check what follows the closing paren
        if self.check(TokenKind::RightParen) {
            // Look ahead past the closing paren
            let saved = self.current;
            self.advance(); // skip )
            let is_def = self.check(TokenKind::Colon);
            self.current = saved;
            return is_def;
        }

        // Check first token - if it's a string literal, it's definitely a call
        if self.check_string_literal() || self.check_int_literal() {
            return false;
        }

        // If it starts with `[`, it could be array type (definition) or array literal (call)
        // Array types are like `[dyn]Type` or `[5]Type`
        // Array literals would be followed by expressions
        if self.check(TokenKind::LeftBracket) {
            let saved = self.current;
            self.advance(); // skip [

            // Check if it's [dyn] or [number] - type syntax
            let looks_like_type = self.check(TokenKind::Dyn) || self.check_int_literal();
            self.current = saved;

            if looks_like_type {
                return true;
            }
            return false;
        }

        // If it's an identifier, check if it's followed by another identifier (Type name pattern)
        // or by a comma/close paren (expression)
        if self.check_any_identifier() {
            let saved = self.current;
            self.advance(); // skip first identifier

            // If followed by another identifier, it's likely `Type name` pattern
            let is_type_name_pattern = self.check_any_identifier();
            self.current = saved;

            return is_type_name_pattern;
        }

        // Default to definition for other cases
        true
    }

    /// Check if current token is any string literal
    fn check_string_literal(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::StringLiteral(_))
    }

    /// Check if current token is any int literal
    fn check_int_literal(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::IntLiteral(_))
    }

    /// Check if current token is any identifier
    fn check_any_identifier(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Identifier(_))
    }

    /// Parse function color (std, compat, plat).
    fn parse_function_color(&mut self) -> Result<FunctionColor, NexusError> {
        match self.peek_kind() {
            TokenKind::Std => {
                self.advance();
                Ok(FunctionColor::Std)
            }
            TokenKind::Compat => {
                self.advance();
                Ok(FunctionColor::Compat)
            }
            TokenKind::Plat => {
                self.advance();
                Ok(FunctionColor::Plat)
            }
            _ => Err(self.error("Expected function color (std, compat, or plat)")),
        }
    }

    /// Parse a list of parameters.
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, NexusError> {
        let mut params = Vec::new();

        if !self.check(TokenKind::RightParen) {
            loop {
                params.push(self.parse_parameter()?);
                if !self.check(TokenKind::Comma) {
                    break;
                }
                self.advance();
            }
        }

        Ok(params)
    }

    /// Parse a single parameter.
    fn parse_parameter(&mut self) -> Result<Parameter, NexusError> {
        let start_span = self.current_span();

        // Check for 'm' modifier indicating mutable parameter
        let mutable = if self.check_identifier("m") {
            self.advance();
            true
        } else {
            false
        };

        let ty = self.parse_type()?;
        let name = self.expect_identifier()?;
        let contracts = self.parse_contracts()?;

        Ok(Parameter {
            mutable,
            name,
            ty,
            contracts,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a type expression.
    fn parse_type(&mut self) -> Result<TypeExpr, NexusError> {
        let start_span = self.current_span();

        // Array type: [N]T or [dyn]T
        if self.check(TokenKind::LeftBracket) {
            self.advance();

            let size = if self.check(TokenKind::Dyn) {
                self.advance();
                ArraySizeExpr::Dynamic
            } else if let TokenKind::IntLiteral(n) = self.peek_kind() {
                self.advance();
                ArraySizeExpr::Fixed(n as usize)
            } else {
                return Err(self.error("Expected array size (number or 'dyn')"));
            };

            self.expect(TokenKind::RightBracket)?;

            let element = self.parse_type()?;

            return Ok(TypeExpr::Array {
                element: Box::new(element),
                size,
                span: start_span.to(&self.previous_span()),
            });
        }

        // Unknown type: unknown<A, B, C>
        if self.check(TokenKind::Unknown) {
            self.advance();
            self.expect(TokenKind::Less)?;

            let mut variants = Vec::new();
            loop {
                variants.push(self.parse_type()?);
                if !self.check(TokenKind::Comma) {
                    break;
                }
                self.advance();
            }

            self.expect(TokenKind::Greater)?;

            return Ok(TypeExpr::Unknown {
                variants,
                span: start_span.to(&self.previous_span()),
            });
        }

        // Void type
        if self.check(TokenKind::Void) {
            self.advance();
            return Ok(TypeExpr::Void {
                span: start_span.to(&self.previous_span()),
            });
        }

        // Primitive or named type
        if let Some(name) = self.try_parse_type_name() {
            return Ok(TypeExpr::Named {
                name,
                span: start_span.to(&self.previous_span()),
            });
        }

        Err(self.error("Expected type"))
    }

    /// Try to parse a type name (primitive or identifier).
    fn try_parse_type_name(&mut self) -> Option<String> {
        let name = match self.peek_kind() {
            TokenKind::Bool => "bool".to_string(),
            TokenKind::I8 => "i8".to_string(),
            TokenKind::I16 => "i16".to_string(),
            TokenKind::I32 => "i32".to_string(),
            TokenKind::I64 => "i64".to_string(),
            TokenKind::U8 => "u8".to_string(),
            TokenKind::U16 => "u16".to_string(),
            TokenKind::U32 => "u32".to_string(),
            TokenKind::U64 => "u64".to_string(),
            TokenKind::F32 => "f32".to_string(),
            TokenKind::F64 => "f64".to_string(),
            TokenKind::Rune => "rune".to_string(),
            TokenKind::Identifier(ref s) => s.clone(),
            _ => return None,
        };
        self.advance();
        Some(name)
    }

    /// Parse annotations (@name or @name(args)).
    fn parse_annotations(&mut self) -> Result<Vec<Annotation>, NexusError> {
        let mut annotations = Vec::new();

        while self.check(TokenKind::At) {
            let start_span = self.current_span();
            self.advance();

            let name = self.expect_identifier()?;

            let args = if self.check(TokenKind::LeftParen) {
                self.advance();
                let mut args = Vec::new();
                if !self.check(TokenKind::RightParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.check(TokenKind::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(TokenKind::RightParen)?;
                args
            } else {
                Vec::new()
            };

            annotations.push(Annotation {
                name,
                args,
                span: start_span.to(&self.previous_span()),
            });
        }

        Ok(annotations)
    }

    /// Parse contracts (@range, @not_null, etc.).
    fn parse_contracts(&mut self) -> Result<Vec<ContractExpr>, NexusError> {
        let mut contracts = Vec::new();

        while self.check(TokenKind::At) {
            let start_span = self.current_span();
            self.advance();

            let name = self.expect_identifier()?;

            let kind = match name.as_str() {
                "range" => {
                    self.expect(TokenKind::LeftParen)?;
                    let min = self.parse_expression()?;
                    self.expect(TokenKind::Comma)?;
                    let max = self.parse_expression()?;
                    self.expect(TokenKind::RightParen)?;
                    ContractKindExpr::Range {
                        min: Box::new(min),
                        max: Box::new(max),
                    }
                }
                "not_null" => ContractKindExpr::NotNull,
                "predicate" => {
                    self.expect(TokenKind::LeftParen)?;
                    let func_name = self.expect_identifier()?;
                    self.expect(TokenKind::RightParen)?;
                    ContractKindExpr::Predicate(func_name)
                }
                "custom" => {
                    self.expect(TokenKind::LeftParen)?;
                    let expr = if let TokenKind::StringLiteral(s) = self.peek_kind() {
                        self.advance();
                        s
                    } else {
                        return Err(self.error("Expected string literal for custom contract"));
                    };
                    self.expect(TokenKind::RightParen)?;
                    ContractKindExpr::Custom(expr)
                }
                _ => {
                    // Unknown contract, treat as custom
                    ContractKindExpr::Custom(name)
                }
            };

            contracts.push(ContractExpr {
                kind,
                span: start_span.to(&self.previous_span()),
            });
        }

        Ok(contracts)
    }

    /// Parse a block of statements.
    pub fn parse_block(&mut self) -> Result<Block, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::LeftBrace)?;
        self.skip_terminators();

        let mut statements = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
            self.skip_terminators();
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(Block::new(statements, start_span.to(&self.previous_span())))
    }

    /// Parse a statement.
    pub fn parse_statement(&mut self) -> Result<Statement, NexusError> {
        // Check for special statement keywords
        match self.peek_kind() {
            TokenKind::Return => return self.parse_return_statement(),
            TokenKind::If => return self.parse_if_statement(),
            TokenKind::Defer => return self.parse_defer_statement(),
            TokenKind::Subscope => return self.parse_subscope_statement(),
            TokenKind::Goto => return self.parse_goto_statement(),
            TokenKind::Exit => return self.parse_exit_statement(),
            TokenKind::LeftBrace => return self.parse_block().map(Statement::Block),
            _ => {}
        }

        // Check for variable declaration (starts with modifier letters)
        if let TokenKind::Identifier(ref s) = self.peek_kind()
            && VarModifiers::parse(s).is_some()
        {
            return self.parse_var_decl();
        }

        // Expression statement (could be assignment if followed by =)
        self.parse_expression_or_assignment()
    }

    /// Parse a variable declaration.
    fn parse_var_decl(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();

        // Parse modifiers
        let modifiers = if let TokenKind::Identifier(ref s) = self.peek_kind() {
            let mods = VarModifiers::parse(s).unwrap_or_default();
            self.advance();
            mods
        } else {
            VarModifiers::default()
        };

        // Validate modifier combinations
        if !modifiers.is_valid() {
            return Err(NexusError::InvalidModifiers {
                message: "const (c) is incompatible with mutable (m) and locked (l)".to_string(),
                span: start_span,
            });
        }

        let name = self.expect_identifier()?;

        // Validate that the variable name is not a valid modifier string
        // This prevents confusing code like `m l = 5` where `l` could be misread as a modifier
        if VarModifiers::parse(&name).is_some() {
            return Err(NexusError::ParseError {
                message: format!(
                    "Variable name '{}' is invalid because it can be misinterpreted as variable modifiers. \
                    Use a different name that is not composed solely of modifier characters (c, m, l, h, u, g).",
                    name
                ),
                span: self.previous_span(),
            });
        }

        // Optional type annotation
        let ty = if self.check(TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Equal)?;
        let init = self.parse_expression()?;

        Ok(Statement::VarDecl(VarDecl {
            modifiers,
            name,
            ty,
            init,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse an expression or assignment statement.
    fn parse_expression_or_assignment(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        let expr = self.parse_expression()?;

        if self.check(TokenKind::Equal) {
            self.advance();
            let value = self.parse_expression()?;
            Ok(Statement::Assignment(Assignment {
                target: expr,
                value,
                span: start_span.to(&self.previous_span()),
            }))
        } else {
            Ok(Statement::Expression(expr))
        }
    }

    /// Parse a return statement.
    fn parse_return_statement(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Return)?;

        let value = if !self.check_terminator() && !self.check(TokenKind::RightBrace) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Statement::Return(ReturnStmt {
            value,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse an if statement.
    fn parse_if_statement(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::If)?;

        let condition = self.parse_if_condition()?;
        let then_block = self.parse_block()?;

        let else_block = if self.check(TokenKind::Else) {
            self.advance();
            if self.check(TokenKind::If) {
                let else_if = self.parse_if_statement()?;
                if let Statement::If(if_stmt) = else_if {
                    Some(ElseClause::ElseIf(Box::new(if_stmt)))
                } else {
                    unreachable!()
                }
            } else {
                Some(ElseClause::Block(self.parse_block()?))
            }
        } else {
            None
        };

        Ok(Statement::If(IfStmt {
            condition,
            then_block,
            else_block,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse an if condition (boolean or pattern matching).
    fn parse_if_condition(&mut self) -> Result<IfCondition, NexusError> {
        // Check for boolean condition: if (expr)
        if self.check(TokenKind::LeftParen) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect(TokenKind::RightParen)?;
            return Ok(IfCondition::Boolean(expr));
        }

        // Pattern matching condition: if matcher(x) { cases }
        let matcher = self.parse_expression()?;
        self.expect(TokenKind::LeftBrace)?;
        self.skip_terminators();

        let mut cases = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            cases.push(self.parse_pattern_case()?);
            self.skip_terminators();
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(IfCondition::Pattern { matcher, cases })
    }

    /// Parse a pattern case.
    fn parse_pattern_case(&mut self) -> Result<PatternCase, NexusError> {
        let start_span = self.current_span();

        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::Arrow)?;

        let body = if self.check(TokenKind::LeftBrace) {
            PatternBody::Block(self.parse_block()?)
        } else {
            PatternBody::Expression(self.parse_expression()?)
        };

        Ok(PatternCase {
            pattern,
            body,
            span: start_span.to(&self.previous_span()),
        })
    }

    /// Parse a pattern.
    fn parse_pattern(&mut self) -> Result<Pattern, NexusError> {
        let span = self.current_span();

        match self.peek_kind() {
            TokenKind::IntLiteral(n) => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::Int(n),
                    span,
                }))
            }
            TokenKind::FloatLiteral(n) => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::Float(n),
                    span,
                }))
            }
            TokenKind::StringLiteral(s) => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::String(s),
                    span,
                }))
            }
            TokenKind::CharLiteral(c) => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::Char(c),
                    span,
                }))
            }
            TokenKind::True => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::Bool(true),
                    span,
                }))
            }
            TokenKind::False => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::Bool(false),
                    span,
                }))
            }
            TokenKind::None => {
                self.advance();
                Ok(Pattern::Literal(Literal {
                    kind: LiteralKind::None,
                    span,
                }))
            }
            TokenKind::Identifier(ref s) if s == "_" => {
                self.advance();
                Ok(Pattern::Wildcard(span))
            }
            TokenKind::Identifier(s) => {
                self.advance();
                Ok(Pattern::Binding { name: s, span })
            }
            _ => Err(self.error("Expected pattern")),
        }
    }

    /// Parse a defer statement.
    fn parse_defer_statement(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Defer)?;
        let body = self.parse_block()?;

        Ok(Statement::Defer(DeferStmt {
            body,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse a subscope statement.
    fn parse_subscope_statement(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Subscope)?;
        let name = self.expect_identifier()?;
        let body = self.parse_block()?;

        Ok(Statement::Subscope(SubscopeStmt {
            name,
            body,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse a goto statement.
    fn parse_goto_statement(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Goto)?;
        let label = self.expect_identifier()?;

        Ok(Statement::Goto(GotoStmt {
            label,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse an exit statement.
    fn parse_exit_statement(&mut self) -> Result<Statement, NexusError> {
        let start_span = self.current_span();
        self.expect(TokenKind::Exit)?;
        let label = self.expect_identifier()?;

        Ok(Statement::Exit(ExitStmt {
            label,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse an expression.
    pub fn parse_expression(&mut self) -> Result<Expression, NexusError> {
        self.parse_primary_expression()
    }

    /// Parse a primary expression with optional postfix operations.
    fn parse_primary_expression(&mut self) -> Result<Expression, NexusError> {
        let mut expr = self.parse_atom()?;

        // Parse postfix operations (field access, index, method call)
        loop {
            if self.check(TokenKind::Dot) {
                expr = self.parse_field_or_method_access(expr)?;
            } else if self.check(TokenKind::LeftBracket) {
                expr = self.parse_index_access(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse an atomic expression (literals, variables, calls, etc.).
    fn parse_atom(&mut self) -> Result<Expression, NexusError> {
        let span = self.current_span();

        match self.peek_kind() {
            // Literals
            TokenKind::IntLiteral(n) => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::Int(n),
                    span,
                }))
            }
            TokenKind::FloatLiteral(n) => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::Float(n),
                    span,
                }))
            }
            TokenKind::StringLiteral(s) => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::String(s),
                    span,
                }))
            }
            TokenKind::CharLiteral(c) => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::Char(c),
                    span,
                }))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::Bool(true),
                    span,
                }))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::Bool(false),
                    span,
                }))
            }
            TokenKind::None => {
                self.advance();
                Ok(Expression::Literal(Literal {
                    kind: LiteralKind::None,
                    span,
                }))
            }

            // Macro call: $name(args)
            TokenKind::Dollar => {
                self.advance();
                let name = self.expect_identifier()?;
                self.expect(TokenKind::LeftParen)?;
                let args = self.parse_arguments()?;
                self.expect(TokenKind::RightParen)?;
                Ok(Expression::MacroCall(MacroCallExpr {
                    name,
                    args,
                    span: span.to(&self.previous_span()),
                }))
            }

            // Array literal: [elem1, elem2, ...]
            TokenKind::LeftBracket => {
                self.advance();
                let mut elements = Vec::new();
                if !self.check(TokenKind::RightBracket) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.check(TokenKind::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect(TokenKind::RightBracket)?;
                Ok(Expression::Array(ArrayExpr {
                    elements,
                    span: span.to(&self.previous_span()),
                }))
            }

            // Grouped expression or lambda: (expr) or (params): type { body }
            TokenKind::LeftParen => {
                self.advance();

                // Check if this is a lambda
                if self.is_lambda_start() {
                    return self.parse_lambda(span);
                }

                let expr = self.parse_expression()?;
                self.expect(TokenKind::RightParen)?;
                Ok(Expression::Grouped(
                    Box::new(expr),
                    span.to(&self.previous_span()),
                ))
            }

            // Identifier: variable reference, function call, or struct init
            TokenKind::Identifier(name) => {
                self.advance();

                // Function call: name(args)
                if self.check(TokenKind::LeftParen) {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(TokenKind::RightParen)?;
                    return Ok(Expression::Call(CallExpr {
                        function: name,
                        args,
                        span: span.to(&self.previous_span()),
                    }));
                }

                // Struct initialization: Name { field: value, ... }
                if self.check(TokenKind::LeftBrace) {
                    return self.parse_struct_init(name, span);
                }

                // Variable reference
                Ok(Expression::Variable(VariableRef { name, span }))
            }

            _ => Err(self.error("Expected expression")),
        }
    }

    /// Check if we're at the start of a lambda.
    fn is_lambda_start(&self) -> bool {
        // Simple heuristic: if next token is ')' followed by ':', it's a lambda
        // or if we see 'type name,' patterns
        let mut depth = 1;
        let mut i = self.current;

        while i < self.tokens.len() && depth > 0 {
            match &self.tokens[i].kind {
                TokenKind::LeftParen => depth += 1,
                TokenKind::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if followed by ':'
                        if i + 1 < self.tokens.len() {
                            return matches!(self.tokens[i + 1].kind, TokenKind::Colon);
                        }
                    }
                }
                _ => {}
            }
            i += 1;
        }

        false
    }

    /// Parse a lambda expression.
    fn parse_lambda(&mut self, start_span: Span) -> Result<Expression, NexusError> {
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RightParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = Some(self.parse_type()?);

        let body = if self.check(TokenKind::LeftBrace) {
            LambdaBody::Block(self.parse_block()?)
        } else {
            LambdaBody::Expression(Box::new(self.parse_expression()?))
        };

        Ok(Expression::Lambda(LambdaExpr {
            captures: Vec::new(), // Will be filled in during analysis
            params,
            return_type,
            body,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse struct initialization.
    fn parse_struct_init(
        &mut self,
        name: String,
        start_span: Span,
    ) -> Result<Expression, NexusError> {
        self.expect(TokenKind::LeftBrace)?;
        self.skip_terminators();

        let mut fields = Vec::new();
        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let field_span = self.current_span();
            let field_name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let value = self.parse_expression()?;

            fields.push(FieldInit {
                name: field_name,
                value,
                span: field_span.to(&self.previous_span()),
            });

            if !self.check(TokenKind::Comma) && !self.check_terminator() {
                break;
            }
            if self.check(TokenKind::Comma) {
                self.advance();
            }
            self.skip_terminators();
        }

        self.expect(TokenKind::RightBrace)?;

        Ok(Expression::StructInit(StructInitExpr {
            name,
            fields,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse field access or method call.
    fn parse_field_or_method_access(
        &mut self,
        object: Expression,
    ) -> Result<Expression, NexusError> {
        let start_span = *object.span();
        self.expect(TokenKind::Dot)?;

        let name = self.expect_identifier()?;

        // Method call: obj.method(args)
        if self.check(TokenKind::LeftParen) {
            self.advance();
            let args = self.parse_arguments()?;
            self.expect(TokenKind::RightParen)?;
            return Ok(Expression::MethodCall(MethodCallExpr {
                receiver: Box::new(object),
                method: name,
                args,
                span: start_span.to(&self.previous_span()),
            }));
        }

        // Field access: obj.field
        Ok(Expression::FieldAccess(FieldAccessExpr {
            object: Box::new(object),
            field: name,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse index access.
    fn parse_index_access(&mut self, array: Expression) -> Result<Expression, NexusError> {
        let start_span = *array.span();
        self.expect(TokenKind::LeftBracket)?;

        // Check for @unchecked annotation
        let unchecked = if self.check(TokenKind::At) {
            self.advance();
            let name = self.expect_identifier()?;
            if name != "unchecked" {
                return Err(self.error("Expected @unchecked"));
            }
            true
        } else {
            false
        };

        let index = self.parse_expression()?;
        self.expect(TokenKind::RightBracket)?;

        Ok(Expression::Index(IndexExpr {
            array: Box::new(array),
            index: Box::new(index),
            unchecked,
            span: start_span.to(&self.previous_span()),
        }))
    }

    /// Parse a list of arguments.
    fn parse_arguments(&mut self) -> Result<Vec<Expression>, NexusError> {
        let mut args = Vec::new();

        if !self.check(TokenKind::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.check(TokenKind::Comma) {
                    break;
                }
                self.advance();
            }
        }

        Ok(args)
    }

    /// Parse a comma-separated list of identifiers.
    fn parse_identifier_list(&mut self) -> Result<Vec<String>, NexusError> {
        let mut names = Vec::new();
        loop {
            names.push(self.expect_identifier()?);
            if !self.check(TokenKind::Comma) {
                break;
            }
            self.advance();
        }
        Ok(names)
    }

    // === Helper methods ===

    /// Check if we're at the end of input.
    fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    /// Get the current token's kind.
    fn peek_kind(&self) -> TokenKind {
        self.tokens
            .get(self.current)
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::Eof)
    }

    /// Get the current token's span.
    fn current_span(&self) -> Span {
        self.tokens
            .get(self.current)
            .map(|t| t.span)
            .unwrap_or(Span::dummy())
    }

    /// Get the previous token's span.
    fn previous_span(&self) -> Span {
        if self.current > 0 {
            self.tokens[self.current - 1].span
        } else {
            Span::dummy()
        }
    }

    /// Advance to the next token.
    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1)
    }

    /// Check if current token matches the given kind.
    fn check(&self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.peek_kind()) == std::mem::discriminant(&kind)
    }

    /// Check if current token is a specific identifier.
    fn check_identifier(&self, name: &str) -> bool {
        matches!(&self.peek_kind(), TokenKind::Identifier(s) if s == name)
    }

    /// Check if current token is a terminator (newline or semicolon).
    fn check_terminator(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Newline | TokenKind::Semicolon)
    }

    /// Skip newlines and semicolons.
    fn skip_terminators(&mut self) {
        while self.check_terminator() {
            self.advance();
        }
    }

    /// Expect a specific token kind.
    fn expect(&mut self, kind: TokenKind) -> Result<&Token, NexusError> {
        if self.check(kind.clone()) {
            Ok(self.advance().unwrap())
        } else {
            Err(NexusError::UnexpectedToken {
                expected: kind.name().to_string(),
                found: self.peek_kind().name().to_string(),
                span: self.current_span(),
            })
        }
    }

    /// Expect an identifier and return its name.
    fn expect_identifier(&mut self) -> Result<String, NexusError> {
        if let TokenKind::Identifier(name) = self.peek_kind() {
            self.advance();
            Ok(name)
        } else {
            Err(NexusError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.peek_kind().name().to_string(),
                span: self.current_span(),
            })
        }
    }

    /// Expect a module path component (identifier or certain keywords like 'compat', 'plat', 'std').
    fn expect_module_path_component(&mut self) -> Result<String, NexusError> {
        match self.peek_kind() {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            TokenKind::Compat => {
                self.advance();
                Ok("compat".to_string())
            }
            TokenKind::Plat => {
                self.advance();
                Ok("plat".to_string())
            }
            TokenKind::Std => {
                self.advance();
                Ok("std".to_string())
            }
            _ => Err(NexusError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.peek_kind().name().to_string(),
                span: self.current_span(),
            }),
        }
    }

    /// Create an error at the current position.
    fn error(&self, message: &str) -> NexusError {
        NexusError::ParseError {
            message: message.to_string(),
            span: self.current_span(),
        }
    }
}
