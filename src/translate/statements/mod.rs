use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway};
use super::{TranslatedDefinition, TranslationScope};

pub mod arguments;
pub mod block;
pub mod do_while;
pub mod emit;
pub mod expression;
pub mod r#for;
pub mod r#if;
pub mod r#return;
pub mod revert;
pub mod try_catch;
pub mod variable;
pub mod r#while;

pub fn translate_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    statement: &solidity::Statement
) -> Result<sway::Statement, Error> {
    // println!("statement : {:#?}", statement);
    match statement {
        solidity::Statement::Block { statements, .. } => block::translate_block_statement(project, translated_definition, scope.clone(), statements),
        solidity::Statement::Assembly { dialect, flags, block, .. } => crate::translate::assembly::translate_assembly_statement(project, translated_definition, scope.clone(), dialect, flags, block),
        solidity::Statement::Args(_, named_arguments) => arguments::translate_args_statement(project, translated_definition, scope.clone(), named_arguments),
        solidity::Statement::If(_, condition, then_body, else_if) => r#if::translate_if_statement(project, translated_definition, scope.clone(), condition, then_body, else_if),
        solidity::Statement::While(_, condition, body) => r#while::translate_while_statement(project, translated_definition, scope.clone(), condition, body),
        solidity::Statement::Expression(_, expression) => expression::translate_expression_statement(project, translated_definition, scope.clone(), expression),
        solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => variable::translate_variable_definition_statement(project, translated_definition, scope.clone(), variable_declaration, initializer),
        solidity::Statement::For(_, initialization, condition, update, body) => r#for::translate_for_statement(project, translated_definition, scope.clone(), initialization, condition, update, body),
        solidity::Statement::DoWhile(_, body, condition) => do_while::translate_do_while_statement(project, translated_definition, scope.clone(), body, condition),
        solidity::Statement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
        solidity::Statement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
        solidity::Statement::Return(_, expression) => r#return::translate_return_statement(project, translated_definition, scope.clone(), expression),
        solidity::Statement::Revert(_, error_type, parameters) => revert::translate_revert_statement(project, translated_definition, scope.clone(), error_type, parameters),
        
        solidity::Statement::RevertNamedArgs(_, path, named_args) => {
            revert::translate_revert_named_arguments(project, translated_definition, scope.clone(), path, named_args)
        }

        solidity::Statement::Emit(_, expression) => emit::translate_emit_statement(project, translated_definition, scope.clone(), expression),
        solidity::Statement::Try(_, expr, params_and_body, catch_clauses) => try_catch::translate_try_catch_statement(project, translated_definition, scope, expr, params_and_body, catch_clauses),
        solidity::Statement::Error(_) => panic!("Encountered a statement that was not parsed correctly"),
    }
}
