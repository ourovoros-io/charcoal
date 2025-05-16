use crate::{errors::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

mod arguments;
mod block;
mod do_while;
mod emit;
mod expression;
mod r#for;
mod r#if;
mod r#return;
mod revert;
mod try_catch;
mod variable;
mod r#while;

pub use self::{
    arguments::*,
    block::*,
    do_while::*,
    emit::*,
    expression::*,
    r#for::*,
    r#if::*,
    r#return::*,
    revert::*,
    try_catch::*,
    variable::*,
    r#while::*,
};

pub fn translate_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    statement: &solidity::Statement
) -> Result<sway::Statement, Error> {
    // println!("statement : {:#?}", statement);
    match statement {
        solidity::Statement::Block { statements, .. } => block::translate_block_statement(project, translated_definition, scope, statements),
        solidity::Statement::Assembly { dialect, flags, block, .. } => crate::translate::assembly::translate_assembly_statement(project, translated_definition, scope, dialect, flags, block),
        solidity::Statement::Args(_, named_arguments) => arguments::translate_args_statement(project, translated_definition, scope, named_arguments),
        solidity::Statement::If(_, condition, then_body, else_if) => r#if::translate_if_statement(project, translated_definition, scope, condition, then_body, else_if),
        solidity::Statement::While(_, condition, body) => r#while::translate_while_statement(project, translated_definition, scope, condition, body),
        solidity::Statement::Expression(_, expression) => expression::translate_expression_statement(project, translated_definition, scope, expression),
        solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => variable::translate_variable_definition_statement(project, translated_definition, scope, variable_declaration, initializer.as_ref()),
        solidity::Statement::For(_, initialization, condition, update, body) => r#for::translate_for_statement(project, translated_definition, scope, initialization, condition, update, body),
        solidity::Statement::DoWhile(_, body, condition) => do_while::translate_do_while_statement(project, translated_definition, scope, body, condition),
        solidity::Statement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
        solidity::Statement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
        solidity::Statement::Return(_, expression) => r#return::translate_return_statement(project, translated_definition, scope, expression),
        solidity::Statement::Revert(_, error_type, parameters) => revert::translate_revert_statement(project, translated_definition, scope, error_type, parameters),
        
        solidity::Statement::RevertNamedArgs(_, path, named_args) => {
            revert::translate_revert_named_arguments(project, translated_definition, scope, path, named_args)
        }

        solidity::Statement::Emit(_, expression) => emit::translate_emit_statement(project, translated_definition, scope, expression),
        solidity::Statement::Try(_, expr, params_and_body, catch_clauses) => try_catch::translate_try_catch_statement(project, translated_definition, scope, expr, params_and_body, catch_clauses),
        solidity::Statement::Error(_) => panic!("Encountered a statement that was not parsed correctly"),
    }
}
