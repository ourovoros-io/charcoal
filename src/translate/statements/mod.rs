use crate::{error::Error, project::Project, translate::*};
use solang_parser::{helpers::CodeLocation, pt as solidity};
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
pub use arguments::*;
pub use block::*;
pub use do_while::*;
pub use emit::*;
pub use expression::*;
pub use r#for::*;
pub use r#if::*;
pub use r#return::*;
pub use revert::*;
pub use try_catch::*;
pub use variable::*;
pub use r#while::*;

#[inline(always)]
pub fn translate_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    statement: &solidity::Statement,
) -> Result<sway::Statement, Error> {
    // println!(
    //     "Translating statement {}",
    //     match project.loc_to_line_and_column(module.clone(), &statement.loc()) {
    //         Some((line, col)) => format!(
    //             "at {}:{}:{}",
    //             project
    //                 .options
    //                 .input
    //                 .join(module.borrow().path.clone())
    //                 .with_extension("sol")
    //                 .to_string_lossy(),
    //             line,
    //             col
    //         ),
    //         None => format!(
    //             "in {}...",
    //             project
    //                 .options
    //                 .input
    //                 .join(module.borrow().path.clone())
    //                 .with_extension("sol")
    //                 .to_string_lossy()
    //         ),
    //     },
    // );

    match statement {
        solidity::Statement::Block { statements, .. } => {
            translate_block_statement(project, module, scope.clone(), statements)
        }
        solidity::Statement::Assembly {
            dialect,
            flags,
            block,
            ..
        } => translate_assembly_statement(project, module, scope.clone(), dialect, flags, block),
        solidity::Statement::Args(_, named_arguments) => {
            translate_args_statement(project, module, scope.clone(), named_arguments)
        }
        solidity::Statement::If(_, condition, then_body, else_if) => translate_if_statement(
            project,
            module,
            scope.clone(),
            condition,
            then_body,
            else_if,
        ),
        solidity::Statement::While(_, condition, body) => {
            translate_while_statement(project, module, scope.clone(), condition, body)
        }
        solidity::Statement::Expression(_, expression) => {
            translate_expression_statement(project, module, scope.clone(), expression)
        }
        solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => {
            translate_variable_definition_statement(
                project,
                module,
                scope.clone(),
                variable_declaration,
                initializer.as_ref(),
            )
        }
        solidity::Statement::For(_, initialization, condition, update, body) => {
            translate_for_statement(
                project,
                module,
                scope.clone(),
                initialization,
                condition,
                update,
                body,
            )
        }
        solidity::Statement::DoWhile(_, body, condition) => {
            translate_do_while_statement(project, module, scope.clone(), body, condition)
        }
        solidity::Statement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
        solidity::Statement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
        solidity::Statement::Return(_, expression) => {
            translate_return_statement(project, module, scope.clone(), expression)
        }
        solidity::Statement::Revert(_, error_type, parameters) => {
            translate_revert_statement(project, module, scope.clone(), error_type, parameters)
        }
        solidity::Statement::RevertNamedArgs(_, path, named_args) => {
            translate_revert_named_arguments(project, module, scope.clone(), path, named_args)
        }
        solidity::Statement::Emit(_, expression) => {
            translate_emit_statement(project, module, scope.clone(), expression)
        }
        solidity::Statement::Try(_, expr, params_and_body, catch_clauses) => {
            translate_try_catch_statement(
                project,
                module,
                scope.clone(),
                expr,
                params_and_body,
                catch_clauses,
            )
        }
        solidity::Statement::Error(_) => {
            panic!("Encountered a statement that was not parsed correctly")
        }
    }
}
