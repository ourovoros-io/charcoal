use crate::{error::Error, project::Project, translate::*};
use std::{cell::RefCell, rc::Rc};

pub fn translate_block(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    statements: &[solidity::Statement],
) -> Result<sway::Block, Error> {
    let mut block = sway::Block::default();

    // Translate each of the statements in the block
    for statement in statements {
        // Translate the statement
        let sway_statement = match translate_statement(project, module.clone(), scope.clone(), statement) {
            Ok(statement) => statement,
            Err(Error::IneffectualStatement(_, _statement)) => {
                // println!("WARNING: Skipping ineffectual statement: {statement}");
                continue;
            }
            Err(error) => return Err(error),
        };

        // Store the index of the sway statement
        let statement_index = block.statements.len();

        // Add the sway statement to the sway block
        block.statements.push(sway_statement);

        // If the sway statement is a variable declaration, keep track of its statement index
        if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
            let store_variable_statement_index = |id: &sway::LetIdentifier| {
                if id.name == "_" {
                    return;
                }

                let scope_entry = scope
                    .borrow()
                    .find_variable(|v| v.borrow().new_name == id.name)
                    .unwrap();

                scope_entry.borrow_mut().statement_index = Some(statement_index);
            };

            match &sway_variable.pattern {
                sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
            }
        }
    }

    finalize_block_translation(project, scope.clone(), &mut block)?;

    Ok(block)
}

pub fn finalize_block_translation(
    _project: &mut Project,
    scope: Rc<RefCell<ir::Scope>>,
    block: &mut sway::Block,
) -> Result<(), Error> {
    // Check the block for variable declarations that need to be marked mutable
    for variable in scope.borrow().get_variables() {
        // Only check variables that are declared as statements
        let Some(statement_index) = variable.borrow().statement_index else {
            continue;
        };

        // If the variable has any mutations, mark it as mutable
        if variable.borrow().mutation_count > 0 {
            if statement_index >= block.statements.len() {
                panic!(
                    "Variable statement index out of bounds: {} >= {} - {block:#?}",
                    statement_index,
                    block.statements.len()
                );
            }
            let let_statement = match &mut block.statements[statement_index] {
                sway::Statement::Let(let_statement) => let_statement,
                statement => panic!(
                    "Expected let statement, found: {} - {statement:?}",
                    sway::TabbedDisplayer(statement)
                ),
            };

            let mark_let_identifier_mutable = |id: &mut sway::LetIdentifier| {
                if id.name == variable.borrow().new_name {
                    id.is_mutable = true;
                }
            };

            match &mut let_statement.pattern {
                sway::LetPattern::Identifier(id) => mark_let_identifier_mutable(id),
                sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(mark_let_identifier_mutable),
            }
        }
    }

    // Check block for sub-blocks that don't contain shadowing variable declarations and flatten them
    for i in (0..block.statements.len()).rev() {
        let mut statements = None;

        {
            let sway::Statement::Expression(sway::Expression::Block(sub_block)) = &block.statements[i] else {
                continue;
            };

            let mut var_count = 0;

            for statement in sub_block.statements.iter() {
                let sway::Statement::Let(sway::Let { pattern, .. }) = statement else {
                    continue;
                };

                let mut check_let_identifier = |identifier: &sway::LetIdentifier| {
                    let parent = scope.borrow().get_parent();

                    if let Some(scope) = parent
                        && scope.borrow().get_variable_from_new_name(&identifier.name).is_some()
                    {
                        var_count += 1;
                    }
                };

                match pattern {
                    sway::LetPattern::Identifier(identifier) => {
                        check_let_identifier(identifier);
                    }

                    sway::LetPattern::Tuple(identifiers) => {
                        for identifier in identifiers.iter() {
                            check_let_identifier(identifier);
                        }
                    }
                }
            }

            if var_count == 0 {
                statements = Some(sub_block.statements.clone());
            }
        }

        if let Some(statements) = statements {
            block.statements.remove(i);

            for statement in statements.into_iter().rev() {
                block.statements.insert(i, statement);
            }
        }
    }

    // If the last statement is a block, flatten it
    if let Some(sway::Statement::Expression(sway::Expression::Block(inner_block))) = block.statements.last().cloned() {
        block.statements.pop();
        block.statements.extend(inner_block.statements);
    }

    Ok(())
}

#[inline]
pub fn translate_block_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    statements: &[solidity::Statement],
) -> Result<sway::Statement, Error> {
    let scope = Rc::new(RefCell::new(ir::Scope::new(None, None, Some(scope.clone()))));

    // Translate the block
    let translated_block = sway::Statement::from(sway::Expression::from(translate_block(
        project,
        module,
        scope.clone(),
        statements,
    )?));

    Ok(translated_block)
}
