use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_for_statement(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    initialization: &Option<Box<solidity::Statement>>,
    condition: &Option<Box<solidity::Expression>>,
    update: &Option<Box<solidity::Expression>>,
    body: &Option<Box<solidity::Statement>>,
) -> Result<sway::Statement, Error> {
    // {
    //     initialization;
    //     while condition {
    //         body;
    //         update;
    //     }
    // }

    // Create a scope for the block that will contain the for loop logic
    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(scope.clone()),
        ..Default::default()
    }));

    // Collect statements for the for loop logic block
    let mut statements = vec![];

    // Translate the initialization statement (if any) and add it to the for loop logic block's statements
    if let Some(initialization) = initialization.as_ref() {
        let statement_index = statements.len();
        let mut statement =
            translate_statement(project, module.clone(), scope.clone(), initialization.as_ref())?;

        // Store the statement index of variable declaration statements in their scope entries
        if let sway::Statement::Let(sway::Let { pattern, .. }) = &mut statement {
            let store_let_identifier_statement_index = |id: &mut sway::LetIdentifier| {
                let Some(variable) = scope.borrow().get_variable_from_new_name(&id.name) else {
                    panic!("error: Variable not found in scope: \"{}\"", id.name);
                };

                variable.borrow_mut().statement_index = Some(statement_index);
            };

            match pattern {
                sway::LetPattern::Identifier(id) => store_let_identifier_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids
                    .iter_mut()
                    .for_each(store_let_identifier_statement_index),
            }
        }

        statements.push(statement);
    }

    // Translate the condition of the for loop ahead of time (if any)
    let condition = if let Some(condition) = condition.as_ref() {
        translate_expression(project, module.clone(), scope.clone(), condition.as_ref())?
    } else {
        sway::Expression::from(sway::Literal::Bool(true))
    };

    // Translate the body of the for loop ahead of time (if any)
    let mut body = match body.as_ref() {
        None => sway::Block::default(),
        Some(body) => match translate_statement(project, module.clone(), scope.clone(), body.as_ref())? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            },
        },
    };

    // Translate the update statement of the for loop (if any) and add it to the end of the for loop's body block
    if let Some(update) = update.as_ref() {
        body.statements
            .push(sway::Statement::from(match update.as_ref() {
                // Check for standalone pre/post decrement statements
                solidity::Expression::PreDecrement(loc, x)
                | solidity::Expression::PostDecrement(loc, x) => translate_assignment_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    "-=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
                )?,

                // Check for standalone pre/post increment statements
                solidity::Expression::PreIncrement(loc, x)
                | solidity::Expression::PostIncrement(loc, x) => translate_assignment_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    "+=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
                )?,

                _ => translate_expression(project, module.clone(), scope.clone(), update.as_ref())?,
            }));
    }

    // Create the while loop for the for loop logic ahead of time
    let while_statement =
        sway::Statement::from(sway::Expression::from(sway::While { condition, body }));

    // If we don't have any initialization statements, just return the generated while loop
    if statements.is_empty() {
        return Ok(while_statement);
    }

    // Add the generated while loop to the for loop logic block's statements
    statements.push(while_statement);

    // Create the for loop logic block using the collected statements
    let mut block = sway::Block {
        statements,
        final_expr: None,
    };

    // Finalize the for loop logic block
    finalize_block_translation(project, scope.clone(), &mut block)?;

    Ok(sway::Statement::from(sway::Expression::from(block)))
}
