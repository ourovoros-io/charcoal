use std::{cell::RefCell, rc::Rc};
use convert_case::Case;
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{expressions::translate_expression, translate_naming_convention, type_names::translate_type_name, TranslatedDefinition, TranslatedVariable, TranslationScope}};
use super::translate_statement;

#[allow(clippy::type_complexity)]
pub fn translate_try_catch_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    expr: &solidity::Expression,
    params_and_body:  &Option<(Vec<(solidity::Loc, Option<solidity::Parameter>)>, Box<solidity::Statement>)>,
    catch_clauses: &[solidity::CatchClause],
) -> Result<sway::Statement, Error> {
    let mut statements = vec![];
    match params_and_body.as_ref() {
        Some((params, body)) =>  {
            if !params.is_empty() {
                let let_statement = sway::Let {
                    pattern: if params.len() == 1 {
                        sway::LetPattern::Identifier(sway::LetIdentifier {
                            is_mutable: false,
                            name: translate_naming_convention(params[0].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake),
                        })
                    } else {
                        sway::LetPattern::Tuple(params.iter().map(|(_, p)| sway::LetIdentifier {
                            is_mutable: false,
                            name: translate_naming_convention(p.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake),
                        }).collect())
                    },
                    type_name: None,
                    value: translate_expression(project, translated_definition, scope, expr)?
                };
                let store_let_identifier = |id: &sway::LetIdentifier, type_name: &sway::TypeName| {
                    
                    let variable = Rc::new(RefCell::new(TranslatedVariable {
                        old_name: id.name.clone(),
                        new_name: id.name.clone(),
                        type_name: type_name.clone(),
                        ..Default::default()
                    }));
                    scope.borrow_mut().variables.push(variable);
                    
                    let variable = scope.borrow().get_variable_from_new_name(&id.name).unwrap();
                    variable.borrow_mut().statement_index = Some(statements.len());
                };
                match &let_statement.pattern {
                    sway::LetPattern::Identifier(id) => {
                        let type_name = translate_type_name(project, translated_definition, &params[0].1.as_ref().unwrap().ty, false, false);
                        store_let_identifier(id, &type_name);
                    },
                    sway::LetPattern::Tuple(ids) => {
                        let type_names = params.iter().map(|(_, p)| translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false, false)).collect::<Vec<_>>();
                        ids.iter().zip(type_names.iter()).for_each(|(id, type_name)| store_let_identifier(id, type_name));
                    }
                }
                statements.push(sway::Statement::from(let_statement));
            }
            
            match translate_statement(project, translated_definition, scope, body)? {
                sway::Statement::Expression(sway::Expression::Block(block)) => {
                    if block.statements.len() == 1 {
                        statements.extend(block.statements.clone());
                    } else {
                        statements.push(sway::Statement::from(sway::Expression::from(block.as_ref().clone())));
                    }
                }
                stmt => statements.push(stmt),
            };
            
        },
        None => statements.push(sway::Statement::from(translate_expression(project, translated_definition, scope, expr)?)),
    }
    
    for cc in catch_clauses {
        statements.push(sway::Statement::Commented(format!("unsupported: {cc}"), None));
    }
    
    Ok(sway::Statement::from(sway::Expression::from(sway::Block {
        statements,
        final_expr: None,
    })))
}
