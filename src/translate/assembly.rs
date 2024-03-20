use super::{
    create_value_expression, finalize_block_translation, TranslatedDefinition, TranslatedVariable,
    TranslationScope,
};
use crate::{errors::Error, project::Project, sway};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::{Num, Zero};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_assembly_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    _dialect: &Option<solidity::StringLiteral>,
    _flags: &Option<Vec<solidity::StringLiteral>>,
    yul_block: &solidity::YulBlock,
) -> Result<sway::Statement, Error> {
    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(scope.clone()),
        ..Default::default()
    }));

    // Translate the block
    let translated_block = sway::Statement::from(sway::Expression::from(
        translate_yul_block(project, translated_definition, scope.clone(), yul_block)?
    ));

    Ok(translated_block)
}

#[inline]
pub fn translate_yul_block(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    yul_block: &solidity::YulBlock,
) -> Result<sway::Block, Error> {
    let mut block = sway::Block::default();

    // Translate each of the statements in the block
    for statement in yul_block.statements.iter() {
        // Translate the statement
        let sway_statement = translate_yul_statement(project, translated_definition, scope.clone(), statement)?;

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

                let scope = scope.borrow();

                let scope_entry = scope.variables.iter().rev().find(|v| v.borrow().new_name == id.name).unwrap();
                let mut scope_entry = scope_entry.borrow_mut();

                scope_entry.statement_index = Some(statement_index);
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

#[inline]
pub fn translate_yul_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    yul_statement: &solidity::YulStatement,
) -> Result<sway::Statement, Error> {
    match yul_statement {
        solidity::YulStatement::Assign(_, identifiers, value) => translate_yul_assign_statement(project, translated_definition, scope.clone(), identifiers, value),
        solidity::YulStatement::VariableDeclaration(_, identifiers, value) => translate_yul_variable_declaration_statement(project, translated_definition, scope.clone(), identifiers, value),
        solidity::YulStatement::If(_, condition, then_block) => translate_yul_if_statement(project, translated_definition, scope.clone(), condition, then_block),
        solidity::YulStatement::For(yul_for) => translate_yul_for_statement(project, translated_definition, scope.clone(), yul_for),
        solidity::YulStatement::Switch(yul_switch) => translate_yul_switch_statement(project, translated_definition, scope.clone(), yul_switch),
        solidity::YulStatement::Leave(_) => todo!("yul leave statement: {yul_statement} - {yul_statement:#?}"),
        solidity::YulStatement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
        solidity::YulStatement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
        solidity::YulStatement::Block(block) => Ok(sway::Statement::from(sway::Expression::from(translate_yul_block(project, translated_definition, scope.clone(), block)?))),
        solidity::YulStatement::FunctionDefinition(_) => todo!("yul function definition statement: {yul_statement} - {yul_statement:#?}"),
        solidity::YulStatement::FunctionCall(yul_function_call) => translate_yul_function_call_statement(project, translated_definition, scope.clone(), yul_function_call),
        solidity::YulStatement::Error(_) => todo!("yul error statement: {yul_statement} - {yul_statement:#?}"),
    }
}

#[inline]
pub fn translate_yul_assign_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    identifiers: &[solidity::YulExpression],
    value: &solidity::YulExpression,
) -> Result<sway::Statement, Error> {
    let translated_identifiers = identifiers.iter()
        .map(|i| translate_yul_expression(project, translated_definition, scope.clone(), i))
        .collect::<Result<Vec<_>, _>>()?;

    for (i, identifier) in translated_identifiers.iter().enumerate() {
        let sway::Expression::Identifier(name) = identifier else { continue };

        let Some(variable) = scope.borrow().get_variable_from_new_name(name) else {
            panic!(
                "{}error: Variable not found in scope: \"{name}\"",
                match project.loc_to_line_and_column(&translated_definition.path, &identifiers[i].loc()) {
                    Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                    None => format!("{} - ", translated_definition.path.to_string_lossy()),
                }
            );
        };

        variable.borrow_mut().mutation_count += 1;
    }

    let value = translate_yul_expression(project, translated_definition, scope.clone(), value)?;
    
    Ok(sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
        operator: "=".into(),
        lhs: if translated_identifiers.len() == 1 {
            translated_identifiers[0].clone()
        } else {
            sway::Expression::Tuple(translated_identifiers)
        },
        rhs: value,
    })))
}

#[inline]
pub fn translate_yul_variable_declaration_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    identifiers: &[solidity::YulTypedIdentifier],
    value: &Option<solidity::YulExpression>,
) -> Result<sway::Statement, Error> {
    // Collect variable translations for the scope
    let mut variables = vec![];

    for p in identifiers.iter() {
        variables.push(Rc::new(RefCell::new(TranslatedVariable {
            old_name: p.id.name.clone(),
            new_name: crate::translate_naming_convention(p.id.name.as_str(), Case::Snake),
            type_name: sway::TypeName::Identifier {
                name: "u256".into(),
                generic_parameters: None,
            },
            ..Default::default()
        })));
    }

    scope.borrow_mut().variables.extend(variables.clone());

    // Create the variable declaration statement
    Ok(sway::Statement::from(sway::Let {
        pattern: if variables.len() == 1 {
            sway::LetPattern::Identifier(sway::LetIdentifier {
                is_mutable: false,
                name: variables[0].borrow().new_name.clone(),
            })
        } else {
            sway::LetPattern::Tuple(
                variables.iter()
                    .map(|p| sway::LetIdentifier {
                        is_mutable: false,
                        name: p.borrow().new_name.clone(),
                    })
                    .collect()
            )
        },

        type_name: None,
        
        value: if let Some(value) = value.as_ref() {
            translate_yul_expression(project, translated_definition, scope.clone(), value)?
        } else {
            create_value_expression(
                translated_definition,
                scope,
                &sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                },
                None,
            )
        },
    }))
}

#[inline]
pub fn translate_yul_if_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    condition: &solidity::YulExpression,
    then_block: &solidity::YulBlock,
) -> Result<sway::Statement, Error> {
    let condition = translate_yul_expression(project, translated_definition, scope.clone(), condition)?;
    let then_body = translate_yul_block(project, translated_definition, scope.clone(), then_block)?;

    Ok(sway::Statement::from(sway::Expression::from(sway::If {
        condition: Some(condition),
        then_body,
        else_if: None,
    })))
}

#[inline]
pub fn translate_yul_for_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    yul_for: &solidity::YulFor,
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

    // Translate the initialization statements and add them to the for loop logic block's statements
    for statement in yul_for.init_block.statements.iter() {
        let statement_index = statements.len();
        let mut statement = translate_yul_statement(project, translated_definition, scope.clone(), statement)?;

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
                sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(store_let_identifier_statement_index),
            }
        }

        statements.push(statement);
    }

    // Translate the condition of the for loop ahead of time
    let condition = translate_yul_expression(project, translated_definition, scope.clone(), &yul_for.condition)?;

    // Translate the body of the for loop ahead of time
    let mut body = translate_yul_block(project, translated_definition, scope.clone(), &yul_for.execution_block)?;

    // Translate the statements of the post block of the for loop and add them to the end of for loop's body block
    for statement in yul_for.post_block.statements.iter() {
        body.statements.push(
            translate_yul_statement(project, translated_definition, scope.clone(), statement)?
        );
    }

    // Create the while loop for the for loop logic ahead of time
    let while_statement = sway::Statement::from(sway::Expression::from(sway::While {
        condition,
        body,
    }));

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

#[inline]
pub fn translate_yul_switch_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    yul_switch: &solidity::YulSwitch,
) -> Result<sway::Statement, Error> {
    let expression = translate_yul_expression(project, translated_definition, scope.clone(), &yul_switch.condition)?;
    let mut branches = vec![];

    for case in yul_switch.cases.iter() {
        match case {
            solidity::YulSwitchOptions::Case(_, pattern, body) => {
                let pattern = translate_yul_expression(project, translated_definition, scope.clone(), pattern)?;
                let value = sway::Expression::from(translate_yul_block(project, translated_definition, scope.clone(), body)?);
                branches.push(sway::MatchBranch { pattern, value });
            }

            solidity::YulSwitchOptions::Default(_, body) => {
                let pattern = sway::Expression::Identifier("_".into());
                let value = sway::Expression::from(translate_yul_block(project, translated_definition, scope.clone(), body)?);
                branches.push(sway::MatchBranch { pattern, value });
            }
        }
    }

    Ok(sway::Statement::from(sway::Expression::from(sway::Match { expression, branches })))
}

#[inline]
pub fn translate_yul_function_call_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    yul_function_call: &solidity::YulFunctionCall,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(
        translate_yul_function_call_expression(project, translated_definition, scope.clone(), yul_function_call)?
    ))
}

pub fn translate_yul_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::YulExpression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::YulExpression::BoolLiteral(_, value, _) => Ok(sway::Expression::from(sway::Literal::Bool(*value))),
        solidity::YulExpression::NumberLiteral(_, value, _, _) => Ok(sway::Expression::from(sway::Literal::DecInt(value.parse().unwrap()))),
        solidity::YulExpression::HexNumberLiteral(_, value, _) => Ok(sway::Expression::from(sway::Literal::HexInt(BigUint::from_str_radix(value.trim_start_matches("0x"), 16).unwrap()))),
        solidity::YulExpression::HexStringLiteral(_, _) => todo!("yul hex string literal expression: {expression:#?}"),
        solidity::YulExpression::StringLiteral(string_literal, _) => Ok(sway::Expression::from(sway::Literal::String(string_literal.string.clone()))),
        solidity::YulExpression::Variable(solidity::Identifier { name, .. }) => translate_yul_variable_expression(project, translated_definition, scope.clone(), expression, name.as_str()),
        solidity::YulExpression::FunctionCall(function_call) => translate_yul_function_call_expression(project, translated_definition, scope.clone(), function_call),
        solidity::YulExpression::SuffixAccess(_, _, _) => Ok(sway::Expression::create_todo(Some(expression.to_string()))),
    }
}

#[inline]
pub fn translate_yul_variable_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::YulExpression,
    name: &str,
) -> Result<sway::Expression, Error> {
    let Some(variable) = scope.borrow().get_variable_from_old_name(name) else {
        panic!(
            "{}error: Variable not found in scope: \"{name}\"",
            match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                None => format!("{} - ", translated_definition.path.to_string_lossy()),
            }
        );
    };

    let variable = variable.borrow();

    if variable.is_storage {
        Ok(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::from(sway::MemberAccess {
                expression: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::Identifier("storage".into()),
                    member: variable.new_name.clone(),
                }),
                member: "read".into(),
            }),
            generic_parameters: None,
            parameters: vec![],
        }))
    } else {
        Ok(sway::Expression::Identifier(variable.new_name.clone()))
    }
}

#[inline]
pub fn translate_yul_function_call_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    function_call: &solidity::YulFunctionCall,
) -> Result<sway::Expression, Error> {
    let parameters = function_call.arguments.iter()
        .map(|a| translate_yul_expression(project, translated_definition, scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    match function_call.id.name.as_str() {
        "stop" => {
            // TODO: stop() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "add" => {
            // add(a, b) => a + b

            if parameters.len() != 2 {
                panic!("Invalid yul add function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "+".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "mul" => {
            // mul(a, b) => a * b

            if parameters.len() != 2 {
                panic!("Invalid yul mul function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "*".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "sub" => {
            // sub(a, b) => a - b

            if parameters.len() != 2 {
                panic!("Invalid yul sub function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "-".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "div" => {
            // div(a, b) => a / b
            
            if parameters.len() != 2 {
                panic!("Invalid yul div function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "/".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "sdiv" => {
            // sdiv(a, b) => a / b

            if parameters.len() != 2 {
                panic!("Invalid yul div function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "/".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "mod" => {
            // mod(a, b) => a % b

            if parameters.len() != 2 {
                panic!("Invalid yul mod function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "smod" => {
            // smod(a, b) => a % b

            if parameters.len() != 2 {
                panic!("Invalid yul smod function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "exp" => {
            // exp(a, b) => {
            //     use std::math::Power;
            //     a.pow(b);
            // }

            if parameters.len() != 2 {
                panic!("Invalid yul exp function call, expected 2 parameters, found {}", parameters.len());
            }

            // Ensure std::math::Power is imported for the pow function
            translated_definition.ensure_use_declared("std::math::Power");

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: parameters[0].clone(),
                    member: "pow".into(),
                }),
                generic_parameters: None,
                parameters: vec![
                    parameters[1].clone(),
                ],
            }))
        }

        "not" => {
            // not(a) => !a

            if parameters.len() != 1 {
                panic!("Invalid yul not function call, expected 1 parameter, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::UnaryExpression {
                operator: "!".into(),
                expression: parameters[0].clone(),
            }))
        }

        "lt" => {
            // lt(a, b) => a < b

            if parameters.len() != 2 {
                panic!("Invalid yul lt function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "<".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "gt" => {
            // gt(a, b) => a > b

            if parameters.len() != 2 {
                panic!("Invalid yul gt function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: ">".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "slt" => {
            // slt(a, b) => a < b

            if parameters.len() != 2 {
                panic!("Invalid yul slt function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "<".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "sgt" => {
            // sgt(a, b) => a > b
            
            if parameters.len() != 2 {
                panic!("Invalid yul sgt function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: ">".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "eq" => {
            // eq(a, b) => a == b
            
            if parameters.len() != 2 {
                panic!("Invalid yul eq function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "==".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "iszero" => {
            // eq(a) => a == 0
            
            if parameters.len() != 1 {
                panic!("Invalid yul iszero function call, expected 1 parameters, found {}", parameters.len());
            }

            let type_name = translated_definition.get_expression_type(scope.clone(), &parameters[0])?;

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "==".into(),
                lhs: parameters[0].clone(),
                rhs: create_value_expression(translated_definition, scope.clone(), &type_name, None),
            }))
        }

        "and" => {
            // and(a, b) => a & b
            
            if parameters.len() != 2 {
                panic!("Invalid yul and function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "&".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "or" => {
            // or(a, b) => a | b
            
            if parameters.len() != 2 {
                panic!("Invalid yul or function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "|".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "xor" => {
            // xor(a, b) => a ^ b
            
            if parameters.len() != 2 {
                panic!("Invalid yul xor function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "^".into(),
                lhs: parameters[0].clone(),
                rhs: parameters[1].clone(),
            }))
        }

        "byte" => {
            // TODO: byte(i, x) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "shl" => {
            // shl(shift, value) => value << shift
            
            if parameters.len() != 2 {
                panic!("Invalid yul shl function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "<<".into(),
                lhs: parameters[1].clone(),
                rhs: parameters[0].clone(),
            }))
        }

        "shr" => {
            // shr(shift, value) => value >> shift
            
            if parameters.len() != 2 {
                panic!("Invalid yul shr function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: ">>".into(),
                lhs: parameters[1].clone(),
                rhs: parameters[0].clone(),
            }))
        }

        "sar" => {
            // sar(shift, value) => value >> shift
            
            if parameters.len() != 2 {
                panic!("Invalid yul sar function call, expected 2 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: ">>".into(),
                lhs: parameters[1].clone(),
                rhs: parameters[0].clone(),
            }))
        }

        "addmod" => {
            // addmod(a, b, c) => (a + b) % c

            if parameters.len() != 3 {
                panic!("Invalid yul addmod function call, expected 3 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![
                    sway::Expression::from(sway::BinaryExpression {
                        operator: "+".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }),
                ]),
                rhs: parameters[3].clone(),
            }))
        }

        "mulmod" => {
            // mulmod(a, b, c) => (a * b) % c

            if parameters.len() != 3 {
                panic!("Invalid yul addmod function call, expected 3 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![
                    sway::Expression::from(sway::BinaryExpression {
                        operator: "*".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }),
                ]),
                rhs: parameters[2].clone(),
            }))
        }

        "signextend" => {
            // TODO: signextend(b, x) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "sha3" => {
            // TODO: sha3(offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "keccak256" => {
            // TODO: keccak256(offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "pc" => {
            // TODO: pc() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "pop" => {
            // TODO: pop(x) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "mload" => {
            // TODO: mload(offset) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "mstore" => {
            // TODO: mstore(offset, value) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "mstore8" => {
            // TODO: mstore8(offset, value) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "sload" => {
            // TODO: sload(key) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "sstore" => {
            // TODO: sstore(key, value) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "tload" => {
            // TODO: tload(p) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "tstore" => {
            // TODO: tstore(p, v) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "msize" => {
            // TODO: msize() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "gas" => {
            // TODO: gas() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "address" => {
            // address() => Identity::from(ContractId::this())

            if parameters.len() != 0 {
                panic!("Invalid yul address function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("Identity::ContractId".into()),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("ContractId::this".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }),
                ],
            }))
        }

        "balance" => {
            // TODO: balance(addr) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "selfbalance" => {
            // selfbalance() => std::context::this_balance(AssetId::default()).as_u256()

            if parameters.len() != 0 {
                panic!("Invalid yul balance function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::context::this_balance".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("AssetId::default".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                        ],
                    }),
                    member: "as_u256".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        }

        "caller" => {
            // caller() => msg_sender().unwrap()

            if parameters.len() != 0 {
                panic!("Invalid yul caller function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("msg_sender".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }),
                    member: "unwrap".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        }

        "callvalue" => {
            // callvalue() => std::context::msg_amount()
            
            if parameters.len() != 0 {
                panic!("Invalid yul callvalue function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("std::context::msg_amount".into()),
                generic_parameters: None,
                parameters: vec![],
            }))
        }

        "calldataload" => {
            // TODO: calldataload(i) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "calldatasize" => {
            // calldatasize() => std::inputs::input_message_data_length(0)
            
            if parameters.len() != 0 {
                panic!("Invalid yul calldatasize function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("std::inputs::input_message_data_length".into()),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::Literal::DecInt(BigUint::zero())),
                ],
            }))
        }

        "calldatacopy" => {
            // TODO: calldatacopy(dest_offset, offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "codesize" => {
            // TODO: codesize() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "codecopy" => {
            // TODO: codecopy(dest_offset, offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "extcodesize" => {
            // TODO: extcodesize(addr) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "extcodecopy" => {
            // TODO: extcodecopy(addr, dest_offset, offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "returndatasize" => {
            // returndatasize() => std::registers::return_length()

            if parameters.len() != 0 {
                panic!("Invalid yul returndatasize function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("std::registers::return_length".to_string()),
                generic_parameters: None,
                parameters: vec![],
            }))
        }

        "returndatacopy" => {
            // TODO: returndatacopy(dest_offset, offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "mcopy" => {
            // TODO: mcopy(t, f, s) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "extcodehash" => {
            // TODO: extcodehash(addr) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "create" => {
            // TODO: create(value, offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "create2" => {
            // TODO: create2(value, offset, length, salt) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "call" => {
            // TODO: call(gas, addr, value, args_offset, args_length, ret_offset, ret_length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "callcode" => {
            // TODO: callcode(gas, addr, value, args_offset, args_length, ret_offset, ret_length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "delegatecall" => {
            // TODO: delegatecall(gas, addr, args_offset, args_length, ret_offset, ret_length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "staticcall" => {
            // TODO: staticcall(gas, addr, args_offset, args_length, ret_offset, ret_length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "return" => {
            // TODO: return(offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "revert" => {
            // TODO: revert(offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "selfdestruct" => {
            // TODO: selfdestruct(addr) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }
        
        "invalid" => {
            // TODO: invalid() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }
        
        "log0" => {
            // TODO: log0(offset, length) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "log1" => {
            // TODO: log1(offset, length, topic0) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "log2" => {
            // TODO: log2(offset, length, topic0, topic1) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "log3" => {
            // TODO: log3(offset, length, topic0, topic1, topic2) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "log4" => {
            // TODO: log4(offset, length, topic0, topic1, topic2, topic3) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "chainid" => {
            // chainid() => asm(r1) {
            //    gm r1 i4;
            //    r1: u64
            // }

            if parameters.len() != 0 {
                panic!("Invalid yul chainid function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::AsmBlock {
                registers: vec![
                    sway::AsmRegister {
                        name: "r1".into(),
                        value: None,
                    },
                ],
                instructions: vec![
                    sway::AsmInstruction {
                        op_code: "gm".into(),
                        args: vec![
                            "r1".into(),
                            "i4".into(),
                        ],
                    }
                ],
                final_expression: Some(sway::AsmFinalExpression {
                    register: "r1".into(),
                    type_name: Some(sway::TypeName::Identifier {
                        name: "u64".into(),
                        generic_parameters: None,
                    }),
                }),
            }))
        }

        "basefee" => {
            // TODO: basefee() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }
        
        "blobbasefee" => {
            // TODO: blobbasefee() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }
        
        "origin" => {
            // TODO: origin() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "gasprice" => {
            // gasprice() => std::tx::tx_gas_price().unwrap_or(0)
            
            if parameters.len() != 0 {
                panic!("Invalid yul gasprice function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::tx::tx_gas_price".to_string()),
                        generic_parameters: None,
                        parameters: vec![],
                    }),
                    member: "unwrap_or".into(),
                }),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::Literal::DecInt(BigUint::zero())),
                ],
            }))
        }

        "blockhash" => {
            // TODO: blockhash(block_number) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "blobhash" => {
            // TODO: blobhash(i) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "coinbase" => {
            // coinbase() => {
            //     let ptr = std::alloc::alloc(__size_of::<b256>());
            //     asm(r1: ptr) {
            //         cb r1;
            //     }
            //     Identity::from(ContractId::from(ptr.read::<b256>()))
            // }

            if parameters.len() != 0 {
                panic!("Invalid yul coinbase function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::Block {
                statements: vec![
                    // let ptr = std::alloc::alloc(__size_of::<b256>());
                    sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                            is_mutable: false,
                            name: "ptr".into(),
                        }),
                        type_name: None,
                        value: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::alloc::alloc".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("__size_of".into()),
                                    generic_parameters: Some(sway::GenericParameterList {
                                        entries: vec![
                                            sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "b256".into(),
                                                    generic_parameters: None,
                                                },
                                                implements: None,
                                            },
                                        ],
                                    }),
                                    parameters: vec![],
                                }),
                            ],
                        }),
                    }),
                    
                    // asm(r1: ptr) {
                    //     cb r1;
                    // }
                    sway::Statement::from(sway::Expression::from(sway::AsmBlock {
                        registers: vec![
                            sway::AsmRegister {
                                name: "r1".into(),
                                value: Some(sway::Expression::Identifier("ptr".into())),
                            },
                        ],
                        instructions: vec![
                            sway::AsmInstruction {
                                op_code: "cb".into(),
                                args: vec![
                                    "r1".into(),
                                ],
                            },
                        ],
                        final_expression: None,
                    })),
                ],
                
                // Identity::from(ContractId::from(ptr.read::<b256>()))
                final_expr: Some(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("Identity::from".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("ContractId::from".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::Identifier("ptr".into()),
                                        member: "read".into(),
                                    }),
                                    generic_parameters: Some(sway::GenericParameterList {
                                        entries: vec![
                                            sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "b256".into(),
                                                    generic_parameters: None,
                                                },
                                                implements: None,
                                            },
                                        ]
                                    }),
                                    parameters: vec![],
                                }),
                            ],
                        })
                    ],
                })),
            }))
        }

        "timestamp" => {
            // timestamp() => std::block::timestamp().as_u256()
            
            if parameters.len() != 0 {
                panic!("Invalid yul timestamp function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::block::timestamp".into()),
                        generic_parameters: None,
                        parameters: vec![],
                    }),
                    member: "as_u256".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        }

        "number" => {
            // number() => std::block::height()
            
            if parameters.len() != 0 {
                panic!("Invalid yul number function call, expected 0 parameters, found {}", parameters.len());
            }

            Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("std::block::height".into()),
                generic_parameters: None,
                parameters: vec![],
            }))
        }

        "difficulty" => {
            // TODO: difficulty() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "prevrandao" => {
            // TODO: prevrandao() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "gaslimit" => {
            // TODO: gaslimit() => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "datasize" => {
            // TODO: datasize(x) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "dataoffset" => {
            // TODO: dataoffset(x) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "datacopy" => {
            // TODO: datacopy(t, f, l) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "setimmutable" => {
            // TODO: setimmutable(offset, "name", value) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "loadimmutable" => {
            // TODO: loadimmutable("name") => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "linkersymbol" => {
            // TODO: linkersymbol("library_id") => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        "memoryguard" => {
            // TODO: memoryguard(size) => ???
            Ok(sway::Expression::create_todo(Some(function_call.to_string())))
        }

        // TODO: verbatim_<n>i_<m>o("<data>", ...), where
        // n is a decimal between 0 and 99 that specifies the number of input stack slots / variables
        // m is a decimal between 0 and 99 that specifies the number of output stack slots / variables
        // data is a string literal that contains the sequence of bytes

        name => todo!("look up yul function in scope: \"{name}\"")
    }
}
