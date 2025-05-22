use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_assignment_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // use solang_parser::helpers::CodeLocation;
    // println!(
    //     "Translating assignment expression: {lhs} {operator} {rhs}; from {}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &lhs.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     },
    // );

    let rhs = match operator {
        "=" => {
            // println!("taking translate_pre_or_post_operator_value_expression path...");
            translate_pre_or_post_operator_value_expression(project, module, scope, rhs)?
        }
        _ => {
            // println!("taking translate_expression path...");
            translate_expression(project, module, scope, rhs)?
        }
    };

    let rhs_type_name = module.borrow_mut().get_expression_type(scope, &rhs)?;

    let (variable, expression) = translate_variable_access_expression(project, module, scope, lhs)?;

    // HACK: struct field lookup
    if variable.is_none() {
        if let sway::Expression::MemberAccess(member_access) = &expression {
            if let sway::TypeName::Identifier { name, .. } = module
                .borrow_mut()
                .get_expression_type(scope, &member_access.expression)?
            {
                if let Some(struct_definition) = module
                    .borrow()
                    .structs
                    .iter()
                    .find(|s| s.borrow().name == name)
                {
                    if struct_definition
                        .borrow()
                        .fields
                        .iter()
                        .any(|f| f.name == member_access.member)
                    {
                        return Ok(expression);
                    }
                }
            }
        }

        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }

    let variable = variable.unwrap();

    create_assignment_expression(
        project,
        module,
        scope,
        operator,
        &expression,
        &variable,
        &rhs,
        &rhs_type_name,
    )
}
