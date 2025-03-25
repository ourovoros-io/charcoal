use crate::{
    errors::Error,
    project::Project,
    sway,
    translate::{
        function_call::utils::coerce_expression, translate_expression, TranslatedDefinition,
        TranslationScope,
    },
};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_return_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    let Some(expression) = expression else {
        return Ok(sway::Statement::from(sway::Expression::Return(None)));
    };

    let current_function_name = translated_definition.current_functions.last().unwrap();
    let function = scope.borrow().find_function(|f| f.borrow().new_name == *current_function_name).unwrap();

    let sway::TypeName::Function { return_type, .. } = function.borrow().type_name.clone() else {
        panic!("Invalid function type name: {:#?}", function.borrow().type_name)
    };
    
    let return_type = return_type.unwrap();

    let mut expression = translate_expression(project, translated_definition, scope.clone(), expression)?;
    let expression_type = translated_definition.get_expression_type(scope.clone(), &expression)?;
    
    expression = coerce_expression(&expression, &expression_type, &return_type).unwrap();

    Ok(sway::Statement::from(sway::Expression::Return(Some(Box::new(expression)))))
}
