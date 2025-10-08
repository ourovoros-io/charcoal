use crate::{error::Error, project::Project, sway};
use num_bigint::BigUint;
use num_traits::Num;
use solang_parser::pt as solidity;

#[inline]
pub fn translate_literal_expression(
    _project: &mut Project,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::Expression::BoolLiteral(_, value) => Ok(sway::Expression::from(sway::Literal::Bool(*value))),

        solidity::Expression::NumberLiteral(_, value, _, _) => Ok(sway::Expression::from(sway::Literal::DecInt(
            value.parse().unwrap(),
            None,
        ))),

        solidity::Expression::RationalNumberLiteral(_, _, _, _, _) => Ok(sway::Expression::create_todo(Some(format!(
            "rational number: {expression}"
        )))),

        solidity::Expression::HexNumberLiteral(_, value, _) | solidity::Expression::AddressLiteral(_, value) => {
            Ok(sway::Expression::from(sway::Literal::HexInt(
                BigUint::from_str_radix(value.trim_start_matches("0x"), 16).unwrap(),
                Some("u256".to_string()),
            )))
        }

        solidity::Expression::HexLiteral(hex_literals) => Ok(sway::Expression::from(sway::Literal::HexInt(
            BigUint::from_str_radix(
                hex_literals
                    .iter()
                    .map(|x| x.hex.clone())
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str(),
                16,
            )
            .unwrap(),
            None,
        ))),

        solidity::Expression::StringLiteral(value) => Ok(sway::Expression::create_string_literal(
            &value.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join(""),
        )),

        _ => panic!("Expected literal expression, found {expression} - {expression:#?}"),
    }
}
