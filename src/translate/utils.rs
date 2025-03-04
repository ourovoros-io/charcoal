use std::{cell::RefCell, rc::Rc};

use crate::sway;

use super::{TranslatedDefinition, TranslationScope};

pub fn align_types(
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    base: sway::Expression,
    input: sway::Expression,
) -> Result<sway::Expression, crate::errors::Error> {
    // Get the base and input types
    let base_type = translated_definition.get_expression_type(scope.clone(), &base)?;
    let input_type = translated_definition.get_expression_type(scope.clone(), &input)?;

    // If the types are the same, return the input
    if base_type == input_type {
        return Ok(input);
    }

    // If the types are incompatible, return an error
    if !base_type.is_compatible_with(&input_type) {
        return Err(crate::errors::Error::Wrapped(
            format!(
                "Incompatible types.. Cannot convert type {} to type {}",
                input_type.to_string(),
                base_type.to_string()
            )
            .into(),
        ));
    }

    match &base_type {
        sway::TypeName::Undefined => todo!(),
        sway::TypeName::Identifier {
            name: base_name, ..
        } => {
            let sway::TypeName::Identifier {
                name: input_name, ..
            } = &input_type
            else {
                panic!("Types dont match: base: {base_type:#?} and input: {input_type:#?}")
            };
            // If the base type and the input type are both numeric types
            if base_type.is_uint() && input_type.is_uint()
                || base_type.is_int() && input_type.is_int()
            {
                return Ok(align_numeric(base_name, input_name, input));
            }
        }
        sway::TypeName::Array { .. } => return Ok(input.clone()),
        sway::TypeName::Tuple {
            type_names: base_type_names,
        } => {
            let sway::TypeName::Tuple {
                type_names: input_type_names,
            } = &input_type
            else {
                panic!("Types dont match: base: {base_type:#?} and input: {input_type:#?}")
            };
            let mut aligned = Vec::new();
            for (base_type_name, input_type_name) in base_type_names
                .iter()
                .zip(input_type_names.iter())
                .collect::<Vec<(&sway::TypeName, &sway::TypeName)>>()
            {
                match base_type_name {
                    sway::TypeName::Undefined => todo!(),
                    sway::TypeName::Identifier {
                        name: base_name, ..
                    } => {
                        let sway::TypeName::Identifier {
                            name: input_name, ..
                        } = input_type_name
                        else {
                            panic!(
                                "Types dont match: base: {base_type:#?} and input: {input_type:#?}"
                            )
                        };
                        // If the base type and the input type are both numeric types
                        if base_type.is_uint() && input_type.is_uint()
                            || base_type.is_int() && input_type.is_int()
                        {
                            aligned.push(align_numeric(base_name, input_name, input));
                        }
                        return Ok(sway::Expression::Tuple(aligned));
                    }
                    sway::TypeName::Array { .. } => return Ok(input.clone()),
                    sway::TypeName::Tuple { type_names } => todo!(),
                    sway::TypeName::StringSlice => return Ok(input.clone()),
                    sway::TypeName::StringArray { .. } => return Ok(input.clone()),
                    sway::TypeName::Function { .. } => return Ok(input.clone()),
                }
            }
        }
        sway::TypeName::StringSlice => return Ok(input.clone()),
        sway::TypeName::StringArray { .. } => return Ok(input.clone()),
        sway::TypeName::Function { .. } => return Ok(input.clone()),
    }

    Ok(input.clone())
}

/// Aligns converts numeric types from input to base types
fn align_numeric(base_name: &str, input_name: &str, input: sway::Expression) -> sway::Expression {
    // Get the first character of the name of the base
    let integer_index = base_name.chars().next().unwrap();

    // Get the bits for base and input
    let base_bits: usize = base_name.trim_start_matches(integer_index).parse().unwrap();
    let input_bits: usize = input_name
        .trim_start_matches(integer_index)
        .parse()
        .unwrap();

    // Create conversions
    if base_bits > input_bits {
        sway::Expression::create_function_calls(
            None,
            &[(
                format!("as_{integer_index}{base_bits}").as_str(),
                Some((None, vec![])),
            )],
        )
    } else {
        sway::Expression::create_function_calls(
            None,
            &[
                (
                    format!("{integer_index}{base_bits}::try_from").as_str(),
                    Some((None, vec![input])),
                ),
                ("unwrap", Some((None, vec![]))),
            ],
        )
    }
}

pub fn match_bits(bits: usize, signed: bool) -> Option<usize> {
    let (name, ty, ident) = if signed {
        ("signed", "int", "I")
    } else {
        ("unsigned", "uint", "U")
    };
    match bits {
        0..=8 => {
            if bits != 8 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}8`..."
                );
            }
            Some(8)
        }
        9..=16 => {
            if bits != 16 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}16`..."
                );
            }
            Some(16)
        }
        17..=32 => {
            if bits != 32 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}32`..."
                );
            }
            Some(32)
        }
        33..=64 => {
            if bits != 64 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}64`..."
                );
            }
            Some(64)
        }
        65..=128 => {
            if bits != 128 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}128`..."
                );
            }
            Some(128)
        }
        129..=256 => {
            if bits != 256 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}256`..."
                );
            }
            Some(256)
        }
        _ => None,
    }
}
