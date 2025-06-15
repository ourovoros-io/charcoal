use crate::{ir, project::Project, sway};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub enum Symbol {
    TypeDefinition(String),
    Enum(String),
    Event(String),
    Error(String),
    Struct(String),
}

#[derive(Clone, Debug)]
pub enum SymbolData {
    TypeDefinition(sway::TypeDefinition),
    Enum(ir::Enum),
    Event {
        type_name: sway::TypeName,
        variant: sway::EnumVariant,
    },
    Error {
        type_name: sway::TypeName,
        variant: sway::EnumVariant,
    },
    Struct(Rc<RefCell<sway::Struct>>),
}

pub fn resolve_symbol(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    symbol: Symbol,
) -> Option<SymbolData> {
    match &symbol {
        Symbol::TypeDefinition(name) => {
            if let Some(type_definition) = module
                .borrow()
                .type_definitions
                .iter()
                .find(|t| t.signature.to_string() == *name)
            {
                if let Some(implementation) = type_definition.implementation.as_ref() {
                    return Some(SymbolData::TypeDefinition(implementation.clone()));
                }
            }
        }

        Symbol::Enum(name) => {
            if let Some(enum_definition) = module
                .borrow()
                .enums
                .iter()
                .find(|e| e.signature.to_string() == *name)
            {
                return Some(SymbolData::Enum(
                    enum_definition.implementation.clone().unwrap(),
                ));
            }
        }

        Symbol::Event(name) => {
            if let Some(event_enum) = module
                .borrow()
                .events_enums
                .iter()
                .find(|e| e.0.borrow().variants.iter().any(|v| v.name == *name))
            {
                let variant = event_enum
                    .0
                    .borrow()
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .cloned()
                    .unwrap();

                return Some(SymbolData::Event {
                    type_name: sway::TypeName::Identifier {
                        name: event_enum.0.borrow().name.clone(),
                        generic_parameters: None,
                    },
                    variant,
                });
            }
        }

        Symbol::Error(name) => {
            if let Some(error_enum) = module
                .borrow()
                .errors_enums
                .iter()
                .find(|e| e.0.borrow().variants.iter().any(|v| v.name == *name))
            {
                let variant = error_enum
                    .0
                    .borrow()
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .cloned()
                    .unwrap();

                return Some(SymbolData::Error {
                    type_name: sway::TypeName::Identifier {
                        name: error_enum.0.borrow().name.clone(),
                        generic_parameters: None,
                    },
                    variant,
                });
            }
        }

        Symbol::Struct(name) => {
            if let Some(struct_definition) = module
                .borrow()
                .structs
                .iter()
                .find(|s| s.signature.to_string() == *name)
            {
                return Some(SymbolData::Struct(
                    struct_definition.implementation.clone().unwrap(),
                ));
            }
        }
    }

    for use_expr in module.borrow().uses.iter() {
        if let Some(imported_module) = project.resolve_use(use_expr) {
            if let Some(symbol) = resolve_symbol(
                project,
                imported_module.clone(),
                scope.clone(),
                symbol.clone(),
            ) {
                return Some(symbol);
            }
        }
    }

    None
}
