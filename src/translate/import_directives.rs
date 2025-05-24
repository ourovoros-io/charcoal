use super::TranslatedModule;
use crate::{error::Error, project::Project};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, path::PathBuf, rc::Rc};

#[inline]
pub fn translate_import_directives(
    project: &mut Project,
    translated_module: Rc<RefCell<TranslatedModule>>,
    import_directives: &[solidity::Import],
) -> Result<(), Error> {
    let source_unit_directory = project.root_folder.as_ref().unwrap().join(
        translated_module
            .borrow()
            .path
            .parent()
            .map(PathBuf::from)
            .unwrap(),
    );

    for import_directive in import_directives.iter() {
        match import_directive {
            solidity::Import::Plain(solidity::ImportPath::Filename(filename), _) => {
                let import_path =
                    project.canonicalize_import_path(&source_unit_directory, &filename.string)?;
                let import_path = PathBuf::from(
                    import_path
                        .to_string_lossy()
                        .to_string()
                        .trim_start_matches(
                            &project
                                .root_folder
                                .as_ref()
                                .unwrap()
                                .to_string_lossy()
                                .to_string(),
                        ),
                );

                let imported_module = project.find_module(&import_path);
                if imported_module.is_none() {
                    panic!(
                        "{}ERROR: failed to resolve import directive from `{import_directive}`",
                        match project.loc_to_line_and_column(
                            translated_module.clone(),
                            &import_directive.loc()
                        ) {
                            Some((line, col)) => format!(
                                "{}:{}:{}: ",
                                translated_module.borrow().path.to_string_lossy(),
                                line,
                                col
                            ),
                            None =>
                                format!("{}: ", translated_module.borrow().path.to_string_lossy()),
                        }
                    );
                }
            }

            solidity::Import::Rename(solidity::ImportPath::Filename(filename), identifiers, _) => {
                for (identifier, alias_identifier) in identifiers.iter() {
                    if alias_identifier.is_some() {
                        todo!("Handle import aliases");
                    }

                    let import_path = project
                        .canonicalize_import_path(&source_unit_directory, &filename.string)?;

                    todo!()
                    // let found = process_import(project, translated_module, Some(&identifier.name), &import_path)?;

                    // if !found {
                    //     panic!(
                    //         "{}ERROR: failed to resolve import directive from `{import_directive}`",
                    //         match project.loc_to_line_and_column(&translated_module.path, &import_directive.loc()) {
                    //             Some((line, col)) => format!("{}:{}:{}: ", translated_module.path.to_string_lossy(), line, col),
                    //             None => format!("{}: ", translated_module.path.to_string_lossy()),
                    //         }
                    //     );
                    // }
                }
            }

            _ => panic!("Unsupported import directive: {import_directive:#?}"),
        }
    }

    Ok(())
}
