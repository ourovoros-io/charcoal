use crate::{error::Error, ir, project::Project, sway};
use convert_case::{Case, Casing};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, path::PathBuf, rc::Rc};

#[inline]
pub fn translate_import_directives(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    import_directives: &[solidity::Import],
) -> Result<(), Error> {
    for import_directive in import_directives.iter() {
        match import_directive {
            solidity::Import::Plain(solidity::ImportPath::Filename(filename), _)
            | solidity::Import::Rename(solidity::ImportPath::Filename(filename), _, _) => {
                // Canonicalize the import path
                let import_path = PathBuf::from(
                    project
                        .canonicalize_import_path(
                            &project
                                .options
                                .input
                                .join(module.borrow().path.parent().unwrap()),
                            &filename.string,
                        )?
                        .to_string_lossy()
                        .to_string()
                        .trim_start_matches(&project.options.input.to_string_lossy().to_string()),
                );

                // Find the module being imported
                let imported_module = project.find_module(&import_path);

                let Some(imported_module) = imported_module else {
                    panic!(
                        "{}: ERROR: failed to resolve import directive from `{import_directive}`",
                        project
                            .loc_to_file_location_string(module.clone(), &import_directive.loc(),),
                    );
                };

                // Construct the use tree
                let mut use_tree = sway::UseTree::Glob;

                for component in imported_module.borrow().path.components().rev() {
                    match component {
                        std::path::Component::Prefix(_) => continue,
                        std::path::Component::RootDir => continue,
                        std::path::Component::CurDir => continue,

                        std::path::Component::ParentDir => {
                            // Discard the prefix of the use tree
                            let sway::UseTree::Path { suffix, .. } = use_tree else {
                                panic!("Malformed import path: {:#?}", import_path)
                            };

                            use_tree = *suffix;
                        }

                        std::path::Component::Normal(name) => {
                            let mut name = name
                                .to_string_lossy()
                                .to_string()
                                .replace(".", "_")
                                .to_case(Case::Snake);

                            if let "lib" | "src" | "main" = name.as_str() {
                                name = format!("_{name}");
                            }

                            use_tree = sway::UseTree::Path {
                                prefix: name,
                                suffix: Box::new(use_tree),
                            };
                        }
                    }
                }

                assert!(
                    use_tree != sway::UseTree::Glob,
                    "Invalid import path: {import_path:#?}",
                );

                // Add the use to the module if we haven't already
                let use_expr = sway::Use {
                    is_public: false,
                    tree: sway::UseTree::Path {
                        prefix: String::new(), // NOTE: intentionally empty to indicate crate root
                        suffix: Box::new(use_tree),
                    },
                };

                if !module.borrow().uses.contains(&use_expr) {
                    module.borrow_mut().uses.push(use_expr);
                }
            }

            _ => panic!("Unsupported import directive: {import_directive:#?}"),
        }
    }

    Ok(())
}
