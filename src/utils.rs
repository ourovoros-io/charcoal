/// Find a key in a [toml::Table] and return the [toml::Value]
#[inline(always)]
pub fn find_in_toml_value<'a>(
    value: &'a toml::Value,
    section: &str,
    key: &str,
) -> Option<&'a toml::Value> {
    let mut section_iter = section.split('.');
    // Attempt to extract the table from the provided TOML value
    let mut incoming_table = match value {
        toml::Value::Table(table) => table,
        _ => return None,
    };

    for section in section_iter.by_ref() {
        // Attempt to get the next section as a table
        let Some(toml::Value::Table(table)) = incoming_table.get(section) else {
            return None;
        };
        incoming_table = table;
    }

    incoming_table.get(key)
}
