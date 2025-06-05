
/// Find a key in a [toml::Table] and return the [toml::Value]
#[inline(always)]
pub fn find_in_toml_value(value: &toml::Value, section: &str, key: &str) -> Option<toml::Value> {
    let mut section = section.split(".");
    let toml::Value::Table(mut table) = value.clone() else {
        return None;
    };

    while let Some(section) = section.next() {
        let Some(toml::Value::Table(table_2)) = table.get(section) else {
            return None;
        };

        table = table_2.clone();
    }

    table.get(key).cloned()
}
