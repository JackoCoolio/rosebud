use std::{fmt::Display, iter::once};

use super::{Bud, Enum, EnumDecl, Field, Schema, SchemaDecl, Type, TypeDecl, Variant, BudFile, GlobalExpr};

fn indent(input: &str, spaces: usize) -> String {
    let mut res = String::new();
    let margin: String = once(' ').cycle().take(spaces).collect();
    for line in input.lines() {
        if !line.is_empty() {
            res.push_str(&margin);
            res.push_str(line);
        }
        res.push('\n');
    }
    res
}

impl<'de> Display for BudFile<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exprs_str = self
            .exprs
            .iter()
            .map(|expr| format!("{}", expr))
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{}", exprs_str)
    }
}

impl<'de> Display for GlobalExpr<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(cmt) => write!(f, "// {}", cmt),
            Self::TypeDecl(decl) => writeln!(f, "{}", decl),
        }
    }
}

impl<'de> Display for Bud<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decls_str = self
            .decls
            .iter()
            .map(|decl| format!("{}\n", decl))
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{}", decls_str)
    }
}

impl<'de> Display for TypeDecl<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Enum(enum_decl) => enum_decl.fmt(f),
            Self::Schema(schema_decl) => schema_decl.fmt(f),
        }
    }
}

impl<'de> Display for EnumDecl<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {} {}", self.identifier, self.def)
    }
}

impl<'de> Display for Enum<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        let mut variants_str = String::new();
        for variant in &self.variants {
            variants_str.push_str(&format!("{}\n", variant));
        }

        variants_str = indent(&variants_str, 4);
        write!(f, "{}}}", variants_str)
    }
}

impl<'de> Display for Variant<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)?;
        if let Some(schema_decl) = &self.schema {
            write!(f, " {}", schema_decl)?;
        };
        write!(f, ",")
    }
}

impl<'de> Display for SchemaDecl<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "schema {} {}", self.identifier, self.def)
    }
}

impl<'de> Display for Schema<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        let mut fields_str = String::new();
        for field in &self.fields {
            fields_str.push_str(&format!("{}\n", field));
        }

        fields_str = indent(&fields_str, 4);
        write!(f, "{}}}", fields_str)
    }
}

impl<'de> Display for Field<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}{},",
            self.identifier,
            self.typ,
            if self.optional { "?" } else { "" }
        )
    }
}

impl<'de> Display for Type<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::String => "string".to_string(),
                Self::Integer { signed, size } =>
                    format!("{}{}", if *signed { 'i' } else { 'u' }, size),
                Self::Extern { identifier } => identifier.to_string(),
                _ => unimplemented!(),
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_type() {
        assert_eq!(format!("{}", Type::String), "string");
        assert_eq!(
            format!(
                "{}",
                Type::Integer {
                    signed: true,
                    size: 32
                }
            ),
            "i32"
        );
        assert_eq!(
            format!(
                "{}",
                Type::Integer {
                    signed: false,
                    size: 8
                }
            ),
            "u8"
        );
        assert_eq!(
            format!(
                "{}",
                Type::Extern {
                    identifier: "Apple"
                }
            ),
            "Apple"
        );
    }

    #[test]
    fn display_field() {
        assert_eq!(
            format!("{}", Field::new("foo", Type::String, true)),
            "foo string?,"
        );
        assert_eq!(
            format!(
                "{}",
                Field::new(
                    "bar",
                    Type::Integer {
                        signed: true,
                        size: 64
                    },
                    false
                )
            ),
            "bar i64,"
        );
        assert_eq!(
            format!(
                "{}",
                Field::new(
                    "baz",
                    Type::Extern {
                        identifier: "Banana"
                    },
                    true
                )
            ),
            "baz Banana?,"
        );
    }

    #[test]
    fn display_schema() {
        const EXPECTED: &str = "{
    foo string,
    bar Baz?,
}";
        assert_eq!(
            format!(
                "{}",
                Schema {
                    fields: vec![
                        Field::new("foo", Type::String, false),
                        Field::new("bar", Type::Extern { identifier: "Baz" }, true),
                    ]
                }
            ),
            EXPECTED
        );
    }
}
