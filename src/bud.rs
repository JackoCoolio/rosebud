use nom::{error::VerboseError, Finish};

use self::de::bud_file;

mod de;
mod ser;

#[derive(Debug, PartialEq)]
pub struct Enum<'de> {
    pub variants: Vec<Variant<'de>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDecl<'de> {
    pub identifier: &'de str,
    pub def: Enum<'de>,
}

#[derive(Debug, PartialEq)]
pub struct Variant<'de> {
    pub identifier: &'de str,
    pub schema: Option<SchemaDecl<'de>>,
}

#[cfg(test)]
impl<'de> Variant<'de> {
    pub fn new(identifier: &'de str) -> Self {
        Variant {
            identifier,
            schema: None,
        }
    }

    pub fn new_with_schema(identifier: &'de str, schema: Option<SchemaDecl<'de>>) -> Self {
        Variant { identifier, schema }
    }
}

#[derive(Debug, PartialEq)]
pub struct Schema<'de> {
    pub fields: Vec<Field<'de>>,
}

#[derive(Debug, PartialEq)]
pub struct SchemaDecl<'de> {
    pub identifier: &'de str,
    pub def: Schema<'de>,
}

#[derive(Debug, PartialEq)]
pub struct Field<'de> {
    pub identifier: &'de str,
    pub typ: Type<'de>,
    pub optional: bool,
}

#[cfg(test)]
impl<'de> Field<'de> {
    pub fn new(identifier: &'de str, typ: Type<'de>, optional: bool) -> Self {
        Field {
            identifier,
            typ,
            optional,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type<'de> {
    String,
    Integer { signed: bool, size: usize },
    Bytes { size: usize },
    Extern { identifier: &'de str },
    InlineTypeDecl { decl: TypeDecl<'de> },
    Float,
    Unit,
}

#[derive(Debug, PartialEq)]
pub enum TypeDecl<'de> {
    Enum(EnumDecl<'de>),
    Schema(SchemaDecl<'de>),
}

#[derive(Debug, PartialEq)]
enum GlobalExpr<'de> {
    Comment(&'de str),
    TypeDecl(TypeDecl<'de>),
}

#[derive(Debug, PartialEq)]
pub struct BudFile<'de> {
    exprs: Vec<GlobalExpr<'de>>,
}

impl<'de, 'a> BudFile<'de> where 'de: 'a {
    pub fn from_str(input: &'de str) -> Result<BudFile<'de>, nom::Err<nom::error::Error<&'a str>>> {
        let parse_result: Result<(&'de str, BudFile), nom::Err<nom::error::Error<&'a str>>> = bud_file(input);
        let (_input, file) = parse_result?;

        Ok(file)
    }
}

#[derive(Debug, PartialEq)]
pub struct Bud<'de> {
    decls: Vec<TypeDecl<'de>>,
}

impl<'de> From<BudFile<'de>> for Bud<'de> {
    fn from(file: BudFile<'de>) -> Self {
        Bud {
            decls: file.exprs.into_iter().filter_map(|expr| match expr {
                GlobalExpr::TypeDecl(decl) => Some(decl),
                _ => None,
            }).collect()
        }
    }
}
