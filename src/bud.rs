use std::str::FromStr;

use nom::{Finish, error::VerboseError};

use self::de::bud;

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

impl<'de> TypeDecl<'de> {
    pub fn identifier(&self) -> &str {
        match self {
            Self::Enum(enm) => enm.identifier,
            Self::Schema(schema) => schema.identifier,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Bud<'de> {
    decls: Vec<TypeDecl<'de>>,
}

impl<'de> Bud<'de> {
    pub fn from_str<'a>(input: &'de str) -> Result<Bud<'de>, VerboseError<&'a str>> {
        let (_, bud) = bud(input).unwrap();
        Ok(bud)
    }
}
