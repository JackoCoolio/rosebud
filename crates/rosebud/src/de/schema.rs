use nom::{IResult, bytes::complete::tag, combinator::{map_res, cut}, sequence::delimited, character::complete::char};

use crate::{Schema, TypeDecl, SchemaDecl};

use super::{unique_list0, lexeme, field::field, lexeme_strict, type_identifier};

/// Tries to parse a [`Schema`] from a `&str`.
fn schema_def(input: &str) -> IResult<&str, Schema> {
    map_res(
        delimited(
            lexeme(char('{')),
            unique_list0(lexeme(char(',')), true, field, |fields, new_field| {
                for ident in fields.iter().map(|f| f.identifier) {
                    if ident == new_field.identifier {
                        return false;
                    }
                }
                true
            }),
            char('}'),
        ),
        |fields| Ok::<Schema, ()>(Schema { fields }),
    )(input)
}

#[test]
fn test_schema_def() {
    use nom_assert::*;
    use crate::*;

    const PERSON: &str = "{
  name string,
  address Location,
  occupation string?,
  age u8,
}";

    assert_parsed_eq!(
        schema_def(PERSON),
        Schema {
            fields: vec![
                Field::new("name", Type::String, false),
                Field::new(
                    "address",
                    Type::Extern {
                        identifier: "Location"
                    },
                    false
                ),
                Field::new("occupation", Type::String, true),
                Field::new(
                    "age",
                    Type::Integer {
                        signed: false,
                        size: 8
                    },
                    false
                ),
            ],
        }
    );

    const CAR: &str = "{manufacturer string}";
    assert_parsed_eq!(
        schema_def(CAR),
        Schema {
            fields: vec![Field::new("manufacturer", Type::String, false),],
        }
    );

    const SOCKET_ADDR: &str = "{host string, port u8}";
    assert_parsed_eq!(
        schema_def(SOCKET_ADDR),
        Schema {
            fields: vec![
                Field::new("host", Type::String, false),
                Field::new(
                    "port",
                    Type::Integer {
                        signed: false,
                        size: 8
                    },
                    false
                ),
            ],
        }
    );

    const DUPLICATE_FIELD: &str = "{foo string, foo i32}";
    assert!(schema_def(DUPLICATE_FIELD).is_err());
}

pub(in super) fn schema_decl(input: &str) -> IResult<&str, SchemaDecl> {
    let (input, _) = lexeme_strict(tag("schema"))(input)?;
    let (input, identifier) = lexeme(type_identifier)(input)?;
    let (input, def) = cut(schema_def)(input)?;

    let decl = SchemaDecl { identifier, def };

    Ok((input, decl))
}

#[inline]
pub(super) fn schema_type_decl(input: &str) -> IResult<&str, TypeDecl> {
    schema_decl(input).map(|(input, decl)| (input, TypeDecl::Schema(decl)))
}

#[test]
fn test_schema_decl() {
    use nom_assert::*;
    use crate::*;

    assert_parsed_eq!(
        schema_decl("schema Foo {}"),
        SchemaDecl {
            identifier: "Foo",
            def: Schema { fields: vec![] },
        }
    );

    // must have identifier
    assert!(schema_decl("schema  {}").is_err());
    // must begin with "schema"
    assert!(schema_decl("Foo {}").is_err());
}
