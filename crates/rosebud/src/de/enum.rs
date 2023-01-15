use nom::{bytes::complete::tag, combinator::{map_res, opt, cut}, sequence::{pair, preceded, delimited}, character::complete::{multispace1, multispace0, char}, IResult};

use crate::{Variant, Enum, EnumDecl, TypeDecl};

use super::{type_identifier, schema::schema_decl, unique_list0, lexeme, lexeme_strict};

fn variant(input: &str) -> IResult<&str, Variant> {
    let variant_identifier = type_identifier;
    map_res(
        pair(variant_identifier, opt(preceded(multispace1, schema_decl))),
        |(identifier, maybe_schema)| {
            Ok::<Variant, ()>(Variant {
                identifier,
                schema: maybe_schema,
            })
        },
    )(input)
}

fn enum_def(input: &str) -> IResult<&str, Enum> {
    map_res(
        delimited(
            pair(char('{'), multispace0),
            unique_list0(lexeme(char(',')), true, variant, |variants, new_variant| {
                for ident in variants.iter().map(|v| v.identifier) {
                    if ident == new_variant.identifier {
                        return false;
                    }
                }
                true
            }),
            pair(multispace0, char('}')),
        ),
        |variants| Ok::<Enum, ()>(Enum { variants }),
    )(input)
}

#[test]
fn test_enum_def() {
    use nom_assert::*;
    use crate::{SchemaDecl, Schema, Field, Type};

    assert_parsed_eq!(enum_def("{}"), Enum { variants: vec![] });

    assert_parsed_eq!(
        enum_def("{Foo,Bar,Baz}"),
        Enum {
            variants: vec![
                Variant::new("Foo"),
                Variant::new("Bar"),
                Variant::new("Baz"),
            ]
        }
    );

    const COMPLEX_ENUM: &str = "{
Foo,
Bar,
Baz schema Baz {
    alpha string,
    beta i32,
},
}";
    assert_parsed_eq!(
        enum_def(COMPLEX_ENUM),
        Enum {
            variants: vec![
                Variant::new("Foo"),
                Variant::new("Bar"),
                Variant::new_with_schema(
                    "Baz",
                    Some(SchemaDecl {
                        identifier: "Baz",
                        def: Schema {
                            fields: vec![
                                Field::new("alpha", Type::String, false),
                                Field::new(
                                    "beta",
                                    Type::Integer {
                                        signed: true,
                                        size: 32
                                    },
                                    false
                                ),
                            ]
                        },
                    })
                ),
            ],
        }
    )
}

fn enum_decl(input: &str) -> IResult<&str, EnumDecl> {
    let (input, _) = lexeme_strict(tag("enum"))(input)?;
    let (input, identifier) = lexeme(type_identifier)(input)?;
    let (input, def) = cut(enum_def)(input)?;

    let decl = EnumDecl { identifier, def };

    Ok((input, decl))
}

#[inline]
pub(super) fn enum_type_decl(input: &str) -> IResult<&str, TypeDecl> {
    enum_decl(input).map(|(input, decl)| (input, TypeDecl::Enum(decl)))
}
