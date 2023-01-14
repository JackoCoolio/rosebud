use std::{collections::HashSet, fmt::Debug};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{
            alpha1, alphanumeric0, char, digit1, multispace0, multispace1, newline, one_of,
        },
        is_alphabetic, is_digit,
    },
    combinator::{map_res, opt, recognize, verify, cut},
    error::{dbg_dmp, FromExternalError, ParseError},
    multi::{many0, many0_count, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsChar, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Parser,
    UnspecializedInput,
};

use self::{field::field, schema::schema_type_decl, r#enum::enum_type_decl};

use super::{Bud, Enum, EnumDecl, Field, Schema, SchemaDecl, Type, TypeDecl, Variant};

mod field;
mod schema;
mod r#enum;

const UPPERCASE: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const LOWERCASE: &str = "abcdefghijklmnopqrstuvwxyz";

#[macro_export()]
macro_rules! parsed {
    ($x:pat) => {
        Ok((_, $x))
    };
}

macro_rules! assert_parsed {
    ($parse:expr, $expected:pat) => {
        match $parse {
            Ok((_, $expected)) => (),
            Ok((rem, actual)) => {
                panic!(
                    "parsed '{:?}' but returned invalid result: '{:?}', with remaining input:
                    '{:?}'",
                    stringify!($parse),
                    actual,
                    rem
                );
            }
            Err(e) => panic!("couldn't parse '{:?}': {:?}", stringify!($parse), e),
        }
        assert!(matches!($parse, Ok((_, $expected))))
    };
}

macro_rules! assert_parsed_eq {
    ($parse:expr, $expected:expr) => {
        match $parse {
            Ok((_, parsed)) if parsed == $expected => (),
            Ok((_, actual)) => panic!(
                "parsed '{:?}' incorrectly, actual: {:?}",
                stringify!($parse),
                actual
            ),
            Err(e) => panic!("couldn't parse: {:?}", e),
        }
    };
}

fn type_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(one_of(UPPERCASE), alphanumeric0))(input)
}

#[test]
fn test_type_identifier() {
    assert!(type_identifier("Foo").is_ok());
    assert!(type_identifier("foo").is_err());
}

fn lexeme<I, O, F, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    terminated(f, multispace0)
}

fn lexeme_strict<I, O, F, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    terminated(f, multispace1)
}

/// Tries to parse a separated list into a [`Vec`], where all elements must satisfy the given
/// function. If not all elements satisfy the function, the parse fails.
fn unique_list0<I, O, O2, E, U, F, G>(
    mut sep: G,
    allow_trailing: bool,
    mut f: F,
    mut uniq: U,
) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + InputLength,
    E: ParseError<I>,
    F: Parser<I, O, E>,
    G: Parser<I, O2, E>,
    U: Fn(&Vec<O>, &O) -> bool,
{
    move |mut i: I| {
        let mut res = Vec::new();

        match f.parse(i.clone()) {
            Err(nom::Err::Error(_)) => return Ok((i, res)), // no results
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                if uniq(&res, &o) {
                    res.push(o);
                    i = i1;
                } else {
                    return Err(nom::Err::Error(E::from_error_kind(
                        i,
                        nom::error::ErrorKind::SeparatedList,
                    )));
                }
            }
        }

        loop {
            let len = i.input_len();
            match sep.parse(i.clone()) {
                Err(nom::Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, _)) => {
                    if i1.input_len() == len {
                        return Err(nom::Err::Error(E::from_error_kind(
                            i,
                            nom::error::ErrorKind::SeparatedList,
                        )));
                    }

                    match f.parse(i1.clone()) {
                        Err(nom::Err::Error(_)) => {
                            return Ok((if allow_trailing { i1 } else { i }, res))
                        }
                        Err(e) => return Err(e),
                        Ok((i2, o)) => {
                            if uniq(&res, &o) {
                                res.push(o);
                                i = i2;
                            } else {
                                return Err(nom::Err::Error(E::from_error_kind(
                                    i1,
                                    nom::error::ErrorKind::SeparatedList,
                                )));
                            }
                        }
                    }
                }
            }
        }
    }
}


fn type_decl(input: &str) -> IResult<&str, TypeDecl> {
    alt((schema_type_decl, enum_type_decl))(input)
}

pub(super) fn bud(input: &str) -> IResult<&str, Bud> {
    many0(lexeme(type_decl))(input).map(|(input, decls)| (input, Bud { decls }))
}

#[test]
fn test_bud() {
    assert_parsed_eq!(bud("enum Foo {}\nschema Bar {}\n"), Bud {
        decls: vec![
            TypeDecl::Enum(EnumDecl {
                identifier: "Foo",
                def: Enum {
                    variants: Vec::new(),
                }
            }),
            TypeDecl::Schema(SchemaDecl {
                identifier: "Bar",
                def: Schema {
                    fields: Vec::new(),
                }
            })
        ]
    });
}
