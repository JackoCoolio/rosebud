use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
            alphanumeric0, multispace0, multispace1, one_of, not_line_ending, space0,
        },
    combinator::recognize,
    sequence::{pair, terminated},
    AsChar, IResult, InputLength, InputTakeAtPosition, Parser, error::ParseError, multi::many0,
};

use self::{schema::schema_type_decl, r#enum::enum_type_decl};

use super::{TypeDecl, GlobalExpr, BudFile};

mod field;
mod schema;
mod r#enum;

const UPPERCASE: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const LOWERCASE: &str = "abcdefghijklmnopqrstuvwxyz";

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
    uniq: U,
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

fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("//")(input)?;
    let (input, _) = space0(input)?;
    let (input, comment) = not_line_ending(input)?;
    Ok((input, comment))
}

#[test]
fn test_comment() {
    use nom_assert::*;

    assert_parsed_matches!(comment("// foo"), "foo");
    assert_parsed_matches!(comment("//foo"), "foo");
}

fn comment_global_expr(input: &str) -> IResult<&str, GlobalExpr> {
    let (input, comment) = comment(input)?;
    Ok((input, GlobalExpr::Comment(comment)))
}

fn type_decl(input: &str) -> IResult<&str, TypeDecl> {
    alt((schema_type_decl, enum_type_decl))(input)
}

fn type_decl_global_expr(input: &str) -> IResult<&str, GlobalExpr> {
    let (input, decl) = type_decl(input)?;
    Ok((input, GlobalExpr::TypeDecl(decl)))
}

fn global_expr(input: &str) -> IResult<&str, GlobalExpr> {
    alt((type_decl_global_expr, comment_global_expr))(input)
}

pub(super) fn bud_file(input: &str) -> IResult<&str, BudFile> {
    let (input, exprs) = many0(lexeme(global_expr))(input)?;
    Ok((input, BudFile { exprs }))
}

#[test]
fn test_bud_file() {
    use nom_assert::*;
    use crate::{EnumDecl, Enum, SchemaDecl, Schema};

    assert_parsed_eq!(bud_file("enum Foo {}\n// foo\nschema Bar {}\n"), BudFile {
        exprs: vec![
            GlobalExpr::TypeDecl(TypeDecl::Enum(EnumDecl {
                identifier: "Foo",
                def: Enum {
                    variants: Vec::new(),
                }
            })),
            GlobalExpr::Comment("foo"),
            GlobalExpr::TypeDecl(TypeDecl::Schema(SchemaDecl {
                identifier: "Bar",
                def: Schema {
                    fields: Vec::new(),
                }
            })),
        ]
    });
}
