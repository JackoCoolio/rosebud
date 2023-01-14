use nom::{branch::alt,  combinator::{map_res, recognize, opt}, multi::{many1, many0}, sequence::{terminated, preceded, pair, tuple}, character::{complete::{one_of, char, alphanumeric0}, is_digit}, bytes::complete::tag, IResult};

use crate::bud::{Type, Field};

use super::{type_identifier, LOWERCASE, lexeme_strict, lexeme};

// TODO: move these macros do a different crate, so we don't have to copy them

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

/// Tries to parse a `usize` from a `&str`.
fn decimal(input: &str) -> IResult<&str, usize> {
    map_res(
        many1(terminated(one_of("1234567890"), many0(char('_')))),
        |x| {
            let st = x.iter().filter(|c| is_digit(**c as u8)).collect::<String>();
            st.parse::<usize>()
        },
    )(input)
}

#[test]
fn test_decimal() {
    assert_parsed!(decimal("1"), 1);
    assert_parsed!(decimal("32"), 32);
}

/// Returns true if `size` is a valid integer size, i.e. 8, 16, 32, 64, or 128.
fn is_valid_integer_size(size: usize) -> bool {
    matches!(size, 8 | 16 | 32 | 64 | 128)
}

/// Returns true if `size` is a valid float size, i.e. 32 or 64.
fn is_valid_float_size(size: usize) -> bool {
    matches!(size, 32 | 64)
}

/// Tries to parse the characters "string" into a [`Type::String`].
fn field_type_string(input: &str) -> IResult<&str, Type<'_>> {
    map_res(tag("string"), |_| Ok::<Type, ()>(Type::String))(input)
}

#[test]
fn test_field_type_string() {
    assert_parsed!(field_type_string("string"), Type::String);
    assert!(field_type_string("").is_err());
}

fn _field_type_integer(input: &str, signed: bool) -> IResult<&str, Type<'_>> {
    map_res(
        preceded(char(if signed { 'i' } else { 'u' }), decimal),
        |x| {
            if is_valid_integer_size(x) {
                Ok(Type::Integer { signed, size: x })
            } else {
                Err("invalid integer size")
            }
        },
    )(input)
}

/// Tries to parse a [`Type::Integer`] from a `&str`.
fn field_type_integer(input: &str) -> IResult<&str, Type> {
    alt((_field_type_signed_integer, _field_type_unsigned_integer))(input)
}

/// Tries to parse a signed [`Type::Integer`] from a `&str`.
#[inline]
fn _field_type_signed_integer(input: &str) -> IResult<&str, Type> {
    _field_type_integer(input, true)
}

#[test]
fn test_field_type_signed_integer() {
    // must start with 'i'
    assert!(_field_type_signed_integer("32").is_err());

    // must not be capitalized
    assert!(_field_type_signed_integer("I32").is_err());

    // must have a size
    assert!(_field_type_signed_integer("i").is_err());

    // must have a valid size
    assert!(_field_type_signed_integer("i31").is_err());

    assert_parsed!(
        _field_type_signed_integer("i32"),
        Type::Integer {
            signed: true,
            size: 32
        }
    );
}

/// Tries to parse an unsigned [`Type::Integer`] from a `&str`.
#[inline]
fn _field_type_unsigned_integer(input: &str) -> IResult<&str, Type> {
    _field_type_integer(input, false)
}

#[test]
fn test_field_type_unsigned_integer() {
    // must start with 'u'
    assert!(_field_type_unsigned_integer("32").is_err());

    // must not be capitalized
    assert!(_field_type_unsigned_integer("U32").is_err());

    // must have a size
    assert!(_field_type_unsigned_integer("u").is_err());

    // must have a valid size
    assert!(_field_type_unsigned_integer("u31").is_err());

    assert_parsed!(
        _field_type_unsigned_integer("u32"),
        Type::Integer {
            signed: false,
            size: 32
        }
    );
}

/// Tries to parse a [`Type::Extern`] from a `&str`.
fn field_type_extern(input: &str) -> IResult<&str, Type> {
    map_res(type_identifier, |x| {
        Ok::<Type, ()>(Type::Extern { identifier: x })
    })(input)
}

#[test]
fn test_field_type_extern() {
    // must begin with a capital letter
    assert!(field_type_extern("foo").is_err());
    // must begin with an alpha character
    assert!(field_type_extern("1foo").is_err());

    // regular capitalized words are fine
    assert_parsed!(field_type_extern("Foo"), Type::Extern { identifier: "Foo" });
    // capitalized word with numbers is fine
    assert_parsed!(
        field_type_extern("Foo123"),
        Type::Extern {
            identifier: "Foo123"
        }
    );
}

/// Tries to parse a [`Type`] from a `&str`.
fn field_type(input: &str) -> IResult<&str, Type> {
    alt((
        field_type_string,
        _field_type_unsigned_integer,
        _field_type_signed_integer,
        field_type_extern,
    ))(input)
}

#[test]
fn test_field_type() {
    assert_parsed!(field_type("string"), Type::String);
    assert_parsed!(
        field_type("u32"),
        Type::Integer {
            signed: false,
            size: 32
        }
    );
    assert_parsed!(
        field_type("i32"),
        Type::Integer {
            signed: true,
            size: 32
        }
    );
    assert_parsed!(field_type("Foo"), Type::Extern { identifier: "Foo" });
}

/// Tries to parse a field identifier from a `&str`.
fn field_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(one_of(LOWERCASE), alphanumeric0))(input)
}

#[test]
fn test_field_identifier() {
    assert!(field_identifier("foo").is_ok());
    assert!(field_identifier("").is_err());
    assert!(field_identifier("Foo").is_err());
    assert!(field_identifier("1Foo").is_err());
}

/// Tries to parse a "?" from a `&str` and returns true if the "?" was parsed.
fn optional(input: &str) -> IResult<&str, bool> {
    map_res(opt(char('?')), |x| Ok::<bool, ()>(x.is_some()))(input)
}

#[test]
fn test_optional() {
    assert!(matches!(optional("?"), Ok((_, true))));
    assert!(matches!(optional(""), Ok((_, false))));
    assert!(matches!(optional("a"), Ok((_, false))));
}

/// Tries to parse a [`Field`] from a `&str`.
pub(super) fn field(input: &str) -> IResult<&str, Field> {
    map_res(
        tuple((
            lexeme_strict(field_identifier),
            lexeme(field_type),
            lexeme(optional),
        )),
        |(identifier, typ, optional)| {
            Ok::<Field, ()>(Field {
                identifier,
                typ,
                optional,
            })
        },
    )(input)
}

#[test]
fn test_field() {
    assert!(field("foo string?").is_ok());
    assert!(field("bar string").is_ok());
    assert!(field("foostring ?").is_err());
    assert_parsed!(
        field("name string"),
        Field {
            identifier: "name",
            typ: Type::String,
            optional: false
        }
    );
    assert_parsed!(
        field("address Location"),
        Field {
            identifier: "address",
            typ: Type::Extern {
                identifier: "Location"
            },
            optional: false
        }
    );
    assert_parsed!(
        field("occupation string?"),
        Field {
            identifier: "occupation",
            typ: Type::String,
            optional: true,
        }
    );
}
