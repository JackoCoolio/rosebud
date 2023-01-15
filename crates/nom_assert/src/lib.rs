//! Some useful asserts for testing nom parsers.

/// Turns into the Ok part of the nom Result.
///
/// # Example
/// ```
/// use nom_assert::parsed;
/// use nom::{IResult, bytes::complete::tag};
///
/// fn parser(input: &str) -> IResult<&str, &str> {
///     tag("foo")(input)
/// }
///
/// match parser("foobar") {
///     parsed!("foo") => { /* ok */ }
///     _ => { /* err */ }
/// }
///
/// // turns into
///
/// match parser("foobar") {
///     Ok((_, "foo")) => { /* ok */ }
///     _ => { /* err */ }
/// }
/// ```
#[macro_export]
macro_rules! parsed {
    ($x:pat) => {
        Ok((_, $x))
    };
}

/// Assert that the result of the parser is Ok and the Ok value matches the given pattern.
///
/// # Example
/// ```
/// use nom_assert::assert_parsed_matches;
/// use nom::{IResult, bytes::complete::tag};
///
/// fn parser(input: &str) -> IResult<&str, &str> {
///     tag("foo")(input)
/// }
///
/// assert_parsed_matches!(parser("foobar"), "foo");
/// ```
#[macro_export]
macro_rules! assert_parsed_matches {
    ($parse:expr, $expected:pat) => {
        match $parse {
            Ok((_, $expected)) => (),
            Ok((rem, actual)) => panic!(
                "parsed '{:?}' but returned invalid result: '{:?}', with remaining input:
                '{:?}'",
                stringify!($parse),
                actual,
                rem
            ),
            Err(e) => panic!("couldn't parse '{:?}': {:?}", stringify!($parse), e),
        }
        assert!(matches!($parse, Ok((_, $expected))))
    };
}

/// Asserts that the result of parser is Ok and the Ok value is equal to the given expression.
/// The type of the left expression must impl [`PartialEq`].
///
/// # Example
/// ```
/// use nom_assert::assert_parsed_eq;
/// use nom::{IResult, bytes::complete::tag};
///
/// fn parser(input: &str) -> IResult<&str, &str> {
///     tag("foo")(input)
/// }
///
/// assert_parsed_eq!(parser("foobar"), "foo");
/// ```
#[macro_export]
macro_rules! assert_parsed_eq {
    ($parse:expr, $expected:expr) => {
        match $parse {
            Ok((_, parsed)) if parsed == $expected => (),
            Ok((rem, actual)) => panic!(
                "parsed '{:?}' but returned invalid result: '{:?}', with remaining input: '{:?}'",
                stringify!($parse),
                actual,
                rem,
            ),
            Err(e) => panic!("couldn't parse '{:?}': {:?}", stringify!($parse), e),
        }
    };
}
