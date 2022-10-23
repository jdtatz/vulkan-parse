#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

mod into_xml;
mod lexer;
mod parse_xml;
mod parser;
mod typed_registry;

pub use crate::{
    into_xml::into_xml,
    lexer::{Constant, Error as LexerError, Token, TokenExtras},
    parse_xml::*,
    parser::*,
    typed_registry::*,
};

pub(crate) fn fmt_write_interspersed<I: Iterator, S: ?Sized + std::fmt::Display>(
    f: &mut std::fmt::Formatter,
    values: I,
    separator: &S,
) -> std::fmt::Result
where
    <I as Iterator>::Item: std::fmt::Display,
{
    let mut is_first = true;
    for v in values {
        if is_first {
            is_first = false;
            write!(f, "{}", v)?;
        } else {
            write!(f, "{}{}", separator, v)?;
        }
    }
    Ok(())
}

pub(crate) trait Container:
    IntoIterator + FromIterator<<Self as IntoIterator>::Item>
{
}
impl<T: IntoIterator + FromIterator<<Self as IntoIterator>::Item>> Container for T {}

pub(crate) struct Seperated<V, const C: char>(pub V);

impl<V: Container, const C: char> Seperated<V, C> {
    pub fn value(self) -> V {
        self.0
    }
}

impl<V: Container, const C: char> FromIterator<V::Item> for Seperated<V, C> {
    fn from_iter<I: IntoIterator<Item = V::Item>>(iter: I) -> Self {
        Self(V::from_iter(iter))
    }
}

impl<V: Container, const C: char> std::str::FromStr for Seperated<V, C>
where
    <V as IntoIterator>::Item: std::str::FromStr,
{
    type Err = <V::Item as std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split(C).map(V::Item::from_str).collect()
    }
}

impl<'a, V: 'a + Container, const C: char> TryFrom<&'a str> for Seperated<V, C>
where
    <V as IntoIterator>::Item: TryFrom<&'a str>,
{
    type Error = <V::Item as TryFrom<&'a str>>::Error;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        s.split(C).map(V::Item::try_from).collect()
    }
}
