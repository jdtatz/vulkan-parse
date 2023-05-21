#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_errors_doc, clippy::too_many_lines)]

#[cfg(feature = "serialize")]
#[macro_use]
extern crate serde;

#[cfg(feature = "serialize")]
#[macro_use]
extern crate serde_with;

#[macro_use]
extern crate vulkan_parse_derive_helper;

pub mod codegen;
mod lexer;
mod parse_xml;
mod parser;
mod typed_registry;

pub use crate::{
    lexer::{tokenize, Constant, Error as LexerError, Token, TokenExtras},
    parse_xml::*,
    parser::*,
    typed_registry::*,
};

pub trait Iterable {
    type IT<'i>: IntoIterator
    where
        Self: 'i;
    fn iter<'s>(&'s self) -> Self::IT<'s>;
}
impl<'a, I: ?Sized + Iterable> Iterable for &'a I {
    type IT<'i> = I::IT<'i> where Self: 'i, I: 'i;

    fn iter<'s>(&'s self) -> Self::IT<'s> {
        (*self).iter()
    }
}

impl<T> Iterable for [T] {
    type IT<'i> = &'i [T] where T: 'i;

    fn iter<'s>(&'s self) -> Self::IT<'s> {
        self
    }
}
impl<T> Iterable for Vec<T> {
    type IT<'i> = &'i [T] where T: 'i;

    fn iter<'s>(&'s self) -> Self::IT<'s> {
        self.as_slice()
    }
}
impl<T: enumflags2::BitFlag> Iterable for enumflags2::BitFlags<T> {
    type IT<'i> = enumflags2::BitFlags<T>;

    fn iter<'s>(&'s self) -> Self::IT<'s> {
        *self
    }
}

#[cfg(feature = "roundtrip")]
pub mod into_xml {
    use std::io::{Error as IoError, Write};

    use crate::{Escaped, IntoXML, XMLWriter};

    struct StdXMLWrite<W: Write>(W);
    impl<W: Write> crate::XMLWriter for StdXMLWrite<W> {
        type Error = IoError;

        fn write_escaped<V: ?Sized + crate::DisplayEscaped>(
            &mut self,
            escaped: &V,
        ) -> Result<(), Self::Error> {
            write!(self.0, "{}", Escaped(escaped))
        }
    }

    pub fn into_xml<W: Write>(reg: &crate::Registry, to: W) -> Result<(), IoError> {
        let mut writer = StdXMLWrite(to);
        writer.write_declaration("1.0", Some("UTF-8"), None)?;
        reg.write_xml(&mut writer)
    }
}
