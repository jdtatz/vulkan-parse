mod lexer;
mod parse_xml;
mod parser;
mod typed_registry;

pub use crate::lexer::*;
pub use crate::parse_xml::*;
pub use crate::parser::*;
pub use crate::typed_registry::*;

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
