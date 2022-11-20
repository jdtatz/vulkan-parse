use std::{fmt, str::FromStr};

pub use roxmltree::Document;
use roxmltree::{Node, NodeId};

use crate::{Container, LexerError, ParseError, Registry, Seperated};

#[derive(Debug)]
pub enum ErrorKind {
    NoMatch(NodeId),
    /// Empty elements are disallowed in vulkan's mixed pseudo-c/xml
    EmptyElement(NodeId),
    MissingAttribute(&'static str, NodeId),
    MissingChildElement(&'static str, NodeId),
    LexerError(LexerError, NodeId),
    PegParsingError(ParseError, NodeId),
    AttributeValueError(&'static str, Box<dyn std::error::Error + 'static>, NodeId),
    TextValueError(Box<dyn std::error::Error + 'static>, NodeId),
}

#[derive(Debug)]
pub struct Error<'d, 'input> {
    kind: ErrorKind,
    document: &'d Document<'input>,
}

#[derive(Debug, Clone, Copy)]
struct DocumentLocation<'a, 'input>(&'a Document<'input>, NodeId);

impl<'a, 'input> fmt::Display for DocumentLocation<'a, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(document, location) = *self;
        let node = document.get_node(location).unwrap();
        let ancestors = node.ancestors().collect::<Vec<_>>();
        for n in ancestors.into_iter().rev() {
            if n.is_root() {
                continue;
            }
            if n.is_text() {
                write!(f, "/{:?}", n.text().unwrap())?;
                break;
            }
            write!(f, "/{}", n.tag_name().name())?;
            let attrs = n.attributes();
            if !attrs.is_empty() {
                let mut is_first = true;
                write!(f, "[")?;
                for attr in attrs {
                    if is_first {
                        is_first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}=\"{}\"", attr.name(), attr.value())?;
                }
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}

impl<'d, 'input> fmt::Display for Error<'d, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ErrorKind::NoMatch(id) => write!(
                f,
                "No Match found at {}",
                DocumentLocation(self.document, *id)
            ),
            ErrorKind::EmptyElement(id) => write!(
                f,
                "Empty elements are disallowed in vulkan's mixed pseudo-c/xml, at {}",
                DocumentLocation(self.document, *id)
            ),
            ErrorKind::MissingAttribute(key, id) => write!(
                f,
                "Attribute {:?} not found in {}",
                key,
                DocumentLocation(self.document, *id)
            ),
            ErrorKind::MissingChildElement(tag, id) => write!(
                f,
                "No child with the tag-name {:?} was found at {}",
                tag,
                DocumentLocation(self.document, *id)
            ),
            ErrorKind::LexerError(e, id) => {
                let unk =
                    &self.document.get_node(*id).unwrap().text().unwrap()[e.span.start..e.span.end];
                writeln!(
                    f,
                    "Lexer encountered an unexpected token {:?} at {:?} in {}",
                    unk,
                    e.span,
                    DocumentLocation(self.document, *id),
                )
            }
            ErrorKind::PegParsingError(e, id) => write!(
                f,
                "Mixed Parsing Error at {}\n{}",
                DocumentLocation(self.document, *id),
                e
            ),
            ErrorKind::AttributeValueError(key, _, id) => write!(
                f,
                "Error encountered when parsing Attribute {:?} with value of {:?} in {}",
                key,
                self.document
                    .get_node(*id)
                    .unwrap()
                    .attribute(*key)
                    .unwrap(),
                DocumentLocation(self.document, *id)
            ),
            ErrorKind::TextValueError(_, id) => write!(
                f,
                "Error encountered when parsing the text {:?} of {}",
                self.document.get_node(*id).unwrap().text().unwrap(),
                DocumentLocation(self.document, *id)
            ),
        }
    }
}

impl<'d, 'input> std::error::Error for Error<'d, 'input> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            ErrorKind::PegParsingError(e, _) => Some(e),
            ErrorKind::AttributeValueError(_, e, _) => Some(&**e),
            _ => None,
        }
    }
}

pub(crate) type ParseResult<T> = std::result::Result<T, ErrorKind>;

// Generic attribute getter with conv
pub(crate) fn try_attribute<'a, 'input, T: 'a + TryFrom<&'a str>>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<Option<T>>
where
    T::Error: 'static + std::error::Error,
{
    node.attribute(attr)
        .map(TryFrom::try_from)
        .transpose()
        .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node.id()))
}

pub(crate) fn attribute<'a, 'input, T: 'a + TryFrom<&'a str>>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<T>
where
    T::Error: 'static + std::error::Error,
{
    try_attribute(node, attr)?.ok_or_else(|| ErrorKind::MissingAttribute(attr, node.id()))
}

// attribute getter+conv for primatives without TryFrom<&'a str>
pub(crate) fn try_attribute_fs<T: FromStr>(node: Node, attr: &'static str) -> ParseResult<Option<T>>
where
    T::Err: 'static + std::error::Error,
{
    node.attribute(attr)
        .map(FromStr::from_str)
        .transpose()
        .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node.id()))
}

pub(crate) fn attribute_fs<T: FromStr>(node: Node, attr: &'static str) -> ParseResult<T>
where
    T::Err: 'static + std::error::Error,
{
    try_attribute_fs(node, attr)?.ok_or_else(|| ErrorKind::MissingAttribute(attr, node.id()))
}

// attribute getter+conv for container-like types with a seperator
pub(crate) fn try_attribute_sep<'a, 'input, V: 'a + Container, const C: char>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<Option<V>>
where
    V::Item: 'a + TryFrom<&'a str>,
    <V::Item as TryFrom<&'a str>>::Error: 'static + std::error::Error,
{
    Ok(try_attribute::<Seperated<_, C>>(node, attr)?.map(Seperated::value))
}

#[allow(dead_code)]
pub(crate) fn attribute_sep<'a, 'input, V: 'a + Container, const C: char>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<V>
where
    V::Item: 'a + TryFrom<&'a str>,
    <V::Item as TryFrom<&'a str>>::Error: 'static + std::error::Error,
{
    try_attribute_sep::<V, C>(node, attr)?
        .ok_or_else(|| ErrorKind::MissingAttribute(attr, node.id()))
}

pub(crate) trait Parse<'a, 'input>: Sized {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>>;
    fn parse(node: Node<'a, 'input>) -> ParseResult<Self> {
        match Self::try_parse(node) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(ErrorKind::NoMatch(node.id())),
            Err(e) => Err(e),
        }
    }
}

pub fn parse_registry<'a, 'input: 'a>(
    document: &'a Document<'input>,
) -> Result<Registry<'a>, Error<'a, 'input>> {
    let root = document.root_element();
    Registry::parse(root).map_err(|kind| Error { kind, document })
}

trait PeekableExt<I: Iterator> {
    fn next_if_some<R>(&mut self, func: impl FnOnce(&I::Item) -> Option<R>) -> Option<R>;
}

impl<I: Iterator> PeekableExt<I> for core::iter::Peekable<I> {
    fn next_if_some<R>(
        &mut self,
        func: impl FnOnce(&<I as Iterator>::Item) -> Option<R>,
    ) -> Option<R> {
        let mut result = None;
        self.next_if(|item| {
            result = func(item);
            result.is_some()
        });
        result
    }
}

pub(crate) trait ParseChildren<'node, 'input: 'node>: 'node + Sized {
    fn from_children<I: Iterator<Item = Node<'node, 'input>>>(
        it: &mut core::iter::Peekable<I>,
        parent_id: NodeId,
    ) -> ParseResult<Self>;
}

pub(crate) fn parse_children<'node, 'input: 'node, T: ParseChildren<'node, 'input>>(
    node: Node<'node, 'input>,
) -> ParseResult<T> {
    let mut it = node.children().filter(Node::is_element).peekable();
    let res = T::from_children(&mut it, node.id())?;
    if let Some(n) = it.next() {
        Err(ErrorKind::NoMatch(n.id()))
    } else {
        Ok(res)
    }
}

impl<'node, 'input: 'node, T: 'node + Parse<'node, 'input>> ParseChildren<'node, 'input> for T {
    fn from_children<I: Iterator<Item = Node<'node, 'input>>>(
        it: &mut core::iter::Peekable<I>,
        parent_id: NodeId,
    ) -> ParseResult<Self> {
        // FIXME replace "$" with either a way of propagatting the tag name from T or introduce a new ErrorKind
        it.next().map_or(
            Err(ErrorKind::MissingChildElement("$", parent_id)),
            T::parse,
        )
    }
}

impl<'node, 'input: 'node, T: 'node + Parse<'node, 'input>> ParseChildren<'node, 'input>
    for Option<T>
{
    fn from_children<I: Iterator<Item = Node<'node, 'input>>>(
        it: &mut core::iter::Peekable<I>,
        _parent_id: NodeId,
    ) -> ParseResult<Self> {
        it.next_if_some(|item| T::try_parse(*item).transpose())
            .transpose()
    }
}

impl<'node, 'input: 'node, T: 'node + Parse<'node, 'input>> ParseChildren<'node, 'input>
    for Vec<T>
{
    fn from_children<I: Iterator<Item = Node<'node, 'input>>>(
        it: &mut core::iter::Peekable<I>,
        _parent_id: NodeId,
    ) -> ParseResult<Self> {
        let mut out = Vec::new();
        while let Some(v) = it.next_if_some(|item| T::try_parse(*item).transpose()) {
            out.push(v?);
        }
        Ok(out)
    }
}

#[impl_trait_for_tuples::impl_for_tuples(4)]
impl<'node, 'input: 'node> ParseChildren<'node, 'input> for Tuple {
    fn from_children<I: Iterator<Item=Node<'node, 'input>>>(it: &mut core::iter::Peekable<I>, parent_id: NodeId) -> ParseResult<Self> {
        Ok(for_tuples!( (#( Tuple::from_children(it, parent_id)? ),* )) )
    }
}
