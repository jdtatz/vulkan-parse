use std::{
    fmt,
    iter::{Filter, FilterMap, Peekable},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    str::FromStr,
};

pub use roxmltree::Document;
use roxmltree::{Node, NodeId};

use crate::{Container, LexerError, ParseError, Registry, Seperated, TryFromTokens, VkXMLTokens};

#[derive(Debug)]
pub enum ErrorKind {
    NoMatch(NodeId),
    /// Empty elements are disallowed in vulkan's mixed pseudo-c/xml
    EmptyElement(NodeId),
    MissingAttribute(&'static str, NodeId),
    UnknownAttribute(String, NodeId),
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
            ErrorKind::UnknownAttribute(key, id) => write!(
                f,
                "Attribute {:?} was not expected in {}",
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

pub trait TryFromStr<'s, const BORROW: bool = true>: 's + Sized {
    type Error;

    fn try_from_str(s: &'s str) -> Result<Self, Self::Error>;
}

impl<'s, T: 'static + FromStr> TryFromStr<'s, false> for T {
    type Error = <Self as FromStr>::Err;

    fn try_from_str(s: &'s str) -> Result<Self, Self::Error> {
        Self::from_str(s)
    }
}

impl<'s, T: 's + TryFrom<&'s str>> TryFromStr<'s, true> for T {
    type Error = <Self as TryFrom<&'s str>>::Error;

    fn try_from_str(s: &'s str) -> Result<Self, Self::Error> {
        Self::try_from(s)
    }
}

// Generic attribute getter with conv
pub(crate) fn try_attribute<'a, 'input, T: TryFromStr<'a, BORROW>, const BORROW: bool>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<Option<T>>
where
    T::Error: 'static + std::error::Error,
{
    node.attribute(attr)
        .map(TryFromStr::try_from_str)
        .transpose()
        .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node.id()))
}

pub(crate) fn attribute<'a, 'input, T: TryFromStr<'a, BORROW>, const BORROW: bool>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<T>
where
    T::Error: 'static + std::error::Error,
{
    try_attribute(node, attr)?.ok_or_else(|| ErrorKind::MissingAttribute(attr, node.id()))
}

pub(crate) fn optional_attribute<'xml, T: TryFromStr<'xml, BORROW>, const BORROW: bool>(
    value: Option<&'xml str>,
    attr: &'static str,
    node_id: roxmltree::NodeId,
) -> ParseResult<Option<T>>
where
    T::Error: 'static + std::error::Error,
{
    value
        .map(TryFromStr::try_from_str)
        .transpose()
        .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node_id))
}

pub(crate) fn required_attribute<'xml, T: TryFromStr<'xml, BORROW>, const BORROW: bool>(
    value: Option<&'xml str>,
    attr: &'static str,
    node_id: roxmltree::NodeId,
) -> ParseResult<T>
where
    T::Error: 'static + std::error::Error,
{
    optional_attribute(value, attr, node_id)?
        .ok_or_else(|| ErrorKind::MissingAttribute(attr, node_id))
}

pub(crate) trait Parse<'a>: Sized {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>>;
    fn parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Self> {
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

fn node_is_element(node: &Node) -> bool {
    node.is_element() || (node.is_text() && node.text().map_or(false, |s| !s.trim().is_empty()))
}

type NodeFilterFn = for<'a, 'input, 'b> fn(&'b Node<'a, 'input>) -> bool;
type PeekableChildrenElementsIter<'a, 'input> =
    Peekable<Filter<roxmltree::Children<'a, 'input>, NodeFilterFn>>;

#[derive(Debug, Clone)]
pub struct PeekableChildrenElements<'a, 'input> {
    iter: PeekableChildrenElementsIter<'a, 'input>,
    pub parent_id: NodeId,
}

impl<'a, 'input: 'a> From<Node<'a, 'input>> for PeekableChildrenElements<'a, 'input> {
    fn from(value: Node<'a, 'input>) -> Self {
        let filter_children: Filter<_, NodeFilterFn> = value.children().filter(node_is_element);
        Self {
            iter: filter_children.peekable(),
            parent_id: value.id(),
        }
    }
}

impl<'a, 'input: 'a> Deref for PeekableChildrenElements<'a, 'input> {
    type Target = PeekableChildrenElementsIter<'a, 'input>;

    fn deref(&self) -> &Self::Target {
        &self.iter
    }
}

impl<'a, 'input: 'a> DerefMut for PeekableChildrenElements<'a, 'input> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.iter
    }
}

trait PeekableExt<I: Iterator> {
    fn next_if_some<R>(&mut self, func: impl FnOnce(&I::Item) -> Option<R>) -> Option<R>;
}

impl<I: Iterator> PeekableExt<I> for Peekable<I> {
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

pub trait ParseChildren<'node>: 'node + Sized {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self>;
}

pub(crate) fn parse_children<'node, 'input: 'node, T: ParseChildren<'node>>(
    node: Node<'node, 'input>,
) -> ParseResult<T> {
    let mut it = node.into();
    let res = T::from_children(&mut it)?;
    if let Some(n) = it.next() {
        Err(ErrorKind::NoMatch(n.id()))
    } else {
        Ok(res)
    }
}

impl<'node, T: 'node + Parse<'node>> ParseChildren<'node> for T {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        // FIXME replace "$" with either a way of propagatting the tag name from T or introduce a new ErrorKind
        it.next().map_or(
            Err(ErrorKind::MissingChildElement("$", it.parent_id)),
            T::parse,
        )
    }
}

impl<'node, T: 'node + Parse<'node>> ParseChildren<'node> for Option<T> {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        it.next_if_some(|item| T::try_parse(*item).transpose())
            .transpose()
    }
}

impl<'node, T: 'node + Parse<'node>> ParseChildren<'node> for Vec<T> {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        let mut out = Vec::new();
        while let Some(v) = it.next_if_some(|item| T::try_parse(*item).transpose()) {
            out.push(v?);
        }
        Ok(out)
    }
}

impl<'node> ParseChildren<'node> for &'node str {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        it.next().map_or(
            Err(ErrorKind::MissingChildElement("$text", it.parent_id)),
            |n| n.text().ok_or(ErrorKind::EmptyElement(n.id())),
        )
    }
}

#[impl_trait_for_tuples::impl_for_tuples(4)]
impl<'node> ParseChildren<'node> for Tuple {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        Ok(for_tuples!( (#( Tuple::from_children(it)? ),* )))
    }
}
