use std::fmt;

use roxmltree::{Document, Node, NodeId};
use serde::Serialize;

use crate::Registry;

#[derive(Debug, Clone)]
pub(crate) enum ErrorKind {
    NoMatch(NodeId),
    /// Empty elements are disallowed in vulkan's mixed pseudo-c/xml
    EmptyElement(NodeId),
    MissingAttribute(&'static str, NodeId),
    MissingChildElement(&'static str, NodeId),
    MixedParseError(peg::error::ParseError<usize>, NodeId),
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
                DocumentLocation(&self.document, *id)
            ),
            ErrorKind::EmptyElement(id) => write!(
                f,
                "Empty elements are disallowed in vulkan's mixed pseudo-c/xml, at {}",
                DocumentLocation(&self.document, *id)
            ),
            ErrorKind::MissingAttribute(key, id) => write!(
                f,
                "Attribute {:?} not found in {}",
                key,
                DocumentLocation(&self.document, *id)
            ),
            ErrorKind::MissingChildElement(tag, id) => write!(
                f,
                "No child with the tag-name {:?} was found at {}",
                tag,
                DocumentLocation(&self.document, *id)
            ),
            ErrorKind::MixedParseError(e, id) => write!(
                f,
                "Mixed Parsing Error at {}\n{}",
                DocumentLocation(&self.document, *id),
                e
            ),
        }
    }
}

impl<'d, 'input> std::error::Error for Error<'d, 'input> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            ErrorKind::MixedParseError(e, _) => Some(e),
            _ => None,
        }
    }
}

pub(crate) type ParseResult<T> = std::result::Result<T, ErrorKind>;

pub(crate) fn get_req_attr<'a, 'input>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<&'a str> {
    node.attribute(attr)
        .ok_or_else(|| ErrorKind::MissingAttribute(attr, node.id()))
}

pub(crate) fn get_req_text<'a, 'input>(node: Node<'a, 'input>) -> ParseResult<&'a str> {
    node.text()
        .ok_or_else(|| ErrorKind::EmptyElement(node.id()))
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

impl<'a, 'input, T: Parse<'a, 'input>> Parse<'a, 'input> for Option<T> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match T::try_parse(node) {
            Ok(Some(v)) => Ok(Some(Some(v))),
            // TODO: which version do I want?
            // Ok(None) => Ok(Some(None)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn parse(node: Node<'a, 'input>) -> ParseResult<Self> {
        match T::try_parse(node) {
            Ok(Some(v)) => Ok(Some(v)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

pub(crate) trait ParseElements<'a, 'input: 'a>: Sized + FromIterator<Self::Item> {
    type Item: Parse<'a, 'input>;
    type NodeIter: Iterator<Item = Node<'a, 'input>>;

    fn get_nodes(node: Node<'a, 'input>) -> ParseResult<Option<Self::NodeIter>>;
}

impl<'a, 'input: 'a, V: ParseElements<'a, 'input>> Parse<'a, 'input> for V {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(nodes) = V::get_nodes(node)? {
            nodes
                .filter(Node::is_element)
                .map(V::Item::parse)
                .collect::<ParseResult<_>>()
                .map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse(node: Node<'a, 'input>) -> ParseResult<Self> {
        if let Some(nodes) = V::get_nodes(node)? {
            nodes.filter(Node::is_element).map(V::Item::parse).collect()
        } else {
            Err(ErrorKind::NoMatch(node.id()))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Terminated<V, T>(pub V, pub Option<T>);

impl<'a, 'input: 'a, V: ParseElements<'a, 'input>, T: Parse<'a, 'input>> Parse<'a, 'input>
    for Terminated<V, T>
where
    <V as ParseElements<'a, 'input>>::NodeIter: DoubleEndedIterator,
{
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }

    fn parse(node: Node<'a, 'input>) -> ParseResult<Self> {
        if let Some(nodes) = V::get_nodes(node)? {
            let mut it = nodes.filter(Node::is_element);
            if let Some(last) = it.next_back() {
                if let Some(t) = T::try_parse(last)? {
                    Ok(Terminated(
                        it.map(V::Item::parse).collect::<ParseResult<V>>()?,
                        Some(t),
                    ))
                } else {
                    Ok(Terminated(
                        it.chain(std::iter::once(last))
                            .map(V::Item::parse)
                            .collect::<ParseResult<V>>()?,
                        None,
                    ))
                }
            } else {
                Ok(Self(V::from_iter(std::iter::empty()), None))
            }
        } else {
            Err(ErrorKind::NoMatch(node.id()))
        }
    }
}

#[derive(Debug)]
pub struct XMLDocument<'input>(Document<'input>);

impl<'input> XMLDocument<'input> {
    pub fn new(xml: &'input str) -> Result<Self, roxmltree::Error> {
        Ok(XMLDocument(Document::parse(xml)?))
    }

    pub fn parse<'a>(&'a self) -> Result<Registry<'a>, Error<'a, 'input>> {
        let root = self.0.root_element();
        Registry::parse(root).map_err(|kind| Error {
            kind,
            document: &self.0,
        })
    }
}

// Generic Impls

impl<'a, 'input: 'a, T: Parse<'a, 'input>> ParseElements<'a, 'input> for Box<[T]> {
    type Item = T;

    // type NodeIter = roxmltree::AxisIter<'a, 'input>;
    type NodeIter = roxmltree::Children<'a, 'input>;

    fn get_nodes(node: Node<'a, 'input>) -> ParseResult<Option<Self::NodeIter>> {
        // Ok(Some(node.next_siblings()))
        Ok(Some(node.children()))
    }
}

impl<'a, 'input: 'a, T: Parse<'a, 'input>> ParseElements<'a, 'input> for Vec<T> {
    type Item = T;

    type NodeIter = roxmltree::Children<'a, 'input>;

    fn get_nodes(node: Node<'a, 'input>) -> ParseResult<Option<Self::NodeIter>> {
        Ok(Some(node.children()))
    }
}
