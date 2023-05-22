use std::{
    error::Error as StdError,
    fmt,
    iter::{Filter, Peekable},
    ops::{Deref, DerefMut},
};

pub use roxmltree::Document;
use roxmltree::{Node, NodeId};

use crate::{LexerError, ParseError, Registry, Seperator, TryFromEscapedStr, UnescapedStr};

#[derive(Debug)]
pub enum ErrorKind {
    NoMatch(NodeId),
    /// Empty elements are disallowed in vulkan's mixed pseudo-c/xml
    EmptyElement(NodeId),
    MissingAttribute(&'static str, NodeId),
    MissingAttributes(&'static [&'static str], NodeId),
    UnknownAttribute(String, NodeId),
    MissingChildElement(&'static str, NodeId),
    LexerError(LexerError, NodeId),
    PegParsingError(ParseError, NodeId),
    AttributeValueError(&'static str, Box<dyn StdError + 'static>, NodeId),
    TextValueError(Box<dyn StdError + 'static>, NodeId),
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
            if attrs.len() > 0 {
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
            ErrorKind::MissingAttributes(key, id) => write!(
                f,
                "Attribute(s) {:?} not found in {}",
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

impl<'d, 'input> StdError for Error<'d, 'input> {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match &self.kind {
            ErrorKind::LexerError(e, _) => Some(e),
            ErrorKind::PegParsingError(e, _) => Some(e),
            ErrorKind::AttributeValueError(_, e, _) => Some(&**e),
            ErrorKind::TextValueError(e, _) => Some(&**e),
            _ => None,
        }
    }
}

pub(crate) type ParseResult<T> = std::result::Result<T, ErrorKind>;

pub trait FromAttrValue<'xml>: Sized {
    fn from_attr_value<'input: 'xml>(
        value: roxmltree::Attribute<'xml, 'input>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> FromAttrValue<'xml> for T
where
    <Self as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn from_attr_value<'input: 'xml>(
        value: roxmltree::Attribute<'xml, 'input>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        TryFromEscapedStr::try_from_escaped_str(value.value())
            .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node_id))
    }
}

// FIXME Remove once we switch from `roxmltree`
impl<'xml> FromAttrValue<'xml> for UnescapedStr<'xml> {
    fn from_attr_value<'input: 'xml>(
        value: roxmltree::Attribute<'xml, 'input>,
        _attr: &'static str,
        _node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        Ok(Self::from(value.value_storage()))
    }
}

pub trait FromInterspersedAttrValue<'xml>: FromIterator<Self::Item> {
    type Item: TryFromEscapedStr<'xml>;

    fn from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: roxmltree::Attribute<'xml, 'input>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self>
    where
        <Self::Item as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
    {
        value
            .value()
            .split(S::SEP)
            .map(TryFromEscapedStr::try_from_escaped_str)
            .collect::<Result<_, _>>()
            .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node_id))
    }
}
impl<'xml, T: TryFromEscapedStr<'xml>> FromInterspersedAttrValue<'xml> for Vec<T> {
    type Item = T;
}
impl<'xml, T: enumflags2::BitFlag + TryFromEscapedStr<'xml>> FromInterspersedAttrValue<'xml>
    for enumflags2::BitFlags<T>
{
    type Item = T;
}

pub trait TryFromAttrValue<'xml>: Sized {
    fn try_from_attr_value<'input: 'xml>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self>;
}

impl<'xml, T: FromAttrValue<'xml>> TryFromAttrValue<'xml> for T
// where
//     <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_attr_value<'input: 'xml>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        FromAttrValue::from_attr_value(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr, node_id))?,
            attr,
            node_id,
        )
    }
}

impl<'xml, T: FromAttrValue<'xml>> TryFromAttrValue<'xml> for Option<T>
// where
//     <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_attr_value<'input: 'xml>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        value
            .map(|value| FromAttrValue::from_attr_value(value, attr, node_id))
            .transpose()
    }
}

pub trait TryFromInterspersedAttrValue<'xml>: Sized {
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromInterspersedAttrValue<'xml> for Vec<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        Self::from_interspersed_attr_value::<S>(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr, node_id))?,
            attr,
            node_id,
        )
    }
}
impl<'xml, T: enumflags2::BitFlag + TryFromEscapedStr<'xml>> TryFromInterspersedAttrValue<'xml>
    for enumflags2::BitFlags<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        Self::from_interspersed_attr_value::<S>(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr, node_id))?,
            attr,
            node_id,
        )
    }
}
impl<'xml, V: FromInterspersedAttrValue<'xml>> TryFromInterspersedAttrValue<'xml> for Option<V>
where
    <V::Item as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<roxmltree::Attribute<'xml, 'input>>,
        attr: &'static str,
        node_id: roxmltree::NodeId,
    ) -> ParseResult<Self> {
        value
            .map(|value| V::from_interspersed_attr_value::<S>(value, attr, node_id))
            .transpose()
    }
}

pub trait TryFromXML<'xml>: Sized {
    fn try_from_xml<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Option<Self>>;
    fn from_xml<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        match Self::try_from_xml(node) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(ErrorKind::NoMatch(node.id())),
            Err(e) => Err(e),
        }
    }
}

pub trait FromAttributes<'xml>: Sized {
    fn from_attributes<'input: 'xml>(
        node: Node<'xml, 'input>,
    ) -> ParseResult<Result<Self, &'static [&'static str]>>;
}

pub trait TryFromAttributes<'xml>: Sized {
    fn try_from_attributes<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self>;
}

impl<'xml, T: FromAttributes<'xml>> TryFromAttributes<'xml> for T {
    fn try_from_attributes<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        match T::from_attributes(node) {
            Ok(Ok(v)) => Ok(v),
            Ok(Err(attrs)) => Err(ErrorKind::MissingAttributes(attrs, node.id())),
            Err(e) => Err(e),
        }
    }
}

impl<'xml, T: FromAttributes<'xml>> TryFromAttributes<'xml> for Option<T> {
    fn try_from_attributes<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        T::from_attributes(node).map(Result::ok)
    }
}

pub trait TryFromTextContent<'xml>: Sized {
    fn try_from_text<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromTextContent<'xml> for T
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_text<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        let text = node
            .text()
            .ok_or(crate::ErrorKind::EmptyElement(node.id()))?;
        T::try_from_escaped_str(text)
            .map_err(|e| crate::ErrorKind::TextValueError(Box::new(e), node.id()))
    }
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromTextContent<'xml> for Option<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_text<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        node.text()
            .map(T::try_from_escaped_str)
            .transpose()
            .map_err(|e| ErrorKind::TextValueError(Box::new(e), node.id()))
    }
}

impl<'s, 'xml: 's> TryFromTextContent<'xml> for UnescapedStr<'s> {
    fn try_from_text<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        node.text_storage()
            .map(UnescapedStr::from)
            .ok_or(crate::ErrorKind::EmptyElement(node.id()))
    }
}

// Generic attribute getter with conv
pub(crate) fn try_attribute<'a, 'input, T: TryFromEscapedStr<'a>>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<Option<T>>
where
    T::Error: 'static + StdError,
{
    node.attribute(attr)
        .map(TryFromEscapedStr::try_from_escaped_str)
        .transpose()
        .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e), node.id()))
}

pub(crate) fn attribute<'a, 'input, T: TryFromEscapedStr<'a>>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<T>
where
    T::Error: 'static + StdError,
{
    try_attribute(node, attr)?.ok_or_else(|| ErrorKind::MissingAttribute(attr, node.id()))
}

pub fn parse_registry<'a, 'input: 'a>(
    document: &'a Document<'input>,
) -> Result<Registry<'a>, Error<'a, 'input>> {
    let root = document.root_element();
    Registry::from_xml(root).map_err(|kind| Error { kind, document })
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

pub trait ParseChildren<'node>: Sized {
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

// FIXME
impl<'node, T: crate::TryFromXML<'node> + crate::into_xml::IntoXMLElement> ParseChildren<'node>
    for T
{
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        it.next().map_or(
            Err(ErrorKind::MissingChildElement(T::TAG, it.parent_id)),
            T::from_xml,
        )
    }
}

impl<'node, T: crate::TryFromXML<'node>> ParseChildren<'node> for Option<T> {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        it.next_if_some(|item| T::try_from_xml(*item).transpose())
            .transpose()
    }
}

impl<'node, T: crate::TryFromXML<'node>> ParseChildren<'node> for Vec<T> {
    fn from_children<'input: 'node>(
        it: &mut PeekableChildrenElements<'node, 'input>,
    ) -> ParseResult<Self> {
        let mut out = Vec::new();
        while let Some(v) = it.next_if_some(|item| T::try_from_xml(*item).transpose()) {
            out.push(v?);
        }
        Ok(out)
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
