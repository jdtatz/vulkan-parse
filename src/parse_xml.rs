use std::{
    error::Error as StdError,
    fmt,
    iter::{Filter, Peekable},
    ops::{Deref, DerefMut},
};

pub use roxmltree::Document;
use roxmltree::{Node, NodeId, TextPos};

use crate::{LexerError, ParseError, Registry, Seperator, TryFromEscapedStr, UnescapedStr};

#[derive(Debug)]
pub enum ErrorKind {
    NoMatch,
    /// Empty elements are disallowed in vulkan's mixed pseudo-c/xml
    EmptyElement,
    MissingAttribute(&'static str),
    MissingAttributes(&'static [&'static str]),
    UnknownAttribute(String),
    MissingChildElement(&'static str),
    LexerError(LexerError),
    PegParsingError(ParseError),
    AttributeValueError(&'static str, Box<dyn StdError + 'static>),
    TextValueError(Box<dyn StdError + 'static>),
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    node: NodeId,
    start: usize,
    end: Option<usize>,
}

impl From<&'_ Node<'_, '_>> for Location {
    fn from(value: &Node) -> Self {
        let core::ops::Range { start, end } = value.range();
        Self {
            node: value.id(),
            start,
            end: Some(end),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    location: Location,
}

impl Error {
    pub fn new(kind: ErrorKind, node: &Node) -> Self {
        Self {
            kind,
            location: node.into(),
        }
    }
}

impl ErrorKind {
    pub fn with_location<I: Into<Location>>(self, location: I) -> Error {
        Error {
            kind: self,
            location: location.into(),
        }
    }
}

#[derive(Debug)]
pub struct DocumentError<'d, 'input> {
    error: Error,
    document: &'d Document<'input>,
}

impl<'d, 'input> DocumentError<'d, 'input> {
    pub fn span(&self) -> (TextPos, Option<TextPos>) {
        (
            self.document.text_pos_at(self.error.location.start),
            self.error
                .location
                .end
                .map(|b| self.document.text_pos_at(b)),
        )
    }
}

#[derive(Debug, Clone, Copy)]
struct DisplayDocumentLocation<'a, 'input>(&'a Document<'input>, Location);

impl<'a, 'input> fmt::Display for DisplayDocumentLocation<'a, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(document, Location { node: location, .. }) = *self;
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

impl<'d, 'input> fmt::Display for DocumentError<'d, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Error { kind, location } = &self.error;
        match &kind {
            ErrorKind::NoMatch => write!(
                f,
                "No Match found at {}",
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::EmptyElement => write!(
                f,
                "Empty elements are disallowed in vulkan's mixed pseudo-c/xml, at {}",
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::MissingAttribute(key) => write!(
                f,
                "Attribute {:?} not found in {}",
                key,
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::MissingAttributes(key) => write!(
                f,
                "Attribute(s) {:?} not found in {}",
                key,
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::UnknownAttribute(key) => write!(
                f,
                "Attribute {:?} was not expected in {}",
                key,
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::MissingChildElement(tag) => write!(
                f,
                "No child with the tag-name {:?} was found at {}",
                tag,
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::LexerError(e) => {
                let unk = &self
                    .document
                    .get_node(location.node)
                    .unwrap()
                    .text()
                    .unwrap()[e.span.start..e.span.end];
                writeln!(
                    f,
                    "Lexer encountered an unexpected token {:?} at {:?} in {}",
                    unk,
                    e.span,
                    DisplayDocumentLocation(self.document, *location),
                )
            }
            ErrorKind::PegParsingError(e) => write!(
                f,
                "Mixed Parsing Error at {}\n{}",
                DisplayDocumentLocation(self.document, *location),
                e
            ),
            ErrorKind::AttributeValueError(key, _) => write!(
                f,
                "Error encountered when parsing Attribute {:?} with value of {:?} in {}",
                key,
                self.document
                    .get_node(location.node)
                    .unwrap()
                    .attribute(*key)
                    .unwrap(),
                DisplayDocumentLocation(self.document, *location)
            ),
            ErrorKind::TextValueError(_) => write!(
                f,
                "Error encountered when parsing the text {:?} of {}",
                self.document
                    .get_node(location.node)
                    .unwrap()
                    .text()
                    .unwrap(),
                DisplayDocumentLocation(self.document, *location)
            ),
        }
    }
}

impl<'d, 'input> StdError for DocumentError<'d, 'input> {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match &self.error.kind {
            ErrorKind::LexerError(e) => Some(e),
            ErrorKind::PegParsingError(e) => Some(e),
            ErrorKind::AttributeValueError(_, e) => Some(&**e),
            ErrorKind::TextValueError(e) => Some(&**e),
            _ => None,
        }
    }
}

pub(crate) type ParseResult<T> = std::result::Result<T, Error>;

pub struct Attribute<'a, 'input> {
    inner: roxmltree::Attribute<'a, 'input>,
    node_id: NodeId,
}

impl<'a, 'input> Deref for Attribute<'a, 'input> {
    type Target = roxmltree::Attribute<'a, 'input>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl From<&'_ Attribute<'_, '_>> for Location {
    fn from(value: &Attribute) -> Self {
        Self {
            node: value.node_id,
            start: value.inner.position(),
            end: None,
        }
    }
}

pub trait FromAttrValue<'xml>: Sized {
    fn from_attr_value<'input: 'xml>(
        value: Attribute<'xml, 'input>,
        attr: &'static str,
    ) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> FromAttrValue<'xml> for T
where
    <Self as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn from_attr_value<'input: 'xml>(
        value: Attribute<'xml, 'input>,
        attr: &'static str,
    ) -> ParseResult<Self> {
        TryFromEscapedStr::try_from_escaped_str(value.value())
            .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e)).with_location(&value))
    }
}

// FIXME Remove once we switch from `roxmltree`
impl<'xml> FromAttrValue<'xml> for UnescapedStr<'xml> {
    fn from_attr_value<'input: 'xml>(
        value: Attribute<'xml, 'input>,
        _attr: &'static str,
    ) -> ParseResult<Self> {
        Ok(Self::from(value.value_storage()))
    }
}

pub trait FromInterspersedAttrValue<'xml>: FromIterator<Self::Item> {
    type Item: TryFromEscapedStr<'xml>;

    fn from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Attribute<'xml, 'input>,
        attr: &'static str,
    ) -> ParseResult<Self>
    where
        <Self::Item as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
    {
        value
            .value()
            .split(S::SEP)
            .map(TryFromEscapedStr::try_from_escaped_str)
            .collect::<Result<_, _>>()
            .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e)).with_location(&value))
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
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self>;
}

impl<'xml, T: FromAttrValue<'xml>> TryFromAttrValue<'xml> for T
// where
//     <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_attr_value<'input: 'xml>(
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self> {
        FromAttrValue::from_attr_value(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr).with_location(node_loc))?,
            attr,
        )
    }
}

impl<'xml, T: FromAttrValue<'xml>> TryFromAttrValue<'xml> for Option<T>
// where
//     <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_attr_value<'input: 'xml>(
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self> {
        value
            .map(|value| FromAttrValue::from_attr_value(value, attr))
            .transpose()
    }
}

pub trait TryFromInterspersedAttrValue<'xml>: Sized {
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromInterspersedAttrValue<'xml> for Vec<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self> {
        Self::from_interspersed_attr_value::<S>(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr).with_location(node_loc))?,
            attr,
        )
    }
}
impl<'xml, T: enumflags2::BitFlag + TryFromEscapedStr<'xml>> TryFromInterspersedAttrValue<'xml>
    for enumflags2::BitFlags<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self> {
        Self::from_interspersed_attr_value::<S>(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr).with_location(node_loc))?,
            attr,
        )
    }
}
impl<'xml, V: FromInterspersedAttrValue<'xml>> TryFromInterspersedAttrValue<'xml> for Option<V>
where
    <V::Item as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<'input: 'xml, S: Seperator>(
        value: Option<Attribute<'xml, 'input>>,
        attr: &'static str,
        node_loc: Location,
    ) -> ParseResult<Self> {
        value
            .map(|value| V::from_interspersed_attr_value::<S>(value, attr))
            .transpose()
    }
}

pub trait TryFromXML<'xml>: Sized {
    fn try_from_xml<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Option<Self>>;
    fn from_xml<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        match Self::try_from_xml(node) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(ErrorKind::NoMatch.with_location(&node)),
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
            Ok(Err(attrs)) => Err(ErrorKind::MissingAttributes(attrs).with_location(&node)),
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
            .ok_or(crate::ErrorKind::EmptyElement)
            .map_err(|kind| Error::new(kind, &node))?;
        T::try_from_escaped_str(text)
            .map_err(|e| Error::new(ErrorKind::TextValueError(Box::new(e)), &node))
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
            .map_err(|e| Error::new(ErrorKind::TextValueError(Box::new(e)), &node))
    }
}

impl<'s, 'xml: 's> TryFromTextContent<'xml> for UnescapedStr<'s> {
    fn try_from_text<'input: 'xml>(node: Node<'xml, 'input>) -> ParseResult<Self> {
        node.text_storage()
            .map(UnescapedStr::from)
            .ok_or(crate::ErrorKind::EmptyElement)
            .map_err(|kind| Error::new(kind, &node))
    }
}

// Generic attribute getter with conv
pub(crate) fn try_from_attribute<'a, 'input, T: TryFromAttrValue<'a>>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<T> {
    T::try_from_attr_value(
        node.attribute_node(attr).map(|inner| Attribute {
            inner,
            node_id: node.id(),
        }),
        attr,
        (&node).into(),
    )
}

pub(crate) fn try_from_interspersed_attr<
    'a,
    'input,
    S: Seperator,
    T: TryFromInterspersedAttrValue<'a>,
>(
    node: Node<'a, 'input>,
    attr: &'static str,
) -> ParseResult<T> {
    T::try_from_interspersed_attr_value::<S>(
        node.attribute_node(attr).map(|inner| Attribute {
            inner,
            node_id: node.id(),
        }),
        attr,
        (&node).into(),
    )
}

pub fn parse_registry<'a, 'input: 'a>(
    document: &'a Document<'input>,
) -> Result<Registry<'a>, DocumentError<'a, 'input>> {
    let root = document.root_element();
    Registry::from_xml(root).map_err(|error| DocumentError { error, document })
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
    pub parent_loc: Location,
}

impl<'a, 'input: 'a> From<Node<'a, 'input>> for PeekableChildrenElements<'a, 'input> {
    fn from(value: Node<'a, 'input>) -> Self {
        let filter_children: Filter<_, NodeFilterFn> = value.children().filter(node_is_element);
        Self {
            iter: filter_children.peekable(),
            parent_loc: Location::from(&value),
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

trait PeekableExt: Iterator {
    fn next_if_some<R>(&mut self, func: impl FnOnce(&Self::Item) -> Option<R>) -> Option<R>;
    fn take_while_some<R, F: FnMut(&Self::Item) -> Option<R>>(
        &mut self,
        func: F,
    ) -> PeekingTakeWhileSome<&mut Self, F> {
        PeekingTakeWhileSome { iter: self, func }
    }
}

impl<P: PeekableExt> PeekableExt for &mut P {
    fn next_if_some<R>(&mut self, func: impl FnOnce(&Self::Item) -> Option<R>) -> Option<R> {
        (*self).next_if_some(func)
    }
}

struct PeekingTakeWhileSome<I, F> {
    iter: I,
    func: F,
}
impl<I: PeekableExt, R, F: FnMut(&I::Item) -> Option<R>> Iterator for PeekingTakeWhileSome<I, F> {
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next_if_some(&mut self.func)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, self.iter.size_hint().1)
    }
}

impl<I: Iterator> PeekableExt for Peekable<I> {
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

pub trait FromXMLChildren<'xml>: Sized {
    fn from_children<'input: 'xml>(
        it: &mut PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Option<Self>>;
}

impl<'xml, T: TryFromXML<'xml>> FromXMLChildren<'xml> for T {
    fn from_children<'input: 'xml>(
        it: &mut PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Option<Self>> {
        it.next_if_some(|node| T::try_from_xml(*node).transpose())
            .transpose()
    }
}

pub trait TryFromXMLChildren<'xml>: Sized {
    fn try_from_children<'input: 'xml>(
        it: &mut PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Self>;
}

impl<'xml, T: crate::Tagged + FromXMLChildren<'xml>> TryFromXMLChildren<'xml> for T {
    fn try_from_children<'input: 'xml>(
        it: &mut PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Self> {
        Self::from_children(it)?
            .ok_or_else(|| ErrorKind::MissingChildElement(T::TAG).with_location(it.parent_loc))
    }
}

impl<'xml, T: FromXMLChildren<'xml>> TryFromXMLChildren<'xml> for Option<T> {
    fn try_from_children<'input: 'xml>(
        it: &mut PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Self> {
        T::from_children(it)
    }
}

impl<'xml, T: TryFromXML<'xml>> TryFromXMLChildren<'xml> for Vec<T> {
    fn try_from_children<'input: 'xml>(
        it: &mut PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Self> {
        it.take_while_some(|item| T::try_from_xml(*item).transpose())
            .collect()
    }
}

// // How does the conflict with the `FromXMLChildren` impl
// impl<'xml, C: IntoIterator + FromIterator<<C as IntoIterator>::Item>> TryFromXMLChildren<'xml> for C where C::Item: TryFromXML<'xml> {
//     fn try_from_children<'input: 'xml>(it: &mut PeekableChildrenElements<'xml, 'input>) -> ParseResult<Self> {
//         it.take_while_some(|item| C::Item::try_from_xml(*item).transpose()).collect()
//     }
// }
