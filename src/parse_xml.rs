use core::{fmt::Write, marker::PhantomData};
use std::{
    borrow::Cow,
    error::Error as StdError,
    fmt,
    iter::{Filter, Peekable},
    ops::{Deref, DerefMut},
    str::FromStr,
};

pub use roxmltree::Document;
use roxmltree::{Node, NodeId};

use crate::{LexerError, ParseError, Registry};

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

pub trait FromEscapedStr<'s>: Sized {
    fn from_escaped_str(s: &'s str) -> Self;
}

pub trait TryFromEscapedStr<'s>: Sized {
    type Error;
    // type Error: 'static + StdError;

    fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error>;
}

impl<'s, T: FromEscapedStr<'s>> TryFromEscapedStr<'s> for T {
    type Error = core::convert::Infallible;

    fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error> {
        Ok(Self::from_escaped_str(s))
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct UnescapedStr<'s>(pub(crate) Cow<'s, str>);

impl<'s> UnescapedStr<'s> {
    #[must_use]
    pub fn as_str(&self) -> &str {
        match &self.0 {
            Cow::Borrowed(s) => s,
            Cow::Owned(s) => s,
        }
    }
}

impl<'s> Default for UnescapedStr<'s> {
    fn default() -> Self {
        Self(Cow::Borrowed(""))
    }
}

impl<'s> From<&'s str> for UnescapedStr<'s> {
    fn from(value: &'s str) -> Self {
        Self(Cow::Borrowed(value))
    }
}

// // FIXME impl once we switch from `roxmltree`
// impl<'s> FromEscapedStr<'s> for UnescapedStr<'s> {
//     fn from_escaped_str(s: &'s str) -> Self {
//         if s.contains('&') {
//             // Self(Cow::Owned(
//             //     s.replace("&quot;", "\"")
//             //         .replace("&apos;", "'")
//             //         .replace("&lt;", "<")
//             //         .replace("&gt;", ">")
//             //         .replace("&amp;", "&"),
//             // ))
//             let mut unescaped = String::with_capacity(s.len());
//             for v in s.split('&') {
//                 if let Some(v) = v.strip_prefix("quot;") {
//                     unescaped.push('"');
//                     unescaped.push_str(v)
//                 } else if let Some(v) = v.strip_prefix("apos;") {
//                     unescaped.push('\'');
//                     unescaped.push_str(v)
//                 }else if let Some(v) = v.strip_prefix("lt;") {
//                     unescaped.push('<');
//                     unescaped.push_str(v)
//                 }else if let Some(v) = v.strip_prefix("gt;") {
//                     unescaped.push('>');
//                     unescaped.push_str(v)
//                 }else if let Some(v) = v.strip_prefix("amp;") {
//                     unescaped.push('&');
//                     unescaped.push_str(v)
//                 } else {
//                     unescaped.push_str(v)
//                 }
//             }
//             Self(Cow::Owned(unescaped))
//         } else {
//             Self(Cow::Borrowed(s))
//         }
//     }
// }

// FIXME remove once we switch from `roxmltree`
impl<'s, 'input: 's> From<&'s roxmltree::StringStorage<'input>> for UnescapedStr<'input> {
    fn from(value: &'s roxmltree::StringStorage<'input>) -> Self {
        match value {
            roxmltree::StringStorage::Borrowed(s) => Self(Cow::Borrowed(*s)),
            roxmltree::StringStorage::Owned(s) => Self(Cow::Owned(String::from(&**s))),
        }
    }
}

impl<'s> fmt::Display for UnescapedStr<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// FIXME: dangerous
impl<'s> FromEscapedStr<'s> for &'s str {
    fn from_escaped_str(s: &'s str) -> Self {
        s
    }
}

macro_rules! impl_try_from_esc_str {
    ($($t:ty),* $(,)?) => {
        $(
            impl<'s> TryFromEscapedStr<'s> for $t {
                type Error = <Self as FromStr>::Err;

                fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error> {
                    Self::from_str(s)
                }
            }

        )*
    }
}

impl_try_from_esc_str! {
    bool,
    u8,
    u32,
    i32,
    usize,
    std::num::NonZeroU8,
    std::num::NonZeroU32,
    std::num::NonZeroUsize,
}

pub trait Seperator {
    // TODO: Switch to `Pattern` once it's stabilized
    // type Value: fmt::Display + core::str::pattern::Pattern<'static>;
    // const SEP: Self::Value;
    const SEP: char;
}

pub struct CommaSeperator;
impl Seperator for CommaSeperator {
    const SEP: char = ',';
}

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

pub trait DisplayEscaped: fmt::Display {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f)
    }
}

impl<T: ?Sized + DisplayEscaped> DisplayEscaped for &T {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).escaped_fmt(f)
    }
}
impl<T: ?Sized + DisplayEscaped> DisplayEscaped for &mut T {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).escaped_fmt(f)
    }
}

#[repr(transparent)]
struct UnescapedWriter<'w, W: fmt::Write>(&'w mut W);

impl<'w, W: fmt::Write> fmt::Write for UnescapedWriter<'w, W> {
    //TODO: can this be done more efficiently using `str.split_inclusive` or something else?
    // The offical `EscapeUnicode`, `EscapeDefault`, & `EscapeDebug` use this method though
    fn write_str(&mut self, s: &str) -> fmt::Result {
        s.chars().try_for_each(|c| self.write_char(c))
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        match c {
            '"' => self.0.write_str("&quot;"),
            '\'' => self.0.write_str("&apos;"),
            '<' => self.0.write_str("&lt;"),
            '>' => self.0.write_str("&gt;"),
            '&' => self.0.write_str("&amp;"),
            c => self.0.write_char(c),
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Unescaped<V>(pub V);

impl<V: fmt::Display> fmt::Display for Unescaped<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<V: fmt::Display> DisplayEscaped for Unescaped<V> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(UnescapedWriter(f), "{}", self.0)
    }
}

impl<'s> DisplayEscaped for UnescapedStr<'s> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Cow::Borrowed(s) => write!(f, "{s}"),
            Cow::Owned(s) => write!(UnescapedWriter(f), "{s}"),
        }
    }
}

impl DisplayEscaped for bool {}
impl DisplayEscaped for u8 {}
impl DisplayEscaped for u32 {}
impl DisplayEscaped for i32 {}
impl DisplayEscaped for usize {}
impl DisplayEscaped for std::num::NonZeroU8 {}
impl DisplayEscaped for std::num::NonZeroU32 {}
impl DisplayEscaped for std::num::NonZeroUsize {}

impl DisplayEscaped for str {}
impl<'xml> DisplayEscaped for fmt::Arguments<'xml> {}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub(crate) struct Escaped<'v, V: ?Sized>(pub &'v V);

impl<'v, V: ?Sized + DisplayEscaped> fmt::Display for Escaped<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (&self.0).escaped_fmt(f)
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub(crate) struct InterspersedDisplay<'v, S, V: ?Sized>(&'v V, PhantomData<S>);

impl<'v, S, V: ?Sized> InterspersedDisplay<'v, S, V> {
    pub fn new(v: &'v V) -> Self {
        InterspersedDisplay(v, PhantomData)
    }
}

impl<'v, S: Seperator, V: ?Sized + crate::Iterable> fmt::Display for InterspersedDisplay<'v, S, V>
where
    <<V as crate::Iterable>::IT<'v> as IntoIterator>::Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut is_first = true;
        for v in crate::Iterable::iter(self.0) {
            if is_first {
                is_first = false;
            } else {
                f.write_char(S::SEP)?;
            }
            v.fmt(f)?;
        }
        Ok(())
    }
}

impl<'v, S: Seperator, V: ?Sized + crate::Iterable> DisplayEscaped for InterspersedDisplay<'v, S, V>
where
    <<V as crate::Iterable>::IT<'v> as IntoIterator>::Item: DisplayEscaped,
{
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut is_first = true;
        for v in crate::Iterable::iter(self.0) {
            if is_first {
                is_first = false;
            } else {
                f.write_char(S::SEP)?;
            }
            v.escaped_fmt(f)?;
        }
        Ok(())
    }
}

pub trait MaybeIntoAttrValue {
    type Value: ?Sized;

    fn maybe_into_value(&self) -> Option<&Self::Value>;
}

impl<T: ?Sized + DisplayEscaped> MaybeIntoAttrValue for T {
    type Value = Self;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        Some(self)
    }
}
impl<T> MaybeIntoAttrValue for Vec<T> {
    type Value = Self;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        Some(self)
    }
}
impl<T: enumflags2::BitFlag> MaybeIntoAttrValue for enumflags2::BitFlags<T> {
    type Value = Self;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        Some(self)
    }
}

impl<T> MaybeIntoAttrValue for Option<T> {
    type Value = T;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        self.as_ref()
    }
}

pub trait XMLWriter {
    type Error;

    fn write_escaped<V: ?Sized + DisplayEscaped>(&mut self, escaped: &V)
    -> Result<(), Self::Error>;
    fn write_element_start(&mut self, prefix: Option<&str>, name: &str) -> Result<(), Self::Error> {
        if let Some(prefix) = prefix {
            self.write_escaped(&format_args!("<{prefix}:{name}"))
        } else {
            self.write_escaped(&format_args!("<{name}"))
        }
    }
    fn write_attribute<V: ?Sized + DisplayEscaped>(
        &mut self,
        prefix: Option<&str>,
        name: &str,
        value: &V,
    ) -> Result<(), Self::Error> {
        let value = Escaped(value);
        if let Some(prefix) = prefix {
            self.write_escaped(&format_args!(" {prefix}:{name}=\"{value}\""))
        } else {
            self.write_escaped(&format_args!(" {name}=\"{value}\""))
        }
    }
    fn write_element_open_end(&mut self) -> Result<(), Self::Error> {
        self.write_escaped(">")
    }
    fn write_element_empty_end(&mut self) -> Result<(), Self::Error> {
        self.write_escaped("/>")
    }
    fn write_element_close_end(
        &mut self,
        prefix: Option<&str>,
        name: &str,
    ) -> Result<(), Self::Error> {
        if let Some(prefix) = prefix {
            self.write_escaped(&format_args!("</{prefix}:{name}>"))
        } else {
            self.write_escaped(&format_args!("</{name}>"))
        }
    }
    fn write_declaration(
        &mut self,
        version: &str,
        encoding: Option<&str>,
        standalone: Option<&str>,
    ) -> Result<(), Self::Error> {
        match (encoding, standalone) {
            (None, None) => self.write_escaped(&format_args!("<?xml version=\"{version}\"?>")),
            (None, Some(standalone)) => self.write_escaped(&format_args!(
                "<?xml version=\"{version}\" standalone=\"{standalone}\"?>"
            )),
            (Some(encoding), None) => self.write_escaped(&format_args!(
                "<?xml version=\"{version}\" encoding=\"{encoding}\"?>"
            )),
            (Some(encoding), Some(standalone)) => self.write_escaped(&format_args!(
                "<?xml version=\"{version}\" encoding=\"{encoding}\" standalone=\"{standalone}\"?>"
            )),
        }
    }
}

pub trait IntoXMLChildren {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error>;
}

impl<T: IntoXML> IntoXMLChildren for T {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.write_xml(writer)
    }
}

impl<T: IntoXML> IntoXMLChildren for Option<T> {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.as_ref().map_or(Ok(()), |v| v.write_xml(writer))
    }
}

impl<T: IntoXML> IntoXMLChildren for [T] {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.iter().try_for_each(|v| v.write_xml(writer))
    }
}

impl<T: IntoXML> IntoXMLChildren for Vec<T> {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.iter().try_for_each(|v| v.write_xml(writer))
    }
}

#[impl_trait_for_tuples::impl_for_tuples(4)]
impl IntoXMLChildren for Tuple {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        for_tuples!( #( Tuple.write_children(writer)?; )* );
        Ok(())
    }
}

pub struct XMLElementBuilder<'t, 'w, W: 'w + ?Sized + XMLWriter> {
    tag_name: &'t str,
    writer: &'w mut W,
}

impl<'t, 'w, W: 'w + ?Sized + XMLWriter> XMLElementBuilder<'t, 'w, W> {
    pub fn new(writer: &'w mut W, name: &'t str) -> Result<Self, W::Error> {
        writer.write_element_start(None, name)?;
        Ok(Self {
            tag_name: name,
            writer,
        })
    }
    pub fn with_attribute<V: ?Sized + DisplayEscaped>(
        self,
        name: &str,
        value: &V,
    ) -> Result<Self, W::Error> {
        self.writer.write_attribute(None, name, value)?;
        Ok(self)
    }

    pub fn write_empty(self) -> Result<(), W::Error> {
        self.writer.write_element_empty_end()
    }

    pub fn write_inner_content<F>(self, closure: F) -> Result<(), W::Error>
    where
        F: FnOnce(&mut W) -> Result<(), W::Error>,
    {
        self.writer.write_element_open_end()?;
        closure(self.writer)?;
        self.writer.write_element_close_end(None, self.tag_name)
    }

    // default methods
    pub fn write_escaped_text<V: ?Sized + DisplayEscaped>(
        self,
        escaped: &V,
    ) -> Result<(), W::Error> {
        self.write_inner_content(|w| w.write_escaped(escaped))
    }

    pub fn with_escaped_attribute<V: ?Sized + MaybeIntoAttrValue>(
        self,
        attr_key: &str,
        maybe_attr_value: &V,
    ) -> Result<Self, W::Error>
    where
        V::Value: DisplayEscaped,
    {
        if let Some(attr_value) = maybe_attr_value.maybe_into_value() {
            self.with_attribute(attr_key, attr_value)
        } else {
            Ok(self)
        }
    }
    pub fn with_interspersed_attribute<'v, S: Seperator, V: MaybeIntoAttrValue>(
        self,
        attr_key: &str,
        maybe_attr_value: &'v V,
    ) -> Result<Self, W::Error>
    where
        V::Value: crate::Iterable,
        <<V::Value as crate::Iterable>::IT<'v> as IntoIterator>::Item: DisplayEscaped,
    {
        if let Some(attr_value) = maybe_attr_value.maybe_into_value() {
            self.with_attribute(attr_key, &InterspersedDisplay::<S, _>::new(attr_value))
        } else {
            Ok(self)
        }
    }
    pub fn with_escaped_attributes<V: ?Sized + IntoXMLAttributes>(
        self,
        attributes: &V,
    ) -> Result<Self, W::Error> {
        let element = self;
        attributes.write_attributes(element)
    }

    pub fn write_children<I: IntoXMLChildren>(self, children: &I) -> Result<(), W::Error> {
        self.write_inner_content(move |writer| children.write_children(writer))
    }

    pub fn write_tokens<'tok, T: crate::IntoVkXMLTokens<'tok>>(
        self,
        value: T,
    ) -> Result<(), W::Error> {
        self.write_inner_content(move |writer| {
            let mut last_ident_like = false;
            for token in value.to_tokens_vector() {
                match token {
                    crate::VkXMLToken::C(token) => {
                        let is_ident_like = token.is_ident_like();
                        if last_ident_like && is_ident_like {
                            writer.write_escaped(" ")?;
                        };
                        writer.write_escaped(&token)?;
                        last_ident_like = is_ident_like;
                    }
                    crate::VkXMLToken::TextTag { name, text } => {
                        last_ident_like = false;
                        XMLElementBuilder::new(writer, name)?.write_escaped_text(&text)?;
                    }
                }
            }
            Ok(())
        })
    }
}

pub(crate) trait IntoXML {
    fn write_xml<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error>;
}

pub(crate) trait IntoXMLElement {
    const TAG: &'static str;

    fn add_static_attrs<'w, W: ?Sized + XMLWriter>(
        element: XMLElementBuilder<'static, 'w, W>,
    ) -> Result<XMLElementBuilder<'static, 'w, W>, W::Error> {
        Ok(element)
    }

    fn write_element<'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'static, 'w, W>,
    ) -> Result<(), W::Error>;

    fn write_element_and_static_attrs<'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'static, 'w, W>,
    ) -> Result<(), W::Error> {
        self.write_element(Self::add_static_attrs(element)?)
    }
}

impl<T: IntoXMLElement> IntoXML for T {
    fn write_xml<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.write_element(Self::add_static_attrs(XMLElementBuilder::new(
            writer,
            Self::TAG,
        )?)?)
    }
}

pub trait IntoXMLAttributes {
    fn write_attributes<'t, 'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error>;
}

impl<'a, T: ?Sized + IntoXMLAttributes> IntoXMLAttributes for &'a T {
    fn write_attributes<'t, 'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error> {
        (**self).write_attributes(element)
    }
}

impl<T: IntoXMLAttributes> IntoXMLAttributes for Option<T> {
    fn write_attributes<'t, 'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error> {
        if let Some(v) = self {
            v.write_attributes(element)
        } else {
            Ok(element)
        }
    }
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

impl<'node, T: crate::TryFromXML<'node> + crate::IntoXMLElement> ParseChildren<'node> for T {
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
