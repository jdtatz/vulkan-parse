use std::{error::Error as StdError, fmt};

use itertools::PutBack;
use xmlparser::{ElementEnd, StrSpan};

use crate::{LexerError, Registry, Seperator, Span, TryFromEscapedStr};

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
    PegParsingError(peg::error::ExpectedSet),
    AttributeValueError(&'static str, Box<dyn StdError + 'static>),
    TextValueError(Box<dyn StdError + 'static>),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::NoMatch => write!(f, "No match found"),
            ErrorKind::EmptyElement => write!(
                f,
                "Empty elements are disallowed in vulkan's mixed pseudo-c/xml"
            ),
            ErrorKind::MissingAttribute(key) => write!(f, "Attribute {key:?} missing from element"),
            ErrorKind::MissingAttributes(keys) => {
                write!(f, "Attribute(s) {keys:?} missing from element")
            }
            ErrorKind::UnknownAttribute(key) => write!(f, "Attribute {key:?} was not expected"),
            ErrorKind::MissingChildElement(tag) => {
                write!(f, "No child with the tag-name {tag:?} was found")
            }
            ErrorKind::LexerError(_) => write!(f, "Lexer encountered an unexpected token"),
            ErrorKind::PegParsingError(expected) => {
                write!(f, "Mixed parsing error, expected {}", expected)
            }
            ErrorKind::AttributeValueError(key, _) => {
                write!(f, "Error encountered when parsing attribute {key:?}")
            }
            ErrorKind::TextValueError(_) => {
                write!(f, "Error encountered when parsing the text content")
            }
        }
    }
}

impl StdError for ErrorKind {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            ErrorKind::LexerError(e) => Some(e),
            ErrorKind::AttributeValueError(_, e) => Some(&**e),
            ErrorKind::TextValueError(e) => Some(&**e),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    // TODO: add xml path/node?
    // node: NodeId,
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub(crate) fn with_span(&self, span: Span) -> Self {
        debug_assert!(self.start <= span.start);
        debug_assert!(self.end >= span.end);
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Vulkan { kind: ErrorKind, location: Location },
    XML(xmlparser::Error),
}

impl ErrorKind {
    pub fn with_location<I: Into<Location>>(self, location: I) -> Error {
        Error::Vulkan {
            kind: self,
            location: location.into(),
        }
    }
}

impl From<xmlparser::Error> for Error {
    fn from(value: xmlparser::Error) -> Self {
        Error::XML(value)
    }
}

#[derive(Debug)]
pub struct DocumentError<'d> {
    error: Error,
    document: &'d str,
}

// impl<'d> DocumentError<'d> {
//     pub fn span(&self) -> (TextPos, Option<TextPos>) {
//         (
//             self.document.text_pos_at(self.error.location.start),
//             self.error
//                 .location
//                 .end
//                 .map(|b| self.document.text_pos_at(b)),
//         )
//     }
// }

#[derive(Debug, Clone, Copy)]
struct DisplayDocumentLocation<'a>(&'a str, Location);

impl<'a> fmt::Display for DisplayDocumentLocation<'a> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let Self(document, Location { node: location, .. }) = *self;
        // let node = document.get_node(location).unwrap();
        // let ancestors = node.ancestors().collect::<Vec<_>>();
        // for n in ancestors.into_iter().rev() {
        //     if n.is_root() {
        //         continue;
        //     }
        //     if n.is_text() {
        //         write!(f, "/{:?}", n.text().unwrap())?;
        //         break;
        //     }
        //     write!(f, "/{}", n.tag_name().name())?;
        //     let attrs = n.attributes();
        //     if attrs.len() > 0 {
        //         let mut is_first = true;
        //         write!(f, "[")?;
        //         for attr in attrs {
        //             if is_first {
        //                 is_first = false;
        //             } else {
        //                 write!(f, ", ")?;
        //             }
        //             write!(f, "{}=\"{}\"", attr.name(), attr.value())?;
        //         }
        //         write!(f, "]")?;
        //     }
        // }
        todo!();
    }
}

impl<'d> fmt::Display for DocumentError<'d> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.error {
            Error::Vulkan { kind, location } => write!(
                f,
                "{kind} : {:?} on line {}",
                &self.document[location.start..location.end],
                1 + self.document[..location.start]
                    .chars()
                    .filter(|c| *c == '\n')
                    .count()
            ),
            Error::XML(e) => write!(f, "{e}"),
        }
        // match &self.error {
        //     Error::Vulkan { kind, location } => match &kind {
        //         ErrorKind::NoMatch => write!(
        //             f,
        //             "No Match found at {}",
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::EmptyElement => write!(
        //             f,
        //             "Empty elements are disallowed in vulkan's mixed pseudo-c/xml, at {}",
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::MissingAttribute(key) => write!(
        //             f,
        //             "Attribute {:?} not found in {}",
        //             key,
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::MissingAttributes(key) => write!(
        //             f,
        //             "Attribute(s) {:?} not found in {}",
        //             key,
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::UnknownAttribute(key) => write!(
        //             f,
        //             "Attribute {:?} was not expected in {}",
        //             key,
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::MissingChildElement(tag) => write!(
        //             f,
        //             "No child with the tag-name {:?} was found at {}",
        //             tag,
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::LexerError(e) => {
        //             let unk = &self
        //                 .document
        //                 .get_node(location.node)
        //                 .unwrap()
        //                 .text()
        //                 .unwrap()[e.span.start..e.span.end];
        //             writeln!(
        //                 f,
        //                 "Lexer encountered an unexpected token {:?} at {:?} in {}",
        //                 unk,
        //                 e.span,
        //                 DisplayDocumentLocation(self.document, *location),
        //             )
        //         }
        //         ErrorKind::PegParsingError(e) => write!(
        //             f,
        //             "Mixed Parsing Error at {}\n{}",
        //             DisplayDocumentLocation(self.document, *location),
        //             e
        //         ),
        //         ErrorKind::AttributeValueError(key, _) => write!(
        //             f,
        //             "Error encountered when parsing Attribute {:?} with value of {:?} in {}",
        //             key,
        //             self.document
        //                 .get_node(location.node)
        //                 .unwrap()
        //                 .attribute(*key)
        //                 .unwrap(),
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //         ErrorKind::TextValueError(_) => write!(
        //             f,
        //             "Error encountered when parsing the text {:?} of {}",
        //             self.document
        //                 .get_node(location.node)
        //                 .unwrap()
        //                 .text()
        //                 .unwrap(),
        //             DisplayDocumentLocation(self.document, *location)
        //         ),
        //     },
        //     Error::XML(e) => write!(f, "{}", e),
        // }
    }
}

impl<'d> StdError for DocumentError<'d> {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match &self.error {
            Error::Vulkan { kind, .. } => kind.source(),
            Error::XML(e) => Some(e),
        }
    }
}

pub(crate) type ParseResult<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Attribute<'xml> {
    pub name: StrSpan<'xml>,
    pub value: StrSpan<'xml>,
    location: Location,
}

impl<'xml> TryFrom<XMLToken<'xml>> for Attribute<'xml> {
    type Error = ();

    fn try_from(value: XMLToken<'xml>) -> Result<Self, Self::Error> {
        match value {
            XMLToken::Attribute {
                prefix: _,
                local,
                value,
                span,
            } => Ok(Self {
                name: local,
                value,
                location: Location {
                    start: span.start(),
                    end: span.end(),
                },
            }),
            _ => Err(()),
        }
    }
}

impl From<&'_ Attribute<'_>> for Location {
    fn from(value: &Attribute) -> Self {
        value.location
    }
}

pub trait FromAttrValue<'xml>: Sized {
    fn from_attr_value(value: &Attribute<'xml>, attr: &'static str) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> FromAttrValue<'xml> for T
where
    <Self as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn from_attr_value(value: &Attribute<'xml>, attr: &'static str) -> ParseResult<Self> {
        TryFromEscapedStr::try_from_escaped_str(value.value.as_str())
            .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e)).with_location(value))
    }
}

pub trait FromInterspersedAttrValue<'xml>: FromIterator<Self::Item> {
    type Item: TryFromEscapedStr<'xml>;

    fn from_interspersed_attr_value<S: Seperator>(
        value: &Attribute<'xml>,
        attr: &'static str,
    ) -> ParseResult<Self>
    where
        <Self::Item as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
    {
        value
            .value
            .as_str()
            .split(S::SEP)
            .map(TryFromEscapedStr::try_from_escaped_str)
            .collect::<Result<_, _>>()
            .map_err(|e| ErrorKind::AttributeValueError(attr, Box::new(e)).with_location(value))
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
    fn try_from_attr_value(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        elem_loc: Location,
    ) -> ParseResult<Self>;
}

impl<'xml, T: FromAttrValue<'xml>> TryFromAttrValue<'xml> for T
// where
//     <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_attr_value(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        elem_loc: Location,
    ) -> ParseResult<Self> {
        FromAttrValue::from_attr_value(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr).with_location(elem_loc))?,
            attr,
        )
    }
}

impl<'xml, T: FromAttrValue<'xml>> TryFromAttrValue<'xml> for Option<T>
// where
//     <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_attr_value(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        _elem_loc: Location,
    ) -> ParseResult<Self> {
        value
            .map(|value| FromAttrValue::from_attr_value(value, attr))
            .transpose()
    }
}

pub trait TryFromInterspersedAttrValue<'xml>: Sized {
    fn try_from_interspersed_attr_value<S: Seperator>(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        elem_loc: Location,
    ) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromInterspersedAttrValue<'xml> for Vec<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<S: Seperator>(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        elem_loc: Location,
    ) -> ParseResult<Self> {
        Self::from_interspersed_attr_value::<S>(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr).with_location(elem_loc))?,
            attr,
        )
    }
}
impl<'xml, T: enumflags2::BitFlag + TryFromEscapedStr<'xml>> TryFromInterspersedAttrValue<'xml>
    for enumflags2::BitFlags<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<S: Seperator>(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        elem_loc: Location,
    ) -> ParseResult<Self> {
        Self::from_interspersed_attr_value::<S>(
            value.ok_or_else(|| ErrorKind::MissingAttribute(attr).with_location(elem_loc))?,
            attr,
        )
    }
}
impl<'xml, V: FromInterspersedAttrValue<'xml>> TryFromInterspersedAttrValue<'xml> for Option<V>
where
    <V::Item as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_interspersed_attr_value<S: Seperator>(
        value: Option<&Attribute<'xml>>,
        attr: &'static str,
        _elem_loc: Location,
    ) -> ParseResult<Self> {
        value
            .map(|value| V::from_interspersed_attr_value::<S>(value, attr))
            .transpose()
    }
}

pub enum XMLToken<'a> {
    ElementStart {
        prefix: StrSpan<'a>,
        local: StrSpan<'a>,
        span: StrSpan<'a>,
    },
    Attribute {
        prefix: StrSpan<'a>,
        local: StrSpan<'a>,
        value: StrSpan<'a>,
        span: StrSpan<'a>,
    },
    ElementEnd {
        end: ElementEnd<'a>,
        span: StrSpan<'a>,
    },
    Text {
        text: StrSpan<'a>,
    },
}

impl<'a> TryFrom<xmlparser::Token<'a>> for XMLToken<'a> {
    type Error = ();

    fn try_from(value: xmlparser::Token<'a>) -> Result<Self, Self::Error> {
        match value {
            xmlparser::Token::ElementStart {
                prefix,
                local,
                span,
            } => Ok(XMLToken::ElementStart {
                prefix,
                local,
                span,
            }),
            xmlparser::Token::Attribute {
                prefix,
                local,
                value,
                span,
            } => Ok(XMLToken::Attribute {
                prefix,
                local,
                value,
                span,
            }),
            xmlparser::Token::ElementEnd { end, span } => Ok(XMLToken::ElementEnd { end, span }),
            xmlparser::Token::Text { text } if !text.trim().is_empty() => {
                Ok(XMLToken::Text { text })
            }
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum XMLChild<'xml> {
    StartElement {
        name: StrSpan<'xml>,
        attributes: Vec<Attribute<'xml>>,
        span: Location,
    },
    EmptyElement {
        name: StrSpan<'xml>,
        attributes: Vec<Attribute<'xml>>,
        span: Location,
    },
    EndElement {
        name: StrSpan<'xml>,
        span: Location,
    },
    Text {
        text: StrSpan<'xml>,
    },
}

impl From<&'_ XMLChild<'_>> for Location {
    fn from(value: &'_ XMLChild<'_>) -> Self {
        match value {
            XMLChild::StartElement { span, .. } => *span,
            XMLChild::EmptyElement { span, .. } => *span,
            XMLChild::EndElement { span, .. } => *span,
            XMLChild::Text { text } => Location {
                start: text.start(),
                end: text.end(),
            },
        }
    }
}

pub struct XMLChildIter<'i, I: Iterator> {
    it: &'i mut I,
}

impl<'i, 'xml: 'i, I: Iterator<Item = ParseResult<XMLToken<'xml>>>> From<&'i mut I>
    for XMLChildIter<'i, I>
{
    fn from(value: &'i mut I) -> Self {
        Self { it: value }
    }
}

impl<'i, 'xml: 'i, I: Iterator<Item = ParseResult<XMLToken<'xml>>>> XMLChildIter<'i, I> {
    pub fn try_next(&mut self) -> ParseResult<Option<XMLChild<'xml>>> {
        Ok(match self.it.next().transpose()? {
            None => None,
            Some(XMLToken::Text { text }) => Some(XMLChild::Text { text }),
            Some(XMLToken::ElementStart {
                local: name,
                span: start_span,
                ..
            }) => {
                let mut attributes = Vec::new();
                loop {
                    match self.it.next().transpose()? {
                        None => return Ok(None),
                        Some(crate::XMLToken::Attribute {
                            prefix: _,
                            local,
                            value,
                            span,
                        }) => attributes.push(Attribute {
                            name: local,
                            value,
                            location: Location {
                                start: span.start(),
                                end: span.end(),
                            },
                        }),
                        Some(crate::XMLToken::ElementEnd {
                            end: xmlparser::ElementEnd::Empty,
                            span: end_span,
                        }) => {
                            return Ok(Some(XMLChild::EmptyElement {
                                name,
                                attributes,
                                span: Location {
                                    start: start_span.start(),
                                    end: end_span.end(),
                                },
                            }));
                        }
                        Some(crate::XMLToken::ElementEnd {
                            end: xmlparser::ElementEnd::Open,
                            span: end_span,
                        }) => {
                            return Ok(Some(XMLChild::StartElement {
                                name,
                                attributes,
                                span: Location {
                                    start: start_span.start(),
                                    end: end_span.end(),
                                },
                            }));
                        }
                        // invalid xml
                        _ => unreachable!(),
                    }
                }
            }
            Some(XMLToken::ElementEnd {
                end: xmlparser::ElementEnd::Close(_prefix, local),
                span,
            }) => {
                return Ok(Some(XMLChild::EndElement {
                    name: local,
                    span: Location {
                        start: span.start(),
                        end: span.end(),
                    },
                }));
            }
            // invalid xml
            _ => unreachable!(),
        })
    }
}

impl<'i, 'xml: 'i, I: Iterator<Item = ParseResult<XMLToken<'xml>>>> Iterator
    for XMLChildIter<'i, I>
{
    type Item = ParseResult<XMLChild<'xml>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_next().transpose()
    }
}

pub trait TryFromXML<'xml>: Sized {
    fn try_from_xml<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        tag: &'xml str,
        attributes: &[Attribute<'xml>],
        children: Option<I>,
        location: Location,
    ) -> ParseResult<Option<Self>>;
    fn from_xml<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        tag: &'xml str,
        attributes: &[Attribute<'xml>],
        children: Option<I>,
        location: Location,
    ) -> ParseResult<Self> {
        match Self::try_from_xml(tag, attributes, children, location) {
            Ok(Some(v)) => Ok(v),
            Ok(None) => Err(ErrorKind::NoMatch.with_location(location)),
            Err(e) => Err(e),
        }
    }
}

pub trait FromAttributes<'xml>: Sized {
    fn from_attributes(
        attributes: &[Attribute<'xml>],
        location: Location,
    ) -> ParseResult<Result<Self, &'static [&'static str]>>;
}

pub trait TryFromAttributes<'xml>: Sized {
    fn try_from_attributes(attributes: &[Attribute<'xml>], location: Location)
    -> ParseResult<Self>;
}

impl<'xml, T: FromAttributes<'xml>> TryFromAttributes<'xml> for T {
    fn try_from_attributes(
        attributes: &[Attribute<'xml>],
        location: Location,
    ) -> ParseResult<Self> {
        match T::from_attributes(attributes, location) {
            Ok(Ok(v)) => Ok(v),
            Ok(Err(attrs)) => Err(ErrorKind::MissingAttributes(attrs).with_location(location)),
            Err(e) => Err(e),
        }
    }
}

impl<'xml, T: FromAttributes<'xml>> TryFromAttributes<'xml> for Option<T> {
    fn try_from_attributes(
        attributes: &[Attribute<'xml>],
        location: Location,
    ) -> ParseResult<Self> {
        T::from_attributes(attributes, location).map(Result::ok)
    }
}

pub trait TryFromTextContent<'xml>: Sized {
    fn try_from_text<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: Option<I>,
        location: Location,
    ) -> ParseResult<Self>;
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromTextContent<'xml> for T
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_text<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: Option<I>,
        location: Location,
    ) -> ParseResult<Self> {
        if let Some(mut it) = it {
            if let Some(ref child @ XMLChild::Text { text }) = it.next().transpose()? {
                let child_location = Location::from(child);
                let res = T::try_from_escaped_str(text.as_str()).map_err(|e| {
                    ErrorKind::TextValueError(Box::new(e)).with_location(child_location)
                })?;
                if let Some(c) = it.next().transpose()? {
                    Err(ErrorKind::NoMatch.with_location(&c))
                } else {
                    Ok(res)
                }
            } else {
                todo!()
            }
        } else {
            Err(crate::ErrorKind::EmptyElement.with_location(location))
        }
    }
}

impl<'xml, T: TryFromEscapedStr<'xml>> TryFromTextContent<'xml> for Option<T>
where
    <T as TryFromEscapedStr<'xml>>::Error: 'static + StdError,
{
    fn try_from_text<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: Option<I>,
        _location: Location,
    ) -> ParseResult<Self> {
        if let Some(mut it) = it {
            if let Some(ref child @ XMLChild::Text { text }) = it.next().transpose()? {
                let child_location = Location::from(child);
                let res = T::try_from_escaped_str(text.as_str())
                    .map_err(|e| {
                        ErrorKind::TextValueError(Box::new(e)).with_location(child_location)
                    })
                    .map(Some)?;
                if let Some(c) = it.next().transpose()? {
                    Err(ErrorKind::NoMatch.with_location(&c))
                } else {
                    Ok(res)
                }
            } else {
                todo!()
            }
        } else {
            Ok(None)
        }
    }
}

// Generic attribute getter with conv
pub(crate) fn try_from_attribute<'xml, T: TryFromAttrValue<'xml>>(
    attributes: &[Attribute<'xml>],
    attr: &'static str,
    location: Location,
) -> ParseResult<T> {
    T::try_from_attr_value(
        attributes.iter().find(|a| a.name.as_str() == attr),
        attr,
        location,
    )
}

pub(crate) fn try_from_interspersed_attr<
    'xml,
    S: Seperator,
    T: TryFromInterspersedAttrValue<'xml>,
>(
    attributes: &[Attribute<'xml>],
    attr: &'static str,
    location: Location,
) -> ParseResult<T> {
    T::try_from_interspersed_attr_value::<S>(
        attributes.iter().find(|a| a.name.as_str() == attr),
        attr,
        location,
    )
}

pub fn parse_registry<'xml>(document: &'xml str) -> Result<Registry<'xml>, DocumentError<'xml>> {
    let mut tokenizer = xmlparser::Tokenizer::from(document);
    let Some(Ok(xmlparser::Token::Declaration { .. })) = tokenizer.next() else {
        unreachable!()
    };
    let mut it = tokenizer.into_iter().filter_map(|r| {
        r.map(|t| XMLToken::try_from(t).ok())
            .map_err(Error::XML)
            .transpose()
    });
    let mut it = itertools::put_back(XMLChildIter::from(&mut it));
    Registry::try_from_children(&mut it, Location {
        start: 0,
        end: document.len(),
    })
    .map_err(|error| DocumentError { error, document })
}

struct ElemChildrenIter<'i, 'xml, I> {
    tag: &'xml str,
    depth: usize,
    it: &'i mut I,
}

impl<'i, 'xml: 'i, I: Iterator<Item = ParseResult<XMLChild<'xml>>>> ElemChildrenIter<'i, 'xml, I> {
    fn new(tag: &'xml str, it: &'i mut I) -> Self {
        Self { tag, depth: 0, it }
    }

    fn try_next(&mut self) -> ParseResult<Option<XMLChild<'xml>>> {
        match self.it.next().transpose()? {
            Some(XMLChild::StartElement {
                name,
                attributes,
                span,
            }) if name.as_str() == self.tag => {
                self.depth += 1;
                Ok(Some(XMLChild::StartElement {
                    name,
                    attributes,
                    span,
                }))
            }
            Some(XMLChild::EndElement { name, span })
                if name.as_str() == self.tag && self.depth > 0 =>
            {
                self.depth -= 1;
                Ok(Some(XMLChild::EndElement { name, span }))
            }
            Some(XMLChild::EndElement { name, .. })
                if name.as_str() == self.tag && self.depth == 0 =>
            {
                Ok(None)
            }
            v => Ok(v),
        }
    }
}

impl<'i, 'xml: 'i, I: Iterator<Item = ParseResult<XMLChild<'xml>>>> Iterator
    for ElemChildrenIter<'i, 'xml, I>
{
    type Item = ParseResult<XMLChild<'xml>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_next().transpose()
    }
}

pub trait FromXMLChildren<'xml>: Sized {
    fn from_children<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: &mut PutBack<I>,
    ) -> ParseResult<Option<Self>>;
}

impl<'xml, T: TryFromXML<'xml>> FromXMLChildren<'xml> for T {
    fn from_children<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: &mut PutBack<I>,
    ) -> ParseResult<Option<Self>> {
        match it.next().transpose()? {
            Some(XMLChild::StartElement {
                name,
                attributes,
                span,
            }) => {
                if let Some(res) = T::try_from_xml(
                    name.as_str(),
                    &attributes,
                    Some(ElemChildrenIter::new(name.as_str(), it).fuse()),
                    span,
                )? {
                    Ok(Some(res))
                } else {
                    it.put_back(Ok(XMLChild::StartElement {
                        name,
                        attributes,
                        span,
                    }));
                    Ok(None)
                }
            }
            Some(XMLChild::EmptyElement {
                name,
                attributes,
                span,
            }) => {
                if let Some(res) = T::try_from_xml(name.as_str(), &attributes, None::<I>, span)? {
                    Ok(Some(res))
                } else {
                    it.put_back(Ok(XMLChild::EmptyElement {
                        name,
                        attributes,
                        span,
                    }));
                    Ok(None)
                }
            }
            Some(v) => {
                it.put_back(Ok(v));
                Ok(None)
            }
            None => Ok(None),
        }
    }
}

pub trait TryFromXMLChildren<'xml>: Sized {
    fn try_from_children<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: &mut PutBack<I>,
        parent_loc: Location,
    ) -> ParseResult<Self>;
}

impl<'xml, T: crate::Tagged + FromXMLChildren<'xml>> TryFromXMLChildren<'xml> for T {
    fn try_from_children<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: &mut PutBack<I>,
        parent_loc: Location,
    ) -> ParseResult<Self> {
        Self::from_children(it)?
            .ok_or_else(|| ErrorKind::MissingChildElement(T::TAG).with_location(parent_loc))
    }
}

impl<'xml, T: FromXMLChildren<'xml>> TryFromXMLChildren<'xml> for Option<T> {
    fn try_from_children<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: &mut PutBack<I>,
        _parent_loc: Location,
    ) -> ParseResult<Self> {
        T::from_children(it)
    }
}

impl<'xml, T: TryFromXML<'xml>> TryFromXMLChildren<'xml> for Vec<T> {
    fn try_from_children<I: Iterator<Item = ParseResult<XMLChild<'xml>>>>(
        it: &mut PutBack<I>,
        _parent_loc: Location,
    ) -> ParseResult<Self> {
        let mut out = Vec::new();
        while let Some(v) = T::from_children(it)? {
            out.push(v)
        }
        Ok(out)
    }
}
