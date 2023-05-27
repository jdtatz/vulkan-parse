use std::{
    fmt,
    num::ParseIntError,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use crate::{ParseResult, UnescapedStr, VulkanApi};

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "comment")]
pub struct Comment<'a> {
    #[vkxml(text)]
    pub comment: UnescapedStr<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum MaybeComment<'a, T> {
    Comment(Comment<'a>),
    Value(T),
}

impl<'a, T> MaybeComment<'a, T> {
    pub fn try_into_value(self) -> Option<T> {
        match self {
            MaybeComment::Value(v) => Some(v),
            MaybeComment::Comment(_) => None,
        }
    }

    pub fn try_as_value(&self) -> Option<&T> {
        match self {
            MaybeComment::Value(v) => Some(v),
            MaybeComment::Comment(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CommentendChildren<'a, T>(pub Vec<MaybeComment<'a, T>>);

impl<'a, T: 'a> CommentendChildren<'a, T> {
    pub fn into_values(self) -> impl 'a + Iterator<Item = T> {
        self.0.into_iter().filter_map(MaybeComment::try_into_value)
    }

    pub fn values(&self) -> impl '_ + Iterator<Item = &'_ T> {
        self.0.iter().filter_map(MaybeComment::try_as_value)
    }
}

impl<'a, T> Deref for CommentendChildren<'a, T> {
    type Target = Vec<MaybeComment<'a, T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for CommentendChildren<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, T: 'a> FromIterator<MaybeComment<'a, T>> for CommentendChildren<'a, T> {
    fn from_iter<I: IntoIterator<Item = MaybeComment<'a, T>>>(iter: I) -> Self {
        CommentendChildren(iter.into_iter().collect())
    }
}

impl<'xml, T> crate::TryFromXMLChildren<'xml> for CommentendChildren<'xml, T>
where
    Vec<MaybeComment<'xml, T>>: crate::TryFromXMLChildren<'xml>,
{
    fn try_from_children<'input: 'xml>(
        it: &mut crate::PeekableChildrenElements<'xml, 'input>,
    ) -> ParseResult<Self> {
        Vec::try_from_children(it).map(Self)
    }
}

impl<'xml, T: crate::IntoXML> crate::IntoXMLChildren for CommentendChildren<'xml, T> {
    fn write_children<W: ?Sized + crate::XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.0.write_children(writer)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum AliasDeprecationKind {
    /// deprecated="aliased"
    #[strum(serialize = "aliased")]
    Aliased,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Alias<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub alias: &'a str,
    #[vkxml(attribute)]
    pub api: Option<VulkanApi>,
    #[vkxml(attribute)]
    pub deprecated: Option<AliasDeprecationKind>,
    #[vkxml(attribute)]
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = T::TAG)]
pub enum DefinitionOrAlias<'a, T: crate::Tagged> {
    #[vkxml(discriminant = "alias")]
    Alias(Alias<'a>),
    Definition(T),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct SemVarVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: Option<u32>,
}

impl FromStr for SemVarVersion {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((major_s, s)) = s.split_once('.') {
            if let Some((minor_s, patch_s)) = s.split_once('.') {
                Ok(Self {
                    major: major_s.parse()?,
                    minor: minor_s.parse()?,
                    patch: Some(patch_s.parse()?),
                })
            } else {
                Ok(Self {
                    major: major_s.parse()?,
                    minor: s.parse()?,
                    patch: None,
                })
            }
        } else {
            Ok(Self {
                major: s.parse()?,
                minor: 0,
                patch: None,
            })
        }
    }
}

impl fmt::Display for SemVarVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(patch) = self.patch {
            write!(f, "{}.{}.{patch}", self.major, self.minor)
        } else {
            write!(f, "{}.{}", self.major, self.minor)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct StdVersion {
    pub major: u32,
    pub minor: u32,
}

#[derive(Debug, Clone)]
pub enum StdVersionParseError {
    IntParseError(ParseIntError),
    MissingPrefix,
    MissingSeperator,
}

impl From<ParseIntError> for StdVersionParseError {
    fn from(e: ParseIntError) -> Self {
        Self::IntParseError(e)
    }
}

impl fmt::Display for StdVersionParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StdVersionParseError::IntParseError(_) => {
                write!(f, "Error while parsing major/minor version number")
            }
            StdVersionParseError::MissingPrefix => {
                write!(f, "Missing \"VK_VERSION_\" or \"VK_API_VERSION_\" prefix")
            }
            StdVersionParseError::MissingSeperator => write!(
                f,
                "Missing '_' seperator between major and minor version numbers"
            ),
        }
    }
}

impl std::error::Error for StdVersionParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            StdVersionParseError::IntParseError(e) => Some(e),
            _ => None,
        }
    }
}

impl FromStr for StdVersion {
    type Err = StdVersionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ver = s
            .strip_prefix("VK_VERSION_")
            .or_else(|| s.strip_prefix("VK_API_VERSION_"))
            .ok_or(StdVersionParseError::MissingPrefix)?;
        let (major, minor) = ver
            .split_once('_')
            .ok_or(StdVersionParseError::MissingSeperator)?;
        Ok(StdVersion {
            major: major.parse()?,
            minor: minor.parse()?,
        })
    }
}

impl fmt::Display for StdVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "VK_VERSION_{}_{}", self.major, self.minor)
    }
}
