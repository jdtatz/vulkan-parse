use std::{
    borrow::Cow,
    fmt,
    num::ParseIntError,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use roxmltree::Node;
use serde::Serialize;

use crate::{attribute, try_attribute, Parse, ParseElements, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Comment<'a>(pub Cow<'a, str>);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum MaybeComment<'a, T> {
    Value(T),
    Comment(Comment<'a>),
}

impl<'a, T> MaybeComment<'a, T> {
    pub fn try_into_value(self) -> Option<T> {
        match self {
            MaybeComment::Value(v) => Some(v),
            MaybeComment::Comment(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CommentendChildren<'a, T>(pub Box<[MaybeComment<'a, T>]>);

impl<'a, T: 'a> CommentendChildren<'a, T> {
    pub fn into_values(self) -> impl 'a + Iterator<Item = T> {
        Vec::from(self.0)
            .into_iter()
            .filter_map(MaybeComment::try_into_value)
    }
}

impl<'a, T> Deref for CommentendChildren<'a, T> {
    type Target = Box<[MaybeComment<'a, T>]>;

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum DefinitionOrAlias<'a, T> {
    Alias {
        name: Cow<'a, str>,
        alias: Cow<'a, str>,
        comment: Option<Cow<'a, str>>,
    },
    Definition(T),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
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
            write!(f, "{}.{}.{}", self.major, self.minor, patch)
        } else {
            write!(f, "{}.{}", self.major, self.minor)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
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

impl<'a, 'input, T: Parse<'a, 'input>> Parse<'a, 'input> for MaybeComment<'a, T> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(v) = T::try_parse(node)? {
            Ok(Some(MaybeComment::Value(v)))
        } else {
            Ok(Comment::try_parse(node)?.map(MaybeComment::Comment))
        }
    }
}

impl<'a, 'input: 'a, T: 'a + Parse<'a, 'input>> ParseElements<'a, 'input>
    for CommentendChildren<'a, T>
{
    type Item = MaybeComment<'a, T>;

    type NodeIter = roxmltree::Children<'a, 'input>;

    fn get_nodes(node: Node<'a, 'input>) -> ParseResult<Option<Self::NodeIter>> {
        Ok(Some(node.children()))
    }
}

impl<'a, 'input> Parse<'a, 'input> for Comment<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("comment") {
            Ok(Some(Comment(Cow::Borrowed(node.text().unwrap_or("")))))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input, T: Parse<'a, 'input>> Parse<'a, 'input> for DefinitionOrAlias<'a, T> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(alias) = try_attribute(node, "alias")? {
            Ok(Some(DefinitionOrAlias::Alias {
                name: attribute(node, "name")?,
                alias,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(T::try_parse(node)?.map(DefinitionOrAlias::Definition))
        }
    }
}
