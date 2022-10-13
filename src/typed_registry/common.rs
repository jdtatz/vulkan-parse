use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use roxmltree::Node;

use crate::{get_req_attr, Parse, ParseElements, ParseResult};

#[derive(Debug, Clone)]
pub struct Comment<'a>(pub Cow<'a, str>);

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum DefinitionOrAlias<'a, T> {
    Alias {
        name: Cow<'a, str>,
        alias: Cow<'a, str>,
        comment: Option<Cow<'a, str>>,
    },
    Definition(T),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemVarVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: Option<u32>,
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
        if let Some(alias) = node.attribute("alias") {
            Ok(Some(DefinitionOrAlias::Alias {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                alias: Cow::Borrowed(alias),
                comment: node.attribute("comment").map(Cow::Borrowed),
            }))
        } else {
            Ok(T::try_parse(node)?.map(DefinitionOrAlias::Definition))
        }
    }
}
