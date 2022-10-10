use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

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
pub struct CommentendChildren<'a, T>(pub Vec<MaybeComment<'a, T>>);

impl<'a, T: 'a> CommentendChildren<'a, T> {
    pub fn into_values(self) -> impl 'a + Iterator<Item = T> {
        self.0.into_iter().filter_map(MaybeComment::try_into_value)
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

#[derive(Debug, Clone)]
pub enum DefinitionOrAlias<'a, T> {
    Alias {
        name: Cow<'a, str>,
        alias: Cow<'a, str>,
    },
    Definition(T),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemVarVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: Option<u32>,
}
