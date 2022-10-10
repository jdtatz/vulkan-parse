use std::borrow::Cow;

use crate::Expression;

use super::common::*;

#[derive(Debug)]
pub struct SpirvExtension<'a> {
    pub name: Cow<'a, str>,
    // TODO, usually the only diffrence between `name` & `enable_extension` is the prefix ("SPV_" vs "VK_"), but not always
    pub enable_extension: Cow<'a, str>,
    pub enable_version: Option<SemVarVersion>,
}

#[derive(Debug)]
pub struct SpirvCapability<'a> {
    pub name: Cow<'a, str>,
    pub enable_version: Option<SemVarVersion>,
    pub enable_extension: Option<Cow<'a, str>>,
    pub struct_enable: Box<[StructEnable<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnableRequires<'a> {
    Core(SemVarVersion),
    Extension(Cow<'a, str>),
    Mix(SemVarVersion, Cow<'a, str>),
}

#[derive(Debug)]
pub struct StructEnable<'a> {
    pub name: Cow<'a, str>,
    pub feature: Cow<'a, str>,
    pub requires: EnableRequires<'a>,
}

#[derive(Debug)]
pub struct PropertyEnable<'a> {
    pub name: Cow<'a, str>,
    pub member: Cow<'a, str>,
    pub value: Expression<'a>,
    pub requires: EnableRequires<'a>,
}
