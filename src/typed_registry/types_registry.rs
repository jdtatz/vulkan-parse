use std::borrow::Cow;

use super::common::*;

#[derive(Debug, Clone)]
pub struct FieldLike<'a> {
    pub name: Cow<'a, str>,
    pub type_name: Cow<'a, str>,
}

/// <type>
#[derive(Debug)]
pub enum Type<'a> {
    /// <type> without category attribute
    Requires(RequiresType<'a>),
    /// <type category="include">
    Include(IncludeType<'a>),
    /// <type category="define">
    Define(DefineType<'a>),
    /// <type category="basetype">
    BaseType(BaseTypeType<'a>),
    /// <type category="bitmask">
    Bitmask(DefinitionOrAlias<'a, BitmaskType<'a>>),
    /// <type category="handle">
    Handle(DefinitionOrAlias<'a, HandleType<'a>>),
    /// <type category="enum">
    Enum(EnumType<'a>),
    /// <type category="funcpointer">
    FnPtr(FnPtrType<'a>),
    /// <type category="struct">
    Struct(DefinitionOrAlias<'a, StructType<'a>>),
    /// <type category="union">
    Union(UnionType<'a>),
}

/// <type> without category attribute
#[derive(Debug)]
pub struct RequiresType<'a> {
    pub name: Cow<'a, str>,
    pub requires: Cow<'a, str>,
}

/// <type category="include">
#[derive(Debug)]
pub struct IncludeType<'a> {
    pub name: Cow<'a, str>,
    /// #include "{name}" is a local include
    pub is_local_include: Option<bool>,
}

/// <type category="define">
#[derive(Debug)]
pub struct DefineType<'a> {
    pub name: Cow<'a, str>,
    // ...
}

// TODO: IOSurfaceRef is incorrectly `typedef struct __IOSurface* <name>IOSurfaceRef</name>;`
//       but should be  `typedef struct <type>__IOSurface</type>* <name>IOSurfaceRef</name>;`
/// <type category="basetype">
#[derive(Debug)]
pub struct BaseTypeType<'a> {
    pub name: Cow<'a, str>,
    // FIXME: base_type can be a forward decleration, a simple typedef, or OBJC with define guards
    pub base_type_name: Option<Cow<'a, str>>,
    // ...
}

/// <type category="bitmask">
#[derive(Debug, Clone)]
pub struct BitmaskType<'a> {
    pub name: Cow<'a, str>,
    pub is_64bits: bool,
    pub has_bitvalues: bool,
}

/// <type category="handle">
#[derive(Debug, Clone)]
pub struct HandleType<'a> {
    pub name: Cow<'a, str>,
    pub handle_kind: HandleKind,
    pub obj_type_enum: Cow<'a, str>,
    pub parent: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub enum HandleKind {
    Dispatch,
    NoDispatch,
}

/// <type category="enum">
#[derive(Debug, Clone)]
pub struct EnumType<'a> {
    pub name: Cow<'a, str>,
}

/// <type category="funcpointer">
#[derive(Debug, Clone)]
pub struct FnPtrType<'a> {
    pub name_and_return: FieldLike<'a>,
    pub requires: Option<Cow<'a, str>>,
    pub params: Box<[FieldLike<'a>]>,
}

/// <type category="struct">
#[derive(Debug, Clone)]
pub struct StructType<'a> {
    pub name: Cow<'a, str>,
    pub members: CommentendChildren<'a, Member<'a>>,
    pub returned_only: Option<bool>,
    pub struct_extends: Box<[Cow<'a, str>]>,
    pub allow_duplicate: Option<bool>,
}

/// <type category="union">
#[derive(Debug, Clone)]
pub struct UnionType<'a> {
    pub name: Cow<'a, str>,
    pub members: CommentendChildren<'a, Member<'a>>,
    pub returned_only: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum MemberSelector {
    Type,
    Format,
    GeometryType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum MemberLimitType {
    Min,
    Max,
    Exact,
    Bits,
    Bitmask,
    Range,
    Struct,
    NoAuto,
    MinPot,
    MaxPot,
    MinMul,
}

/// <member>
#[derive(Debug, Clone)]
pub struct Member<'a> {
    pub base: FieldLike<'a>,
    pub selector: Option<MemberSelector>,
    pub selection: Option<Cow<'a, str>>,
    pub values: Option<Cow<'a, str>>,
    pub limit_type: Option<MemberLimitType>,
}
