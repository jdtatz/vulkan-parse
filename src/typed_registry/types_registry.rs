use std::{
    borrow::Cow,
    num::{NonZeroU32, NonZeroU8},
};

use crate::{Expression, Token};

use super::common::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PointerKind {
    Single,
    Double { inner_is_const: bool },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayLength<'a> {
    Static(NonZeroU32),
    Constant(Cow<'a, str>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DynamicLength<'a> {
    NullTerminated,
    // FIXME only found in VkAccelerationStructureBuildGeometryInfoKHR->ppGeometries, is this a mistake?
    Static(NonZeroU32),
    Parameterized(Cow<'a, str>),
    ParameterizedField {
        parameter: Cow<'a, str>,
        field: Cow<'a, str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DynamicShapeKind<'a> {
    Expression {
        latex_expr: Cow<'a, str>,
        c_expr: Expression<'a>,
    },
    Single(DynamicLength<'a>),
    Double(DynamicLength<'a>, DynamicLength<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OptionalKind {
    Single(bool),
    Double(bool, bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternSyncKind<'a> {
    /// externsync="true"
    Value,
    Fields(Box<[Cow<'a, str>]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NoAutoValidityKind {
    /// noautovalidity="true"
    Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FieldLike<'a> {
    pub name: Cow<'a, str>,
    pub type_name: Cow<'a, str>,
    pub is_struct: bool,
    pub is_const: bool,
    pub pointer_kind: Option<PointerKind>,
    pub bitfield_size: Option<NonZeroU8>,
    pub array_shape: Option<Box<[ArrayLength<'a>]>>,
    pub dynamic_shape: Option<DynamicShapeKind<'a>>,
    pub extern_sync: Option<ExternSyncKind<'a>>,
    pub optional: Option<OptionalKind>,
    pub no_auto_validity: Option<NoAutoValidityKind>,
    /// The field-like that paramertizes what type of vulkan handle this one is.
    /// => This field-like is a generic vulkan handle, and it is an error if `type_name` isn't `uint64_t`
    pub object_type: Option<Cow<'a, str>>,
    pub comment: Option<Cow<'a, str>>,
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
    pub comment: Option<Cow<'a, str>>,
    pub requires: Option<Cow<'a, str>>,
    pub is_disabled: bool,
    pub value: DefineTypeValue<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefineTypeValue<'a> {
    Expression(Expression<'a>),
    FunctionDefine {
        params: Box<[Cow<'a, str>]>,
        expression: Box<[Token<'a>]>,
    },
    MacroFunctionCall {
        name: Cow<'a, str>,
        args: Box<[Expression<'a>]>,
    },
    Code(Box<[Token<'a>]>),
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
