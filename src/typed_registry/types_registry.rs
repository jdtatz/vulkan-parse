use std::{
    borrow::Cow,
    num::{NonZeroU32, NonZeroU8},
};

use roxmltree::Node;

use crate::{
    c_with_vk_ext, get_req_attr, get_req_text, vk_tokenize, Expression, Parse, ParseResult, Token,
    TypeSpecifier,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldLike<'a> {
    pub name: Cow<'a, str>,
    pub type_name: TypeSpecifier<'a>,
    /// if true, then either a const pointer or array
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

impl<'a> FieldLike<'a> {
    pub fn default_new(name: Cow<'a, str>, type_name: TypeSpecifier<'a>) -> Self {
        Self {
            name,
            type_name,
            is_const: false,
            pointer_kind: None,
            bitfield_size: None,
            array_shape: None,
            dynamic_shape: None,
            extern_sync: None,
            optional: None,
            no_auto_validity: None,
            object_type: None,
            comment: None,
        }
    }
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
    pub requires: Option<Cow<'a, str>>,
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

impl<'a, 'input> Parse<'a, 'input> for Type<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("type") {
            let category = node.attribute("category");
            match category {
                None => RequiresType::parse(node).map(Type::Requires),
                Some("include") => IncludeType::parse(node).map(Type::Include),
                Some("define") => DefineType::parse(node).map(Type::Define),
                Some("basetype") => BaseTypeType::parse(node).map(Type::BaseType),
                Some("bitmask") => DefinitionOrAlias::parse(node).map(Type::Bitmask),
                Some("handle") => DefinitionOrAlias::parse(node).map(Type::Handle),
                Some("enum") => EnumType::parse(node).map(Type::Enum),
                Some("funcpointer") => FnPtrType::parse(node).map(Type::FnPtr),
                Some("struct") => DefinitionOrAlias::parse(node).map(Type::Struct),
                Some("union") => UnionType::parse(node).map(Type::Union),
                Some(_) => unreachable!(),
            }
            .map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for RequiresType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(RequiresType {
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            requires: node.attribute("requires").map(Cow::Borrowed),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for IncludeType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(IncludeType {
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            is_local_include: node.text().map(|s| s.contains('"')),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for DefineType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, true);
        c_with_vk_ext::type_define(&tokens, node.attribute("name"), node.attribute("require"))
            .map_err(|e| crate::ErrorKind::MixedParseError(e, node.id()))
            .map(Some)
    }
}

impl<'a, 'input> Parse<'a, 'input> for BaseTypeType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        // FIXME Fully parse the OBJC with define guards
        Ok(Some(BaseTypeType {
            name: Cow::Borrowed(get_req_text(
                node.first_element_child()
                    .ok_or_else(|| crate::ErrorKind::MissingChildElement("name", node.id()))?,
            )?),
            base_type_name: node
                .last_element_child()
                .filter(|n| n.has_tag_name("type"))
                .map(|n| get_req_text(n).map(Cow::Borrowed))
                .transpose()?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for DefinitionOrAlias<'a, BitmaskType<'a>> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }
}

impl<'a, 'input> Parse<'a, 'input> for DefinitionOrAlias<'a, HandleType<'a>> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }
}

impl<'a, 'input> Parse<'a, 'input> for EnumType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }
}

impl<'a, 'input> Parse<'a, 'input> for FnPtrType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }
}

impl<'a, 'input> Parse<'a, 'input> for DefinitionOrAlias<'a, StructType<'a>> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }
}

impl<'a, 'input> Parse<'a, 'input> for UnionType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        todo!()
    }
}
