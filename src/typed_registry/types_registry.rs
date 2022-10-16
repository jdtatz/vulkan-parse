use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    str::FromStr,
};

use roxmltree::Node;
use serde::Serialize;

use crate::{
    c_with_vk_ext, get_req_attr, get_req_text, parse_cexpr, vk_tokenize, ErrorKind, Expression,
    Parse, ParseResult, Token, TypeSpecifier,
};

use super::common::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum PointerKind {
    Single,
    Double { inner_is_const: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ArrayLength<'a> {
    Static(NonZeroU32),
    Constant(Cow<'a, str>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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

impl<'a> fmt::Display for DynamicLength<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DynamicLength::NullTerminated => write!(f, "null-terminated"),
            DynamicLength::Static(n) => write!(f, "{}", n),
            DynamicLength::Parameterized(p) => write!(f, "{}", p),
            DynamicLength::ParameterizedField { parameter, field } => {
                write!(f, "{}-&gt;{}", parameter, field)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum DynamicShapeKind<'a> {
    Expression {
        latex_expr: Cow<'a, str>,
        c_expr: Expression<'a>,
    },
    Single(DynamicLength<'a>),
    Double(DynamicLength<'a>, DynamicLength<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum OptionalKind {
    Single(bool),
    Double(bool, bool),
}

impl FromStr for OptionalKind {
    type Err = <bool as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((a, b)) = s.split_once(',') {
            Ok(Self::Double(a.parse()?, b.parse()?))
        } else {
            s.parse().map(Self::Single)
        }
    }
}

impl fmt::Display for OptionalKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OptionalKind::Single(b) => write!(f, "{}", b),
            OptionalKind::Double(b1, b2) => write!(f, "{},{}", b1, b2),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ExternSyncKind<'a> {
    /// externsync="true"
    Value,
    Fields(Box<[Cow<'a, str>]>),
}

impl<'a> ExternSyncKind<'a> {
    fn from_str(s: &'a str) -> Self {
        match s {
            "true" => Self::Value,
            // FIXME split into 
            s => Self::Fields(s.split(',').map(Cow::Borrowed).collect())
            // #[cfg(debug_assertions)]
            // s => todo!("Unexpected <[field-like] externsync=...> of {:?}", s),
            // #[cfg(not(debug_assertions))]
            // _ => Err(()),
        }
    }
}

impl<'a> fmt::Display for ExternSyncKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternSyncKind::Value => write!(f, "true"),
            ExternSyncKind::Fields(fs) => crate::fmt_write_interspersed(f, fs.iter(), ","),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum NoAutoValidityKind {
    /// noautovalidity="true"
    Value,
}

impl FromStr for NoAutoValidityKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Self::Value),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <[field-like] noautovalidity=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

impl fmt::Display for NoAutoValidityKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => write!(f, "true"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RequiresType<'a> {
    pub name: Cow<'a, str>,
    pub requires: Option<Cow<'a, str>>,
}

/// <type category="include">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IncludeType<'a> {
    pub name: Cow<'a, str>,
    /// #include "{name}" is a local include
    pub is_local_include: Option<bool>,
}

/// <type category="define">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DefineType<'a> {
    pub name: Cow<'a, str>,
    pub comment: Option<Cow<'a, str>>,
    pub requires: Option<Cow<'a, str>>,
    pub is_disabled: bool,
    pub value: DefineTypeValue<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BaseTypeType<'a> {
    pub name: Cow<'a, str>,
    // FIXME: base_type can be a forward decleration, a simple typedef, or OBJC with define guards
    pub base_type_name: Option<Cow<'a, str>>,
    // ...
}

/// <type category="bitmask">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BitmaskType<'a> {
    pub name: Cow<'a, str>,
    pub is_64bits: bool,
    pub has_bitvalues: bool,
}

/// <type category="handle">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct HandleType<'a> {
    pub name: Cow<'a, str>,
    pub handle_kind: HandleKind,
    pub obj_type_enum: Cow<'a, str>,
    pub parent: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum HandleKind {
    Dispatch,
    NoDispatch,
}

impl FromStr for HandleKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "VK_DEFINE_NON_DISPATCHABLE_HANDLE" => Ok(HandleKind::NoDispatch),
            "VK_DEFINE_HANDLE" => Ok(HandleKind::Dispatch),
            #[cfg(debug_assertions)]
            s => todo!(
                "Unexpected <type category=\"handle\"><type>...</type> of {:?}",
                s
            ),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

impl fmt::Display for HandleKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HandleKind::Dispatch => write!(f, "VK_DEFINE_HANDLE"),
            HandleKind::NoDispatch => write!(f, "VK_DEFINE_NON_DISPATCHABLE_HANDLE"),
        }
    }
}

/// <type category="enum">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EnumType<'a> {
    pub name: Cow<'a, str>,
}

/// <type category="funcpointer">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FnPtrType<'a> {
    pub name_and_return: FieldLike<'a>,
    pub requires: Option<Cow<'a, str>>,
    pub params: Box<[FieldLike<'a>]>,
}

/// <type category="struct">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructType<'a> {
    pub name: Cow<'a, str>,
    pub members: CommentendChildren<'a, Member<'a>>,
    pub returned_only: Option<bool>,
    pub struct_extends: Option<Box<[Cow<'a, str>]>>,
    pub allow_duplicate: Option<bool>,
}

/// <type category="union">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnionType<'a> {
    pub name: Cow<'a, str>,
    pub members: CommentendChildren<'a, Member<'a>>,
    pub returned_only: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub enum MemberSelector {
    Type,
    Format,
    GeometryType,
}

impl FromStr for MemberSelector {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "type" => Ok(Self::Type),
            "format" => Ok(Self::Format),
            "geometryType" => Ok(Self::GeometryType),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <member selector=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

impl fmt::Display for MemberSelector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type => write!(f, "type"),
            Self::Format => write!(f, "format"),
            Self::GeometryType => write!(f, "geometryType"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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

impl FromStr for MemberLimitType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "min" => Ok(Self::Min),
            "max" => Ok(Self::Max),
            "exact" => Ok(Self::Exact),
            "bits" => Ok(Self::Bits),
            "bitmask" => Ok(Self::Bitmask),
            "range" => Ok(Self::Range),
            "struct" => Ok(Self::Struct),
            "noauto" => Ok(Self::NoAuto),
            "min,pot" => Ok(Self::MinPot),
            "max,pot" => Ok(Self::MaxPot),
            "min,mul" => Ok(Self::MinMul),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <member limittype=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

impl fmt::Display for MemberLimitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberLimitType::Min => write!(f, "min"),
            MemberLimitType::Max => write!(f, "max"),
            MemberLimitType::Exact => write!(f, "exact"),
            MemberLimitType::Bits => write!(f, "bits"),
            MemberLimitType::Bitmask => write!(f, "bitmask"),
            MemberLimitType::Range => write!(f, "range"),
            MemberLimitType::Struct => write!(f, "struct"),
            MemberLimitType::NoAuto => write!(f, "noauto"),
            MemberLimitType::MinPot => write!(f, "min,pot"),
            MemberLimitType::MaxPot => write!(f, "max,pot"),
            MemberLimitType::MinMul => write!(f, "min,mul"),
        }
    }
}

/// <member>
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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
        c_with_vk_ext::type_define(&tokens, node.attribute("name"), node.attribute("requires"))
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

impl<'a, 'input> Parse<'a, 'input> for BitmaskType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let ty_name = get_req_text(
            node.first_element_child()
                .ok_or_else(|| crate::ErrorKind::MissingChildElement("type", node.id()))?,
        )?;
        let is_64bits = match ty_name.trim() {
            "VkFlags" => false,
            "VkFlags64" => true,
            #[cfg(debug_assertions)]
            s => todo!(
                "Unexpected <type category=\"bitmask\"><type>...</type> of {:?}",
                s
            ),
            #[cfg(not(debug_assertions))]
            _ => return Ok(None),
        };
        Ok(Some(BitmaskType {
            name: Cow::Borrowed(get_req_text(
                node.last_element_child()
                    .ok_or_else(|| crate::ErrorKind::MissingChildElement("name", node.id()))?,
            )?),
            is_64bits,
            has_bitvalues: node.has_attribute("requires") || node.has_attribute("bitvalues"),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for HandleType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let ty_name = get_req_text(
            node.first_element_child()
                .ok_or_else(|| crate::ErrorKind::MissingChildElement("type", node.id()))?,
        )?;
        Ok(Some(HandleType {
            name: Cow::Borrowed(get_req_text(
                node.last_element_child()
                    .ok_or_else(|| crate::ErrorKind::MissingChildElement("name", node.id()))?,
            )?),
            handle_kind: ty_name.parse::<HandleKind>().unwrap(),
            obj_type_enum: get_req_attr(node, "objtypeenum").map(Cow::Borrowed)?,
            parent: node.attribute("parent").map(Cow::Borrowed),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for EnumType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(EnumType {
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for FnPtrType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, false);
        c_with_vk_ext::type_funcptr(&tokens, node.attribute("requires"))
            .map_err(|e| crate::ErrorKind::MixedParseError(e, node.id()))
            .map(Some)
    }
}

impl<'a, 'input> Parse<'a, 'input> for StructType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(StructType {
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            returned_only: node.attribute("returnedonly").and_then(|v| v.parse().ok()),
            struct_extends: node
                .attribute("structextends")
                .map(|es| es.split(',').map(Cow::Borrowed).collect()),
            allow_duplicate: node
                .attribute("allowduplicate")
                .and_then(|v| v.parse().ok()),
            members: Parse::parse(node)?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for UnionType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(UnionType {
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            returned_only: node.attribute("returnedonly").and_then(|v| v.parse().ok()),
            members: Parse::parse(node)?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for Member<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("member") {
            Ok(Some(Member {
                base: Parse::parse(node)?,
                selector: node.attribute("selector").and_then(|v| v.parse().ok()),
                selection: node.attribute("selection").map(Cow::Borrowed),
                values: node.attribute("values").map(Cow::Borrowed),
                limit_type: node.attribute("limittype").and_then(|v| v.parse().ok()),
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for FieldLike<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, false);
        let f = c_with_vk_ext::field_like(&tokens)
            .map_err(|e| crate::ErrorKind::MixedParseError(e, node.id()))?;
        let dynamic_shape = node.attribute("len").and_then(|len| {
            if let Some(latex_expr) = len.strip_prefix("latexmath:") {
                let c_expr = node.attribute("altlen").expect("The `altlen` attribute is required when the `len` attribute is a latex expression");
                Some(parse_cexpr(c_expr).map(|c_expr| DynamicShapeKind::Expression {
                    latex_expr: Cow::Borrowed(latex_expr),
                    c_expr,
                }))
            } else {
                let mut it = len.split(',').map(|v| {
                    if v == "null-terminated" {
                        DynamicLength::NullTerminated
                    } else if let Ok(n) = v.parse() {
                        DynamicLength::Static(n)
                    } else if let Some((parameter, field)) = v.split_once("->").or_else(|| v.split_once("-&gt;")) {
                        DynamicLength::ParameterizedField {
                            parameter: Cow::Borrowed(parameter),
                            field: Cow::Borrowed(field),
                        }
                    } else {
                        DynamicLength::Parameterized(Cow::Borrowed(v))
                    }
                });
                let outer = it.next().expect("The `len` attribute must not be empty");
                Some(Ok(match it.next() {
                    Some(inner) => {
                        let n = it.count();
                        if n != 0 {
                            #[cfg(debug_assertions)]
                            todo!(
                                "Expected only 1 or 2 comma-seperated values in the `len` attribute, found {} values",
                                2 + n
                            );
                            #[cfg(not(debug_assertions))]
                            return None;
                        }
                        DynamicShapeKind::Double(outer, inner)
                    }
                    None => DynamicShapeKind::Single(outer),
                }))
            }
        }).transpose().map_err(|e| ErrorKind::MixedParseError(e, node.id()))?;
        Ok(Some(FieldLike {
            dynamic_shape,
            extern_sync: node
                .attribute("externsync")
                .map(|v| ExternSyncKind::from_str(v)),
            optional: node.attribute("optional").and_then(|v| v.parse().ok()),
            no_auto_validity: node
                .attribute("noautovalidity")
                .and_then(|v| v.parse().ok()),
            object_type: node.attribute("objecttype").map(Cow::Borrowed),
            ..f
        }))
    }
}
