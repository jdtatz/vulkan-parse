use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    str::FromStr,
};

use roxmltree::Node;
use serde::Serialize;

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{
    attribute, c_with_vk_ext, text_value, try_attribute, try_attribute_fs, try_attribute_sep,
    vk_tokenize, Expression, Parse, ParseResult, Token, TypeSpecifier,
};

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

impl<'a> From<&'a str> for DynamicLength<'a> {
    fn from(v: &'a str) -> Self {
        if v == "null-terminated" {
            DynamicLength::NullTerminated
        } else if let Ok(n) = v.parse() {
            DynamicLength::Static(n)
        } else if let Some((parameter, field)) =
            v.split_once("->").or_else(|| v.split_once("-&gt;"))
        {
            DynamicLength::ParameterizedField {
                parameter: Cow::Borrowed(parameter),
                field: Cow::Borrowed(field),
            }
        } else {
            DynamicLength::Parameterized(Cow::Borrowed(v))
        }
    }
}

impl<'a> fmt::Display for DynamicLength<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DynamicLength::NullTerminated => write!(f, "null-terminated"),
            DynamicLength::Static(n) => write!(f, "{}", n),
            DynamicLength::Parameterized(p) => write!(f, "{}", p),
            DynamicLength::ParameterizedField { parameter, field } => {
                // write!(f, "{}-&gt;{}", parameter, field)
                write!(f, "{}->{}", parameter, field)
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
pub enum ExternSyncFieldValue<'a> {
    ValueField(Cow<'a, str>),
    ArrayField(Cow<'a, str>),
}

impl<'a> From<&'a str> for ExternSyncFieldValue<'a> {
    fn from(s: &'a str) -> Self {
        if let Some(field) = s.strip_suffix("[]") {
            Self::ArrayField(Cow::Borrowed(field))
        } else {
            Self::ValueField(Cow::Borrowed(s))
        }
    }
}

/// This is likely redundant and may be removed in the future
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ExternSyncParam<'a> {
    ValueParam(Cow<'a, str>),
    ArrayParam(Cow<'a, str>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExternSyncField<'a> {
    /// This is likely redundant and may be removed in the future
    pub param: ExternSyncParam<'a>,
    pub field: ExternSyncFieldValue<'a>,
}

#[derive(Debug, Clone)]
pub struct ExternSyncParseError;

impl fmt::Display for ExternSyncParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Unrecongized `externsync` attribute value, the expected form is `{{param}}(->|[].){{field}}([])?`"
        )
    }
}

impl std::error::Error for ExternSyncParseError {}

impl<'a> TryFrom<&'a str> for ExternSyncField<'a> {
    type Error = ExternSyncParseError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if let Some((param, field)) = value.split_once("[].") {
            Ok(Self {
                param: ExternSyncParam::ArrayParam(Cow::Borrowed(param)),
                field: ExternSyncFieldValue::from(field),
            })
        } else if let Some((param, field)) =
            value.split_once("->").or_else(|| value.split_once("-&gt;"))
        {
            Ok(Self {
                param: ExternSyncParam::ValueParam(Cow::Borrowed(param)),
                field: ExternSyncFieldValue::from(field),
            })
        } else {
            Err(ExternSyncParseError)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ExternSyncKind<'a> {
    /// externsync="true"
    Value,
    Fields(Vec<ExternSyncField<'a>>),
}

impl<'a> TryFrom<&'a str> for ExternSyncKind<'a> {
    type Error = ExternSyncParseError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "true" => Ok(Self::Value),
            s => s
                .split(',')
                .map(ExternSyncField::try_from)
                .collect::<Result<_, _>>()
                .map(Self::Fields),
        }
    }
}

impl<'a> fmt::Display for ExternSyncField<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.param {
            ExternSyncParam::ValueParam(p) => write!(f, "{}->", p),
            ExternSyncParam::ArrayParam(p) => write!(f, "{}[].", p),
        }?;
        match &self.field {
            ExternSyncFieldValue::ValueField(v) => write!(f, "{}", v),
            ExternSyncFieldValue::ArrayField(v) => write!(f, "{}[]", v),
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

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum NoAutoValidityKind {
    /// noautovalidity="true"
    #[strum(serialize = "true")]
    Value,
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
    #[must_use]
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
    Enum(DefinitionOrAlias<'a, EnumType<'a>>),
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
    pub deprecation_comment: Option<Cow<'a, str>>,
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

impl<'a> DefineTypeValue<'a> {
    pub fn as_expr(&self) -> Option<Cow<'_, Expression<'a>>> {
        match self {
            Self::Expression(e) => Some(Cow::Borrowed(e)),
            Self::MacroFunctionCall { name, args } => Some(Cow::Owned(Expression::FunctionCall(
                Box::new(Expression::Identifier(name.clone())),
                args.clone(),
            ))),
            _ => None,
        }
    }
}

// NOTE: IOSurfaceRef is incorrectly `typedef struct __IOSurface* <name>IOSurfaceRef</name>;`
//       but should be  `typedef struct <type>__IOSurface</type>* <name>IOSurfaceRef</name>;`
/// <type category="basetype">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum BaseTypeType<'a> {
    Forward(Cow<'a, str>),
    TypeDef(FieldLike<'a>),
    DefineGuarded {
        pre: Vec<Token<'a>>,
        name: Cow<'a, str>,
        post: Vec<Token<'a>>,
    },
}

impl<'a> BaseTypeType<'a> {
    #[must_use]
    pub fn name(&self) -> &Cow<'a, str> {
        match self {
            BaseTypeType::Forward(name)
            | BaseTypeType::TypeDef(FieldLike { name, .. })
            | BaseTypeType::DefineGuarded { name, .. } => name,
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum HandleKind {
    #[strum(serialize = "VK_DEFINE_HANDLE")]
    Dispatch,
    #[strum(serialize = "VK_DEFINE_NON_DISPATCHABLE_HANDLE")]
    NoDispatch,
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
    pub params: Option<Vec<FieldLike<'a>>>,
}

/// <type category="struct">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructType<'a> {
    pub name: Cow<'a, str>,
    pub members: CommentendChildren<'a, Member<'a>>,
    pub returned_only: Option<bool>,
    pub struct_extends: Option<Vec<Cow<'a, str>>>,
    pub allow_duplicate: Option<bool>,
    pub requires: Option<Cow<'a, str>>,
    pub comment: Option<Cow<'a, str>>,
}

/// <type category="union">
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnionType<'a> {
    pub name: Cow<'a, str>,
    pub members: CommentendChildren<'a, Member<'a>>,
    pub returned_only: Option<bool>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
#[non_exhaustive]
pub enum MemberSelector {
    #[strum(serialize = "type")]
    Type,
    #[strum(serialize = "format")]
    Format,
    #[strum(serialize = "geometryType")]
    GeometryType,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
#[non_exhaustive]
pub enum MemberLimitType {
    #[strum(serialize = "min")]
    Min,
    #[strum(serialize = "max")]
    Max,
    #[strum(serialize = "exact")]
    Exact,
    #[strum(serialize = "bits")]
    Bits,
    #[strum(serialize = "bitmask")]
    Bitmask,
    #[strum(serialize = "range")]
    Range,
    #[strum(serialize = "struct")]
    Struct,
    #[strum(serialize = "noauto")]
    NoAuto,
    #[strum(serialize = "min,pot")]
    MinPot,
    #[strum(serialize = "max,pot")]
    MaxPot,
    #[strum(serialize = "min,mul")]
    MinMul,
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
            let category = try_attribute(node, "category")?;
            match category {
                None => RequiresType::parse(node).map(Type::Requires),
                Some("include") => IncludeType::parse(node).map(Type::Include),
                Some("define") => DefineType::parse(node).map(Type::Define),
                Some("basetype") => BaseTypeType::parse(node).map(Type::BaseType),
                Some("bitmask") => DefinitionOrAlias::parse(node).map(Type::Bitmask),
                Some("handle") => DefinitionOrAlias::parse(node).map(Type::Handle),
                Some("enum") => DefinitionOrAlias::parse(node).map(Type::Enum),
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
            name: attribute(node, "name")?,
            requires: try_attribute(node, "requires")?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for IncludeType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(IncludeType {
            name: attribute(node, "name")?,
            is_local_include: node.text().map(|s| s.contains('"')),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for DefineType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, true, false)?;
        c_with_vk_ext::type_define(
            &tokens,
            try_attribute(node, "name")?,
            try_attribute(node, "requires")?,
        )
        .map_err(|e| crate::ErrorKind::PegParsingError(e, node.id()))
        .map(Some)
    }
}

impl<'a, 'input> Parse<'a, 'input> for BaseTypeType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, true, true)?;
        c_with_vk_ext::type_basetype(&tokens)
            .map_err(|e| crate::ErrorKind::PegParsingError(e, node.id()))
            .map(Some)
    }
}

impl<'a, 'input> Parse<'a, 'input> for BitmaskType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let ty_node = node
            .first_element_child()
            .ok_or_else(|| crate::ErrorKind::MissingChildElement("type", node.id()))?;
        let name_node = node
            .last_element_child()
            .ok_or_else(|| crate::ErrorKind::MissingChildElement("name", node.id()))?;

        let ty_name = text_value(ty_node)?;
        let is_64bits = match ty_name {
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
            name: text_value(name_node)?,
            is_64bits,
            //  FIXME add check that name.replace("Flags", "FlagBits") == attribute("requires").xor(attribute("bitvalues"))
            has_bitvalues: node.has_attribute("requires") || node.has_attribute("bitvalues"),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for HandleType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let ty_node = node
            .first_element_child()
            .ok_or_else(|| crate::ErrorKind::MissingChildElement("type", node.id()))?;
        let name_node = node
            .last_element_child()
            .ok_or_else(|| crate::ErrorKind::MissingChildElement("name", node.id()))?;
        Ok(Some(HandleType {
            name: text_value(name_node)?,
            handle_kind: text_value(ty_node)?,
            obj_type_enum: attribute(node, "objtypeenum")?,
            parent: try_attribute(node, "parent")?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for EnumType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(EnumType {
            name: attribute(node, "name")?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for FnPtrType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, false, false)?;
        c_with_vk_ext::type_funcptr(&tokens, try_attribute(node, "requires")?)
            .map_err(|e| crate::ErrorKind::PegParsingError(e, node.id()))
            .map(Some)
    }
}

impl<'a, 'input> Parse<'a, 'input> for StructType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(StructType {
            name: attribute(node, "name")?,
            returned_only: try_attribute_fs(node, "returnedonly")?,
            struct_extends: try_attribute_sep::<_, ','>(node, "structextends")?,
            allow_duplicate: try_attribute_fs(node, "allowduplicate")?,
            members: Parse::parse(node)?,
            requires: try_attribute(node, "requires")?,
            comment: try_attribute(node, "comment")?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for UnionType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(UnionType {
            name: attribute(node, "name")?,
            returned_only: try_attribute_fs(node, "returnedonly")?,
            members: Parse::parse(node)?,
            comment: try_attribute(node, "comment")?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for Member<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("member") {
            Ok(Some(Member {
                base: Parse::parse(node)?,
                selector: try_attribute(node, "selector")?,
                selection: try_attribute(node, "selection")?,
                values: try_attribute(node, "values")?,
                limit_type: try_attribute(node, "limittype")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for FieldLike<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let tokens = vk_tokenize(node, false, false)?;
        let f = c_with_vk_ext::field_like(&tokens)
            .map_err(|e| crate::ErrorKind::PegParsingError(e, node.id()))?;
        let dynamic_shape = try_attribute(node, "len")?
            .map(|len: &str| {
                Ok(if let Some(latex_expr) = len.strip_prefix("latexmath:") {
                    // let c_expr = try_attribute(node, "altlen").transpose().expect("The `altlen` attribute is required when the `len` attribute is a latex expression");
                    let c_expr = attribute(node, "altlen")?;
                    DynamicShapeKind::Expression {
                        latex_expr: Cow::Borrowed(latex_expr),
                        c_expr,
                    }
                } else if let Some((l1, l2)) = len.split_once(',') {
                    DynamicShapeKind::Double(l1.into(), l2.into())
                } else {
                    DynamicShapeKind::Single(len.into())
                })
            })
            .transpose()?;
        Ok(Some(FieldLike {
            dynamic_shape,
            extern_sync: try_attribute(node, "externsync")?,
            optional: try_attribute_fs(node, "optional")?,
            no_auto_validity: try_attribute(node, "noautovalidity")?,
            object_type: try_attribute(node, "objecttype")?,
            ..f
        }))
    }
}
