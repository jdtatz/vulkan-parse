use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    ops,
    str::FromStr,
};

use roxmltree::Node;
use vulkan_parse_derive_helper::DisplayEscaped;

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{
    tokenize, DisplayEscaped, Expression, ParseResult, Token, TryFromEscapedStr, TryFromTokens,
    TypeSpecifier, UnescapedStr, VulkanApi,
};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum PointerKind {
    Single,
    Double { inner_is_const: bool },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ArrayLength<'a> {
    Static(NonZeroU32),
    Constant(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum DynamicLength<'a> {
    NullTerminated,
    // FIXME only found in VkAccelerationStructureBuildGeometryInfoKHR->ppGeometries, is this a mistake?
    Static(NonZeroU32),
    Parameterized(&'a str),
    ParameterizedField { parameter: &'a str, field: &'a str },
}

impl<'a, 'de: 'a> From<&'de str> for DynamicLength<'a> {
    fn from(v: &'de str) -> Self {
        if v == "null-terminated" {
            DynamicLength::NullTerminated
        } else if let Ok(n) = v.parse() {
            DynamicLength::Static(n)
        } else if let Some((parameter, field)) =
            v.split_once("->").or_else(|| v.split_once("-&gt;"))
        {
            DynamicLength::ParameterizedField {
                parameter: (parameter),
                field: (field),
            }
        } else {
            DynamicLength::Parameterized(v)
        }
    }
}

impl<'a> fmt::Display for DynamicLength<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DynamicLength::NullTerminated => write!(f, "null-terminated"),
            DynamicLength::Static(n) => write!(f, "{n}"),
            DynamicLength::Parameterized(p) => write!(f, "{p}"),
            DynamicLength::ParameterizedField { parameter, field } => {
                // write!(f, "{}-&gt;{}", parameter, field)
                write!(f, "{parameter}->{field}")
            }
        }
    }
}

impl<'a> DisplayEscaped for DynamicLength<'a> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DynamicLength::NullTerminated => write!(f, "null-terminated"),
            DynamicLength::Static(n) => write!(f, "{n}"),
            DynamicLength::Parameterized(p) => write!(f, "{p}"),
            DynamicLength::ParameterizedField { parameter, field } => {
                write!(f, "{parameter}-&gt;{field}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum DynamicShapeKind<'a> {
    Expression {
        latex_expr: &'a str,
        c_expr: Expression<'a>,
    },
    Single(DynamicLength<'a>),
    Double(DynamicLength<'a>, DynamicLength<'a>),
}

impl<'a, 'xml: 'a> crate::FromAttributes<'xml> for DynamicShapeKind<'a> {
    fn from_attributes<'input: 'xml>(
        node: Node<'xml, 'input>,
    ) -> ParseResult<Result<Self, &'static [&'static str]>> {
        if let Some(len) = crate::try_from_attribute::<Option<&str>>(node, "len")? {
            Ok(Ok(
                if let Some(latex_expr) = len.strip_prefix("latexmath:") {
                    // let c_expr = try_attribute(node, "altlen").transpose().expect("The `altlen` attribute is required when the `len` attribute is a latex expression");
                    let c_expr = crate::try_from_attribute(node, "altlen")?;
                    DynamicShapeKind::Expression { latex_expr, c_expr }
                } else if let Some((l1, l2)) = len.split_once(',') {
                    DynamicShapeKind::Double(l1.into(), l2.into())
                } else {
                    DynamicShapeKind::Single(len.into())
                },
            ))
        } else {
            Ok(Err(&["len"]))
        }
    }
}

impl<'a> crate::IntoXMLAttributes for DynamicShapeKind<'a> {
    fn write_attributes<'t, 'w, W: ?Sized + crate::XMLWriter>(
        &self,
        element: crate::XMLElementBuilder<'t, 'w, W>,
    ) -> Result<crate::XMLElementBuilder<'t, 'w, W>, W::Error> {
        match self {
            DynamicShapeKind::Expression { latex_expr, c_expr } => element
                .with_escaped_attribute("len", &format_args!("latexmath:{latex_expr}"))?
                .with_escaped_attribute("altlen", c_expr),
            DynamicShapeKind::Single(l) => element.with_escaped_attribute("len", l),
            DynamicShapeKind::Double(l1, l2) => {
                element.with_escaped_attribute("len", &format_args!("{l1},{l2}"))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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
            OptionalKind::Single(b) => write!(f, "{b}"),
            OptionalKind::Double(b1, b2) => write!(f, "{b1},{b2}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExternSyncFieldValue<'a> {
    ValueField(&'a str),
    ArrayField(&'a str),
}

impl<'a> From<&'a str> for ExternSyncFieldValue<'a> {
    fn from(s: &'a str) -> Self {
        if let Some(field) = s.strip_suffix("[]") {
            Self::ArrayField(field)
        } else {
            Self::ValueField(s)
        }
    }
}

/// This is likely redundant and may be removed in the future
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExternSyncParam<'a> {
    ValueParam(&'a str),
    ArrayParam(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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

impl<'a, 'de: 'a> TryFrom<&'de str> for ExternSyncField<'a> {
    type Error = ExternSyncParseError;

    fn try_from(value: &'de str) -> Result<Self, Self::Error> {
        if let Some((param, field)) = value.split_once("[].") {
            Ok(Self {
                param: ExternSyncParam::ArrayParam(param),
                field: ExternSyncFieldValue::from(field),
            })
        } else if let Some((param, field)) =
            value.split_once("->").or_else(|| value.split_once("-&gt;"))
        {
            Ok(Self {
                param: ExternSyncParam::ValueParam(param),
                field: ExternSyncFieldValue::from(field),
            })
        } else {
            Err(ExternSyncParseError)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExternSyncKind<'a> {
    /// externsync="true"
    True,
    Fields(Vec<ExternSyncField<'a>>),
}

impl<'a, 'de: 'a> TryFrom<&'de str> for ExternSyncKind<'a> {
    type Error = ExternSyncParseError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "true" => Ok(Self::True),
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
            ExternSyncParam::ValueParam(p) => write!(f, "{p}->"),
            ExternSyncParam::ArrayParam(p) => write!(f, "{p}[]."),
        }?;
        match &self.field {
            ExternSyncFieldValue::ValueField(v) => write!(f, "{v}"),
            ExternSyncFieldValue::ArrayField(v) => write!(f, "{v}[]"),
        }
    }
}

impl<'a> fmt::Display for ExternSyncKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternSyncKind::True => write!(f, "true"),
            ExternSyncKind::Fields(fs) => write!(
                f,
                "{}",
                crate::InterspersedDisplay::<crate::CommaSeperator, _>::new(fs.as_slice())
            ),
        }
    }
}
impl<'a> crate::DisplayEscaped for ExternSyncKind<'a> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //FIXME
        crate::Unescaped(self).escaped_fmt(f)
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
pub enum NoAutoValidityKind {
    /// noautovalidity="true"
    #[strum(serialize = "true")]
    True,
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
pub enum FieldLikeDeprecationKind {
    /// deprecated="ignored"
    #[strum(serialize = "ignored")]
    Ignored,
}

// TODO Bikeshed needed
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum FieldLikeSizing<'a> {
    BitfieldSize(NonZeroU8),
    ArrayShape(Vec<ArrayLength<'a>>),
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tokenized)]
pub struct FieldLike<'a> {
    pub name: &'a str,
    pub type_name: TypeSpecifier<'a>,
    /// if true, then either a const pointer or array
    pub is_const: bool,
    pub pointer_kind: Option<PointerKind>,
    pub sizing: Option<FieldLikeSizing<'a>>,
    #[xml(attribute(flattened))]
    pub dynamic_shape: Option<DynamicShapeKind<'a>>,
    /// denotes that the member should be externally synchronized when accessed by Vulkan
    #[xml(attribute(rename = "externsync"))]
    pub extern_sync: Option<ExternSyncKind<'a>>,
    /// whether this value can be omitted by providing NULL (for pointers), VK_NULL_HANDLE (for handles) or 0 (for bitmasks/values)
    #[xml(attribute())]
    pub optional: Option<OptionalKind>,
    /// no automatic validity language should be generated
    #[xml(attribute(rename = "noautovalidity"))]
    pub no_auto_validity: Option<NoAutoValidityKind>,
    /// The field-like that paramertizes what type of vulkan handle this one is.
    /// => This field-like is a generic vulkan handle, and it is an error if `type_name` isn't `uint64_t`
    #[xml(attribute(rename = "objecttype"))]
    pub object_type: Option<&'a str>,
    /// which vulkan api this belongs to
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    /// If this field-like is deprecated, and how it is e.g. ignored
    #[xml(attribute())]
    pub deprecated: Option<FieldLikeDeprecationKind>,
    /// descriptive text with no semantic meaning
    pub comment: Option<UnescapedStr<'a>>,
}

impl<'a> FieldLike<'a> {
    #[must_use]
    pub fn default_new(name: &'a str, type_name: TypeSpecifier<'a>) -> Self {
        Self {
            name,
            type_name,
            is_const: false,
            pointer_kind: None,
            sizing: None,
            dynamic_shape: None,
            extern_sync: None,
            optional: None,
            no_auto_validity: None,
            object_type: None,
            api: None,
            deprecated: None,
            comment: None,
        }
    }

    #[must_use]
    pub fn array_shape(&self) -> Option<&[ArrayLength<'a>]> {
        match &self.sizing {
            Some(FieldLikeSizing::ArrayShape(shape)) => Some(shape.as_slice()),
            _ => None,
        }
    }

    #[must_use]
    pub fn bitfield_size(&self) -> Option<NonZeroU8> {
        match self.sizing {
            Some(FieldLikeSizing::BitfieldSize(n)) => Some(n),
            _ => None,
        }
    }
}

/// <type>
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "type")]
pub enum Type<'a> {
    /// <type category="include">
    #[xml(discriminant(attr = "category", value = "include"))]
    Include(IncludeType<'a>),
    /// <type category="define">
    #[xml(discriminant(attr = "category", value = "define"))]
    Define(DefineType<'a>),
    /// <type category="basetype">
    #[xml(discriminant(attr = "category", value = "basetype"))]
    BaseType(BaseTypeType<'a>),
    /// <type category="bitmask">
    #[xml(discriminant(attr = "category", value = "bitmask"))]
    Bitmask(DefinitionOrAlias<'a, BitmaskType<'a>>),
    /// <type category="handle">
    #[xml(discriminant(attr = "category", value = "handle"))]
    Handle(DefinitionOrAlias<'a, HandleType<'a>>),
    /// <type category="enum">
    #[xml(discriminant(attr = "category", value = "enum"))]
    Enum(DefinitionOrAlias<'a, EnumType<'a>>),
    /// <type category="funcpointer">
    #[xml(discriminant(attr = "category", value = "funcpointer"))]
    FnPtr(FnPtrType<'a>),
    /// <type category="struct">
    #[xml(discriminant(attr = "category", value = "struct"))]
    Struct(DefinitionOrAlias<'a, StructType<'a>>),
    /// <type category="union">
    #[xml(discriminant(attr = "category", value = "union"))]
    Union(UnionType<'a>),
    /// <type> without category attribute
    Requires(RequiresType<'a>),
}

/// <type> without category attribute
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "type")]
pub struct RequiresType<'a> {
    /// name of this type
    #[xml(attribute())]
    pub name: &'a str,
    /// name of another type definition required by this one
    #[xml(attribute())]
    pub requires: Option<&'a str>,
}

/// <type category="include">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct IncludeType<'a> {
    pub name: &'a str,
    /// #include "{name}" is a local include
    pub is_local_include: Option<bool>,
}

impl<'a> crate::Tagged for IncludeType<'a> {
    const TAG: &'static str = "type";
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct MacroCode<'t>(pub Vec<Token<'t>>);

impl<'t, 's: 't> TryFromEscapedStr<'s> for MacroCode<'t> {
    type Error = crate::lexer::Error;

    fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error> {
        tokenize(s, true, true)
            .collect::<Result<_, _>>()
            .map(MacroCode)
    }
}

impl<'t> fmt::Display for MacroCode<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut last_ident_like = false;
        for token in &self.0 {
            let is_ident_like = token.is_ident_like();
            if last_ident_like && is_ident_like {
                write!(f, " ")?
            };
            write!(f, "{token}")?;
            last_ident_like = is_ident_like;
        }
        Ok(())
    }
}

impl<'t> DisplayEscaped for MacroCode<'t> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut last_ident_like = false;
        for token in &self.0 {
            let is_ident_like = token.is_ident_like();
            if last_ident_like && is_ident_like {
                write!(f, " ")?
            };
            token.escaped_fmt(f)?;
            last_ident_like = is_ident_like;
        }
        Ok(())
    }
}

/// <type category="define" name="...">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "type")]
pub struct GuardedDefine<'a> {
    #[xml(attribute())]
    pub name: &'a str,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
    /// name of another type definition required by this one
    #[xml(attribute())]
    pub requires: Option<&'a str>,
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    #[xml(text)]
    pub code: MacroCode<'a>,
}

/// <type category="define">...<name>...</name>...</type>
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tokenized, tag = "type")]
pub struct MacroDefine<'a> {
    /// name of defined macro
    pub name: &'a str,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
    /// name of another type or macro definition required by this one
    #[xml(attribute())]
    pub requires: Option<&'a str>,
    #[xml(attribute())]
    pub deprecated: Option<bool>,
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    pub deprecation_comment: Option<&'a str>,
    pub is_disabled: bool,
    pub value: MacroDefineValue<'a>,
}

/// <type category="define">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "type")]
pub enum DefineType<'a> {
    #[xml(discriminant(attr = "name"))]
    GuardedMacro(GuardedDefine<'a>),
    Macro(MacroDefine<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum MacroDefineValue<'a> {
    Expression(Expression<'a>),
    FunctionDefine {
        params: Vec<&'a str>,
        expression: Vec<Token<'a>>,
    },
    MacroFunctionCall {
        name: &'a str,
        args: Vec<Expression<'a>>,
    },
}

impl<'a> MacroDefineValue<'a> {
    #[must_use]
    pub fn as_expr(&self) -> Option<Cow<'_, Expression<'a>>> {
        match self {
            Self::Expression(e) => Some(Cow::Borrowed(e)),
            Self::MacroFunctionCall { name, args } => Some(Cow::Owned(Expression::FunctionCall(
                Box::new(Expression::Identifier(name)),
                args.clone(),
            ))),
            _ => None,
        }
    }
}

// NOTE: IOSurfaceRef is incorrectly `typedef struct __IOSurface* <name>IOSurfaceRef</name>;`
//       but should be  `typedef struct <type>__IOSurface</type>* <name>IOSurfaceRef</name>;`
/// <type category="basetype">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "type", tokenized)]
pub enum BaseTypeType<'a> {
    /// Forward declaration of a struct
    Forward(&'a str),
    /// C typedef defining a type alias
    TypeDef(FieldLike<'a>),
    DefineGuarded {
        /// C pre-processor tokens before the name tag
        pre: Vec<Token<'a>>,
        /// Name of the type defined
        name: &'a str,
        /// C pre-processor tokens after the name tag
        post: Vec<Token<'a>>,
    },
}

impl<'a> BaseTypeType<'a> {
    #[must_use]
    pub fn name(&self) -> &'a str {
        match self {
            BaseTypeType::Forward(name)
            | BaseTypeType::TypeDef(FieldLike { name, .. })
            | BaseTypeType::DefineGuarded { name, .. } => name,
        }
    }
}

/// <type category="bitmask">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct BitmaskType<'a> {
    /// name of this type
    pub name: &'a str,
    pub is_64bits: bool,
    pub has_bitvalues: bool,
    pub api: Option<VulkanApi>,
}

impl<'a> BitmaskType<'a> {
    #[must_use]
    pub fn type_name(&self) -> &'static str {
        if self.is_64bits {
            "VkFlags64"
        } else {
            "VkFlags"
        }
    }

    /// name of an enum definition that defines the valid values for parameters of this type
    #[must_use]
    pub fn bitvalues(&self) -> Option<String> {
        self.has_bitvalues
            .then(|| self.name.replace("Flags", "FlagBits"))
    }
}

impl<'a> crate::Tagged for BitmaskType<'a> {
    const TAG: &'static str = "type";
}

/// <type category="handle">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tokenized, tag = "type")]
pub struct HandleType<'a> {
    /// name of this type
    pub name: &'a str,
    pub handle_kind: HandleKind,
    /// name of VK_OBJECT_TYPE_* API enumerant which corresponds to this type.
    #[xml(attribute(rename = "objtypeenum"))]
    pub obj_type_enum: &'a str,
    /// Notes another handle type that acts as a parent object for this type.
    #[xml(attribute())]
    pub parent: Option<&'a str>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::IntoStaticStr, strum::Display,
)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum HandleKind {
    #[strum(serialize = "VK_DEFINE_HANDLE")]
    Dispatch,
    #[strum(serialize = "VK_DEFINE_NON_DISPATCHABLE_HANDLE")]
    NoDispatch,
}

// FIXME Remove ASAP
impl<'a> Into<UnescapedStr<'a>> for HandleKind {
    fn into(self) -> UnescapedStr<'a> {
        UnescapedStr(Cow::Borrowed(self.into()))
    }
}

/// <type category="enum">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "type")]
pub struct EnumType<'a> {
    /// name of this type
    #[xml(attribute())]
    pub name: &'a str,
}

/// <type category="funcpointer">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tokenized, tag = "type")]
pub struct FnPtrType<'a> {
    /// name of this type
    pub name: &'a str,
    pub return_type_name: TypeSpecifier<'a>,
    pub return_type_pointer_kind: Option<PointerKind>,
    /// name of another type definition required by this one
    #[xml(attribute())]
    pub requires: Option<&'a str>,
    pub params: Option<Vec<FieldLike<'a>>>,
}

/// <type category="struct">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "type")]
pub struct StructType<'a> {
    /// name of this type
    #[xml(attribute())]
    pub name: &'a str,
    #[xml(child)]
    pub members: CommentendChildren<'a, Member<'a>>,
    /// Notes that this struct is going to be filled in by the API, rather than an application filling it out and passing it to the API.
    #[xml(attribute(rename = "returnedonly"))]
    pub returned_only: Option<bool>,
    /// Lists parent structures which this structure may extend via the `pNext` chain of the parent.
    #[xml(attribute(seperator = "crate::CommaSeperator", rename = "structextends"))]
    pub struct_extends: Option<Vec<&'a str>>,
    /// `pNext` can include multiple structures of this type.
    #[xml(attribute(rename = "allowduplicate"))]
    pub allow_duplicate: Option<bool>,
    /// name of another type definition required by this one
    #[xml(attribute())]
    pub requires: Option<&'a str>,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
}

/// <type category="union">
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "type")]
pub struct UnionType<'a> {
    /// name of this type
    #[xml(attribute())]
    pub name: &'a str,
    #[xml(child)]
    pub members: CommentendChildren<'a, Member<'a>>,
    /// Notes that this union is going to be filled in by the API, rather than an application filling it out and passing it to the API.
    #[xml(attribute(rename = "returnedonly"))]
    pub returned_only: Option<bool>,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
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
#[non_exhaustive]
pub enum MemberSelector {
    #[strum(serialize = "type")]
    Type,
    #[strum(serialize = "format")]
    Format,
    #[strum(serialize = "geometryType")]
    GeometryType,
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
#[non_exhaustive]
pub enum MemberLimitType {
    #[strum(serialize = "min")]
    Minimum,
    #[strum(serialize = "max")]
    Maximum,
    #[strum(serialize = "pot")]
    PowerOfTwo,
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
    MinimumPowerOfTwo,
    #[strum(serialize = "max,pot")]
    MaximumPowerOfTwo,
    #[strum(serialize = "min,mul")]
    MinimumMul,
}

/// <member>
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "member")]
pub struct Member<'a> {
    #[xml(flatten)]
    pub base: FieldLike<'a>,
    /// for a union member, identifies a separate enum member that selects which of the union's members are valid
    #[xml(attribute())]
    pub selector: Option<MemberSelector>,
    /// for a member of a union, identifies an enum value indicating the member is valid
    #[xml(attribute())]
    pub selection: Option<&'a str>,
    /// list of legal values, usually used only for `sType` enums
    #[xml(attribute())]
    pub values: Option<&'a str>,
    /// Specifies the type of a device limit.
    /// only applicable for members of VkPhysicalDeviceProperties and VkPhysicalDeviceProperties2, their substructures, and extensions.
    #[xml(attribute(rename = "limittype"))]
    pub limit_type: Option<MemberLimitType>,
}

impl<'a> ops::Deref for Member<'a> {
    type Target = FieldLike<'a>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a> ops::DerefMut for Member<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

impl<'a, 'de: 'a> crate::TryFromXML<'de> for IncludeType<'a> {
    fn try_from_xml<'input: 'de>(node: Node<'de, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(IncludeType {
            name: crate::try_from_attribute(node, "name")?,
            is_local_include: node.text().map(|s| s.contains('"')),
        }))
    }
}

impl<'a> crate::IntoXMLElement for IncludeType<'a> {
    fn write_element<'w, W: ?Sized + crate::XMLWriter>(
        &self,
        element: crate::XMLElementBuilder<'static, 'w, W>,
    ) -> Result<(), W::Error> {
        let Self {
            name,
            is_local_include,
        } = self;
        let elem = element.with_escaped_attribute("name", name)?;
        match is_local_include {
            Some(true) if name.ends_with(".h") => {
                elem.write_escaped_text(&format_args!("#include \"{name}\""))
            }
            Some(true) => elem.write_escaped_text(&format_args!("#include \"{name}.h\"")),
            Some(false) if name.ends_with(".h") => {
                elem.write_escaped_text(&format_args!("#include &lt;{name}&gt;"))
            }
            Some(false) => elem.write_escaped_text(&format_args!("#include &lt;{name}.h&gt;")),
            None => elem.write_empty(),
        }
    }
}

impl<'a, 'de: 'a> crate::TryFromXML<'de> for BitmaskType<'a> {
    fn try_from_xml<'input: 'de>(node: Node<'de, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(BitmaskType {
            //  FIXME add check that name.replace("Flags", "FlagBits") == attribute("requires").xor(attribute("bitvalues"))
            has_bitvalues: node.has_attribute("requires") || node.has_attribute("bitvalues"),
            api: crate::try_from_attribute(node, "api")?,
            ..TryFromTokens::try_from_node(node)?
        }))
    }
}

impl<'a> crate::IntoXMLElement for BitmaskType<'a> {
    fn write_element<'w, W: ?Sized + crate::XMLWriter>(
        &self,
        element: crate::XMLElementBuilder<'static, 'w, W>,
    ) -> Result<(), W::Error> {
        element // TODO requires / bitvalues
            .with_escaped_attribute("bitvalues", &self.bitvalues().as_deref())?
            .with_escaped_attribute("api", &self.api.as_ref())?
            .write_tokens(self)
    }
}
