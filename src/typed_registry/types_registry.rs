use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    ops,
    str::FromStr,
};

use roxmltree::Node;

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{
    attribute, parse_children, tokenize, try_attribute, try_attribute_fs, try_attribute_sep,
    Expression, Parse, ParseResult, Token, TryFromTokens, TypeSpecifier,
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum DynamicLength<'a> {
    NullTerminated,
    // FIXME only found in VkAccelerationStructureBuildGeometryInfoKHR->ppGeometries, is this a mistake?
    Static(NonZeroU32),
    Parameterized(&'a str),
    ParameterizedField { parameter: &'a str, field: &'a str },
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl<'a> TryFrom<&'a str> for ExternSyncField<'a> {
    type Error = ExternSyncParseError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExternSyncKind<'a> {
    /// externsync="true"
    True,
    Fields(Vec<ExternSyncField<'a>>),
}

impl<'a> TryFrom<&'a str> for ExternSyncKind<'a> {
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
            ExternSyncKind::Fields(fs) => crate::fmt_write_interspersed(f, fs.iter(), ","),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum NoAutoValidityKind {
    /// noautovalidity="true"
    #[strum(serialize = "true")]
    True,
}

// TODO Bikeshed needed
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum FieldLikeSizing<'a> {
    BitfieldSize(NonZeroU8),
    ArrayShape(Vec<ArrayLength<'a>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct FieldLike<'a> {
    pub name: &'a str,
    pub type_name: TypeSpecifier<'a>,
    /// if true, then either a const pointer or array
    pub is_const: bool,
    pub pointer_kind: Option<PointerKind>,
    pub sizing: Option<FieldLikeSizing<'a>>,
    pub dynamic_shape: Option<DynamicShapeKind<'a>>,
    /// denotes that the member should be externally synchronized when accessed by Vulkan
    pub extern_sync: Option<ExternSyncKind<'a>>,
    /// whether this value can be omitted by providing NULL (for pointers), VK_NULL_HANDLE (for handles) or 0 (for bitmasks/values)
    pub optional: Option<OptionalKind>,
    /// no automatic validity language should be generated
    pub no_auto_validity: Option<NoAutoValidityKind>,
    /// The field-like that paramertizes what type of vulkan handle this one is.
    /// => This field-like is a generic vulkan handle, and it is an error if `type_name` isn't `uint64_t`
    pub object_type: Option<&'a str>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct RequiresType<'a> {
    /// name of this type
    pub name: &'a str,
    /// name of another type definition required by this one
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

/// <type category="define" name="...">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct GuardedDefine<'a> {
    pub name: &'a str,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// name of another type definition required by this one
    pub requires: Option<&'a str>,
    pub code: Vec<Token<'a>>,
}

/// <type category="define">...<name>...</name>...</type>
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct MacroDefine<'a> {
    /// name of defined macro
    pub name: &'a str,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// name of another type or macro definition required by this one
    pub requires: Option<&'a str>,
    pub deprecation_comment: Option<&'a str>,
    pub is_disabled: bool,
    pub value: MacroDefineValue<'a>,
}

/// <type category="define">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum DefineType<'a> {
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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
    pub fn name(&self) -> &&'a str {
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

/// <type category="handle">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct HandleType<'a> {
    /// name of this type
    pub name: &'a str,
    pub handle_kind: HandleKind,
    /// name of VK_OBJECT_TYPE_* API enumerant which corresponds to this type.
    pub obj_type_enum: &'a str,
    /// Notes another handle type that acts as a parent object for this type.
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

/// <type category="enum">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct EnumType<'a> {
    /// name of this type
    pub name: &'a str,
}

/// <type category="funcpointer">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct FnPtrType<'a> {
    /// name of this type
    pub name: &'a str,
    pub return_type_name: TypeSpecifier<'a>,
    pub return_type_pointer_kind: Option<PointerKind>,
    /// name of another type definition required by this one
    pub requires: Option<&'a str>,
    pub params: Option<Vec<FieldLike<'a>>>,
}

/// <type category="struct">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct StructType<'a> {
    /// name of this type
    pub name: &'a str,
    pub members: CommentendChildren<'a, Member<'a>>,
    /// Notes that this struct is going to be filled in by the API, rather than an application filling it out and passing it to the API.
    pub returned_only: Option<bool>,
    /// Lists parent structures which this structure may extend via the `pNext` chain of the parent.
    pub struct_extends: Option<Vec<&'a str>>,
    /// `pNext` can include multiple structures of this type.
    pub allow_duplicate: Option<bool>,
    /// name of another type definition required by this one
    pub requires: Option<&'a str>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

/// <type category="union">
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct UnionType<'a> {
    /// name of this type
    pub name: &'a str,
    pub members: CommentendChildren<'a, Member<'a>>,
    /// Notes that this union is going to be filled in by the API, rather than an application filling it out and passing it to the API.
    pub returned_only: Option<bool>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display)]
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

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumString, strum::Display)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Member<'a> {
    pub base: FieldLike<'a>,
    /// for a union member, identifies a separate enum member that selects which of the union's members are valid
    pub selector: Option<MemberSelector>,
    /// for a member of a union, identifies an enum value indicating the member is valid
    pub selection: Option<&'a str>,
    /// list of legal values, usually used only for `sType` enums
    pub values: Option<&'a str>,
    /// Specifies the type of a device limit.
    /// only applicable for members of VkPhysicalDeviceProperties and VkPhysicalDeviceProperties2, their substructures, and extensions.
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

impl<'a, 'input> Parse<'a, 'input> for GuardedDefine<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(name) = try_attribute(node, "name")? {
            let code = node
                .text()
                .ok_or_else(|| crate::ErrorKind::EmptyElement(node.id()))?;
            Ok(Some(Self {
                name,
                comment: try_attribute(node, "comment")?,
                requires: try_attribute(node, "requires")?,
                code: tokenize(code, true, true)
                    .collect::<Result<_, _>>()
                    .map_err(|e| crate::ErrorKind::LexerError(e, node.id()))?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for MacroDefine<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(Self {
            requires: try_attribute(node, "requires")?,
            ..Self::try_from_node(node)?
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for DefineType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(if let Some(g) = GuardedDefine::try_parse(node)? {
            Some(Self::GuardedMacro(g))
        } else {
            MacroDefine::try_parse(node)?.map(Self::Macro)
        })
    }
}

impl<'a, 'input> Parse<'a, 'input> for BaseTypeType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(Self::try_from_node(node)?))
    }
}

impl<'a, 'input> Parse<'a, 'input> for BitmaskType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(BitmaskType {
            //  FIXME add check that name.replace("Flags", "FlagBits") == attribute("requires").xor(attribute("bitvalues"))
            has_bitvalues: node.has_attribute("requires") || node.has_attribute("bitvalues"),
            ..TryFromTokens::try_from_node(node)?
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for HandleType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(HandleType {
            obj_type_enum: attribute(node, "objtypeenum")?,
            parent: try_attribute(node, "parent")?,
            ..TryFromTokens::try_from_node(node)?
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
        Ok(Some(Self {
            requires: try_attribute(node, "requires")?,
            ..Self::try_from_node(node)?
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for StructType<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(StructType {
            name: attribute(node, "name")?,
            returned_only: try_attribute_fs(node, "returnedonly")?,
            struct_extends: try_attribute_sep::<_, ','>(node, "structextends")?,
            allow_duplicate: try_attribute_fs(node, "allowduplicate")?,
            members: parse_children(node)?,
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
            members: parse_children(node)?,
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
        let dynamic_shape = try_attribute(node, "len")?
            .map(|len: &str| {
                Ok(if let Some(latex_expr) = len.strip_prefix("latexmath:") {
                    // let c_expr = try_attribute(node, "altlen").transpose().expect("The `altlen` attribute is required when the `len` attribute is a latex expression");
                    let c_expr = attribute(node, "altlen")?;
                    DynamicShapeKind::Expression {
                        latex_expr: (latex_expr),
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
            ..Self::try_from_node(node)?
        }))
    }
}
