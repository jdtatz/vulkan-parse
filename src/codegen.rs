use std::num::NonZeroU8;

use crate::{
    typed_registry::{
        BaseTypeType, BitmaskEnum, BitmaskType, Command, CommentendChildren, ConstantEnum,
        DefineType, DefinitionOrAlias, EnumType, Enums, EnumsValues, Extension, Feature, FnPtrType,
        Format, HandleType, IncludeType, Items, Platform, PseudoExtension, Registry, RequiresType,
        SpirvCapability, SpirvExtension, StructType, Tag, Type, UnionType, UnusedEnum, ValueEnum,
        WrappedExtension,
    },
    UnescapedStr,
};

#[derive(Debug, Clone)]
pub enum CodegenUnit<'a> {
    Platform(Platform<'a>),
    Tag(Tag<'a>),
    RequiresType(RequiresType<'a>),
    IncludeType(IncludeType<'a>),
    DefineType(DefineType<'a>),
    BaseType(BaseTypeType<'a>),
    BitmaskType(DefinitionOrAlias<'a, BitmaskType<'a>>),
    HandleType(DefinitionOrAlias<'a, HandleType<'a>>),
    EnumType(DefinitionOrAlias<'a, EnumType<'a>>),
    FnPtr(FnPtrType<'a>),
    StructType(DefinitionOrAlias<'a, StructType<'a>>),
    UnionType(UnionType<'a>),
    /// Hardcoded constants, not an enumerated type
    Constant(DefinitionOrAlias<'a, ConstantEnum<'a>>),
    /// An enum where each value is distinct
    Enum {
        /// name of the corresponding <type> associated with this group
        name: &'a str,
        /// bit width of the enum value type. If omitted, a default value of 32 is used.
        bit_width: Option<NonZeroU8>,
        /// descriptive text with no semantic meaning
        comment: Option<UnescapedStr<'a>>,
        values: CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>,
        unused_values: Option<UnusedEnum<'a>>,
    },
    /// An enum where the values constitute a bitmask
    Bitmask {
        /// name of the corresponding <type> associated with this group
        name: &'a str,
        /// bit width of the enum value type. If omitted, a default value of 32 is used.
        bit_width: Option<NonZeroU8>,
        /// descriptive text with no semantic meaning
        comment: Option<UnescapedStr<'a>>,
        values: CommentendChildren<'a, DefinitionOrAlias<'a, BitmaskEnum<'a>>>,
    },
    Command(DefinitionOrAlias<'a, Command<'a>>),
    Feature(Feature<'a>),
    Extension(Extension<'a>),
    PseudoExtension(PseudoExtension<'a>),
    Format(Format<'a>),
    SpirvExtension(SpirvExtension<'a>),
    SpirvCapability(SpirvCapability<'a>),
}

impl<'a> From<Platform<'a>> for CodegenUnit<'a> {
    fn from(value: Platform<'a>) -> Self {
        Self::Platform(value)
    }
}

impl<'a> From<Tag<'a>> for CodegenUnit<'a> {
    fn from(value: Tag<'a>) -> Self {
        Self::Tag(value)
    }
}

impl<'a> From<Type<'a>> for CodegenUnit<'a> {
    fn from(value: Type<'a>) -> Self {
        match value {
            Type::Requires(ty) => Self::RequiresType(ty),
            Type::Include(ty) => Self::IncludeType(ty),
            Type::Define(ty) => Self::DefineType(ty),
            Type::BaseType(ty) => Self::BaseType(ty),
            Type::Bitmask(ty) => Self::BitmaskType(ty),
            Type::Handle(ty) => Self::HandleType(ty),
            Type::Enum(ty) => Self::EnumType(ty),
            Type::FnPtr(ty) => Self::FnPtr(ty),
            Type::Struct(ty) => Self::StructType(ty),
            Type::Union(ty) => Self::UnionType(ty),
        }
    }
}

impl<'a> From<DefinitionOrAlias<'a, ConstantEnum<'a>>> for CodegenUnit<'a> {
    fn from(value: DefinitionOrAlias<'a, ConstantEnum<'a>>) -> Self {
        Self::Constant(value)
    }
}

// impl<'a> From<Enums<'a>> for CodegenUnit<'a> {
//     fn from(value: Enums<'a>) -> Self {
//         todo!()
//     }
// }

impl<'a> From<DefinitionOrAlias<'a, Command<'a>>> for CodegenUnit<'a> {
    fn from(value: DefinitionOrAlias<'a, Command<'a>>) -> Self {
        Self::Command(value)
    }
}

impl<'a> From<Feature<'a>> for CodegenUnit<'a> {
    fn from(value: Feature<'a>) -> Self {
        Self::Feature(value)
    }
}

impl<'a> From<WrappedExtension<'a>> for CodegenUnit<'a> {
    fn from(value: WrappedExtension<'a>) -> Self {
        match value {
            WrappedExtension::Extension(e) => Self::Extension(e),
            WrappedExtension::PseudoExtension(e) => Self::PseudoExtension(e),
        }
    }
}

impl<'a> From<Format<'a>> for CodegenUnit<'a> {
    fn from(value: Format<'a>) -> Self {
        Self::Format(value)
    }
}

impl<'a> From<SpirvExtension<'a>> for CodegenUnit<'a> {
    fn from(value: SpirvExtension<'a>) -> Self {
        Self::SpirvExtension(value)
    }
}

impl<'a> From<SpirvCapability<'a>> for CodegenUnit<'a> {
    fn from(value: SpirvCapability<'a>) -> Self {
        Self::SpirvCapability(value)
    }
}

impl<'a> From<Registry<'a>> for Vec<CodegenUnit<'a>> {
    fn from(registry: Registry<'a>) -> Self {
        let mut units = Vec::new();
        for items in registry.registry.into_values() {
            match items {
                Items::Platforms {
                    platforms,
                    comment: _,
                } => units.extend(platforms.into_iter().map(CodegenUnit::from)),
                Items::Tags { tags, .. } => units.extend(tags.into_iter().map(CodegenUnit::from)),
                Items::Types { types, .. } => {
                    units.extend(types.into_values().map(CodegenUnit::from))
                }
                Items::Enums(Enums {
                    values: EnumsValues::Constants { constants },
                    ..
                }) => units.extend(constants.into_values().map(CodegenUnit::from)),
                Items::Enums(Enums {
                    name,
                    bit_width,
                    comment,
                    values:
                        EnumsValues::Enum {
                            values,
                            unused_values,
                        },
                }) => units.push(CodegenUnit::Enum {
                    name,
                    bit_width,
                    comment,
                    values,
                    unused_values,
                }),
                Items::Enums(Enums {
                    name,
                    bit_width,
                    comment,
                    values: EnumsValues::Bitmask { values },
                }) => units.push(CodegenUnit::Bitmask {
                    name,
                    bit_width,
                    comment,
                    values,
                }),
                Items::Commands {
                    commands,
                    comment: _,
                } => units.extend(commands.into_values().map(CodegenUnit::from)),
                Items::Features(feature) => units.push(CodegenUnit::from(feature)),
                Items::Extensions {
                    extensions,
                    comment: _,
                } => units.extend(extensions.into_iter().map(CodegenUnit::from)),
                Items::Formats { formats } => {
                    units.extend(formats.into_iter().map(CodegenUnit::from))
                }
                Items::SpirvExtensions {
                    extensions,
                    comment: _,
                } => units.extend(extensions.into_iter().map(CodegenUnit::from)),
                Items::SpirvCapabilities {
                    capabilities,
                    comment: _,
                } => units.extend(capabilities.into_iter().map(CodegenUnit::from)),
            }
        }
        units
    }
}
