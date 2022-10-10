use std::{borrow::Cow, num::NonZeroU8};

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct Format<'a> {
    pub name: Cow<'a, str>,
    pub class: Cow<'a, str>,
    pub block_size: NonZeroU8,
    pub texels_per_block: NonZeroU8,
    pub block_extent: Option<[NonZeroU8; 3]>,
    pub packed: Option<NonZeroU8>,
    pub compressed: Option<FormatCompressionType>,
    pub chroma: Option<FormatChroma>,
    pub children: Box<[FormatChild<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum FormatChild<'a> {
    Component {
        name: ComponentName,
        bits: ComponentBits,
        numeric_format: ComponentNumericFormat,
        plane_index: Option<u8>,
    },

    Plane {
        index: u8,
        width_divisor: NonZeroU8,
        height_divisor: NonZeroU8,
        compatible: Cow<'a, str>,
    },

    SpirvImageFormat {
        name: Cow<'a, str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum FormatChroma {
    Type420,
    Type422,
    Type444,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum FormatCompressionType {
    BC,
    ETC2,
    EAC,
    // FIXME I'm really not sure what else to name these
    /// Adaptive Scalable Texture Compression (ASTC) Low Dynamic Range (LDR)
    #[allow(non_camel_case_types)]
    ASTC_LDR,
    /// Adaptive Scalable Texture Compression (ASTC) High Dynamic Range (HDR)
    #[allow(non_camel_case_types)]
    ASTC_HDR,
    PVRTC,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum ComponentBits {
    Compressed,
    Bits(NonZeroU8),
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum ComponentNumericFormat {
    SRGB,
    UNORM,
    SNORM,
    UINT,
    SINT,
    USCALED,
    SSCALED,
    UFLOAT,
    SFLOAT,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum ComponentName {
    A,
    R,
    G,
    B,
    S,
    D,
}
