use std::{fmt, io::Write};

use quick_xml::{
    escape::escape,
    events::{attributes::Attribute, BytesDecl, BytesStart, BytesText, Event},
    writer::Writer,
    Error,
};

use crate::{
    Alias, BaseTypeType, BitPosEnum, BitmaskEnum, BitmaskType, Command, CommandParam, Comment,
    ConstantEnum, DefineType, DefinitionOrAlias, DynamicShapeKind, EnableSpirvCapability, EnumType,
    Enums, EnumsValues, Extension, ExtensionEnable, Feature, FieldLike, FnPtrType, Format,
    FormatChild, GuardedDefine, HandleKind, HandleType, ImplicitExternSyncParam,
    ImplicitExternSyncParams, IncludeType, IntoVkXMLTokens, Items, MacroDefine, MaybeComment,
    Member, Platform, PropertyEnable, Proto, PseudoExtension, Registry, Require, RequireEnum,
    RequireValue, RequireValueEnum, RequiresType, Seperated, SpirvCapability, SpirvExtension,
    StructEnable, StructType, Tag, Type, UnionType, UnusedEnum, ValueEnum, VersionEnable,
    VkXMLToken, WrappedExtension,
};

type Result = std::result::Result<(), Error>;

pub fn into_xml<W: Write>(reg: &Registry, to: W) -> Result {
    let mut writer = Writer::new(to);
    writer.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))?;
    reg.write_xml(&mut writer)
}

// FIXME This makes ElementWriter::write_inner_content take a FnOnce instead of a Fn
struct ElementWriter2<'a, W: Write> {
    writer: &'a mut Writer<W>,
    start_tag: BytesStart<'a>,
}

impl<'a, W: Write> ElementWriter2<'a, W> {
    pub fn with_attribute<'b, I>(mut self, attr: I) -> Self
    where
        I: Into<Attribute<'b>>,
    {
        self.start_tag.push_attribute(attr);
        self
    }

    pub fn write_text_content(
        self,
        text: BytesText,
    ) -> std::result::Result<&'a mut Writer<W>, Error> {
        self.writer
            .write_event(Event::Start(self.start_tag.borrow()))?;
        self.writer.write_event(Event::Text(text))?;
        self.writer
            .write_event(Event::End(self.start_tag.to_end()))?;
        Ok(self.writer)
    }

    pub fn write_empty(self) -> std::result::Result<&'a mut Writer<W>, Error> {
        self.writer.write_event(Event::Empty(self.start_tag))?;
        Ok(self.writer)
    }

    pub fn write_inner_content<F>(self, closure: F) -> std::result::Result<&'a mut Writer<W>, Error>
    where
        F: FnOnce(&mut Writer<W>) -> Result,
    {
        self.writer
            .write_event(Event::Start(self.start_tag.borrow()))?;
        closure(self.writer)?;
        self.writer
            .write_event(Event::End(self.start_tag.to_end()))?;
        Ok(self.writer)
    }
}

trait WriterExt<W: Write> {
    #[must_use]
    fn create_element2<'a, N>(&'a mut self, name: &'a N) -> ElementWriter2<W>
    where
        N: 'a + AsRef<str> + ?Sized;
}

impl<W: Write> WriterExt<W> for Writer<W> {
    fn create_element2<'a, N>(&'a mut self, name: &'a N) -> ElementWriter2<W>
    where
        N: 'a + AsRef<str> + ?Sized,
    {
        ElementWriter2 {
            writer: self,
            start_tag: BytesStart::new(name.as_ref()),
        }
    }
}

trait ElementWriterExt<'a, W: Write>: Sized {
    fn with_fmt_attribute<V: fmt::Display>(self, attr_key: &'a str, attr_value: V) -> Self;

    fn with_opt_attribute<V: fmt::Display>(
        self,
        attr_key: &'a str,
        opt_attr_value: Option<V>,
    ) -> Self {
        if let Some(attr_value) = opt_attr_value {
            self.with_fmt_attribute(attr_key, attr_value)
        } else {
            self
        }
    }

    fn write_empty_(self) -> Result;

    fn write_escaped_text(self, text: &str) -> Result;

    fn write_inner_content_<F>(self, closure: F) -> Result
    where
        F: FnOnce(&mut Writer<W>) -> Result;

    // fn write_children<I>(self, children: I) -> Result
    // where
    //     I: IntoIterator,
    //     I::Item: IntoXML,
    // {
    //     self.write_inner_content_(move |writer| {
    //         for child in children {
    //             child.write_xml(writer)?;
    //         }
    //         Ok(())
    //     })
    // }

    fn write_children<T: IntoXML>(self, children: &[T]) -> Result {
        self.write_inner_content_(move |writer| {
            for child in children {
                child.write_xml(writer)?;
            }
            Ok(())
        })
    }
}

impl<'a, W: Write> ElementWriterExt<'a, W> for ElementWriter2<'a, W> {
    fn with_fmt_attribute<'b, V: fmt::Display>(self, attr_key: &'a str, attr_value: V) -> Self {
        let s = attr_value.to_string();
        self.with_attribute(Attribute {
            key: quick_xml::name::QName(attr_key.as_bytes()),
            value: escape(&s).into_owned().into_bytes().into(),
        })
    }

    fn write_empty_(self) -> Result {
        self.write_empty()?;
        Ok(())
    }

    fn write_escaped_text(self, text: &str) -> Result {
        self.write_text_content(BytesText::new(text))?;
        Ok(())
    }

    fn write_inner_content_<F>(self, closure: F) -> Result
    where
        F: FnOnce(&mut Writer<W>) -> Result,
    {
        self.write_inner_content(closure)?;
        Ok(())
    }
}

trait IntoXML {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result;
}

trait IntoXMLElement {
    const TAG: &'static str;

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result;
}

impl<E: IntoXMLElement> IntoXML for E {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        self.write_element(Self::add_static_attrs(
            writer.create_element2(<Self as IntoXMLElement>::TAG),
        ))
    }
}

impl<'a, E: IntoXMLElement> IntoXMLElement for &'a E {
    const TAG: &'static str = E::TAG;

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        E::add_static_attrs(element)
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        (*self).write_element(element)
    }
}

impl<'a> IntoXMLElement for Alias<'a> {
    const TAG: &'static str = "";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Alias {
            name,
            alias,
            comment,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_fmt_attribute("alias", alias)
            .with_opt_attribute("comment", comment.as_deref())
            .write_empty_()
    }
}

impl<'a, T: IntoXMLElement> IntoXMLElement for DefinitionOrAlias<'a, T> {
    const TAG: &'static str = T::TAG;

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        T::add_static_attrs(element)
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        match self {
            DefinitionOrAlias::Alias(alias) => alias.write_element(element),
            DefinitionOrAlias::Definition(defn) => defn.write_element(element),
        }
    }
}

impl<'a, T: IntoXML> IntoXML for MaybeComment<'a, T> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            MaybeComment::Value(value) => value.write_xml(writer),
            MaybeComment::Comment(comment) => comment.write_xml(writer),
        }
    }
}

impl<'a> IntoXMLElement for Comment<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element.write_escaped_text(&self.0)
    }

    const TAG: &'static str = "comment";
}

// Specfics

impl<'a> IntoXMLElement for Registry<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element.write_children(&self.0)
    }

    const TAG: &'static str = "registry";
}

impl<'a> IntoXML for Items<'a> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            Items::Platforms { platforms, comment } => writer
                .create_element2("platforms")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(platforms),
            Items::Tags { tags, comment } => writer
                .create_element2("tags")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(tags),
            Items::Types { types, comment } => writer
                .create_element2("types")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(types),
            Items::Enums(enums) => enums.write_xml(writer),
            Items::Commands { commands, comment } => writer
                .create_element2("commands")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(commands),
            Items::Features(features) => features.write_xml(writer),
            Items::Extensions {
                extensions,
                comment,
            } => writer
                .create_element2("extensions")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(extensions),
            Items::Formats(formats) => writer.create_element2("formats").write_children(formats),
            Items::SpirvExtensions {
                extensions,
                comment,
            } => writer
                .create_element2("spirvextensions")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(extensions),
            Items::SpirvCapabilities {
                capabilities,
                comment,
            } => writer
                .create_element2("spirvcapabilities")
                .with_opt_attribute("comment", comment.as_deref())
                .write_children(capabilities),
        }
    }
}

impl<'a> IntoXMLElement for Platform<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element
            .with_fmt_attribute("name", &self.name)
            .with_fmt_attribute("protect", &self.protect)
            .with_opt_attribute("comment", self.comment.as_deref())
            .write_empty_()
    }

    const TAG: &'static str = "platform";
}

impl<'a> IntoXMLElement for Tag<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element
            .with_fmt_attribute("name", &self.name)
            .with_fmt_attribute("author", &self.author)
            .with_fmt_attribute("contact", &self.contact)
            .write_empty_()
    }

    const TAG: &'static str = "tag";
}

fn write_tokens<'t, W: Write>(
    tokens: impl IntoIterator<Item = VkXMLToken<'t>>,
    writer: &mut Writer<W>,
) -> Result {
    let mut last_ident_like = false;
    for token in tokens {
        match token {
            VkXMLToken::C(token) => {
                let is_ident_like = token.is_ident_like();
                let s = if last_ident_like && is_ident_like {
                    format!(" {token}")
                } else {
                    token.to_string()
                };
                writer.write_event(Event::Text(BytesText::new(&s)))?;
                last_ident_like = is_ident_like;
            }
            VkXMLToken::TextTag { name, text } => {
                last_ident_like = false;
                writer
                    .create_element2(name.as_ref())
                    .write_escaped_text(text.as_ref())?;
            }
        }
    }
    Ok(())
}

impl<'a> IntoXMLElement for FieldLike<'a> {
    const TAG: &'static str = "";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let element = match &self.dynamic_shape {
            Some(DynamicShapeKind::Expression { latex_expr, c_expr }) => element
                .with_fmt_attribute("len", format_args!("latexmath:{latex_expr}"))
                .with_fmt_attribute("altlen", c_expr),
            Some(DynamicShapeKind::Double(l1, l2)) => {
                element.with_fmt_attribute("len", format_args!("{l1},{l2}"))
            }
            Some(DynamicShapeKind::Single(l)) => element.with_fmt_attribute("len", l),
            None => element,
        };
        element
            .with_opt_attribute("optional", self.optional.as_ref())
            .with_opt_attribute("noautovalidity", self.no_auto_validity.as_ref())
            .with_opt_attribute("externsync", self.extern_sync.as_ref())
            .with_opt_attribute("objecttype", self.object_type.as_ref())
            .write_inner_content_(|writer| write_tokens(self.to_tokens_vector(), writer))
    }
}

impl<'a> IntoXML for Type<'a> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            Type::Requires(ty) => ty.write_xml(writer),
            Type::Include(ty) => ty.write_xml(writer),
            Type::Define(ty) => ty.write_xml(writer),
            Type::BaseType(ty) => ty.write_xml(writer),
            Type::Bitmask(ty) => ty.write_xml(writer),
            Type::Handle(ty) => ty.write_xml(writer),
            Type::Enum(ty) => ty.write_xml(writer),
            Type::FnPtr(ty) => ty.write_xml(writer),
            Type::Struct(ty) => ty.write_xml(writer),
            Type::Union(ty) => ty.write_xml(writer),
        }
    }
}

impl<'a> IntoXMLElement for RequiresType<'a> {
    const TAG: &'static str = "type";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self { name, requires } = self;
        element
            .with_fmt_attribute("name", name)
            .with_opt_attribute("requires", requires.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for IncludeType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "include")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self {
            name,
            is_local_include,
        } = self;
        let elem = element.with_fmt_attribute("name", name);
        match is_local_include {
            Some(true) if name.ends_with(".h") => {
                elem.write_escaped_text(&format!("#include \"{name}\""))
            }
            Some(true) => elem.write_escaped_text(&format!("#include \"{name}.h\"")),
            Some(false) if name.ends_with(".h") => {
                elem.write_escaped_text(&format!("#include <{name}>"))
            }
            Some(false) => elem.write_escaped_text(&format!("#include <{name}.h>")),
            None => elem.write_empty_(),
        }
    }
}

impl<'a> IntoXMLElement for DefineType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "define")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        match self {
            DefineType::GuardedMacro(g) => g.write_element(element),
            DefineType::Macro(m) => m.write_element(element),
        }
    }
}

impl<'a> IntoXMLElement for GuardedDefine<'a> {
    const TAG: &'static str = "type";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element
            .with_fmt_attribute("name", &self.name)
            .with_opt_attribute("requires", self.requires.as_deref())
            .with_opt_attribute("comment", self.comment.as_deref())
            .write_inner_content_(|writer| {
                write_tokens(self.code.iter().cloned().map(VkXMLToken::C), writer)
            })
    }
}

impl<'a> IntoXMLElement for MacroDefine<'a> {
    const TAG: &'static str = "type";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element
            .with_opt_attribute("requires", self.requires.as_deref())
            .with_opt_attribute("comment", self.comment.as_deref())
            .write_inner_content_(|writer| write_tokens(self.to_tokens_vector(), writer))
    }
}

impl<'a> IntoXMLElement for BaseTypeType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "basetype")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element.write_inner_content_(|writer| write_tokens(self.to_tokens_vector(), writer))
    }
}

impl<'a> IntoXMLElement for BitmaskType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "bitmask")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self {
            name,
            is_64bits,
            has_bitvalues,
        } = self;
        element // TODO requires / bitvalues
            .with_opt_attribute(
                "bitvalues",
                has_bitvalues.then(|| name.replace("Flags", "FlagBits")),
            )
            .write_inner_content_(|writer| {
                writer.write_event(Event::Text(BytesText::new("typedef ")))?;
                writer
                    .create_element2("type")
                    .write_escaped_text(is_64bits.then_some("VkFlags64").unwrap_or("VkFlags"))?;
                writer.write_event(Event::Text(BytesText::new(" ")))?;
                writer.create_element2("name").write_escaped_text(name)?;
                writer.write_event(Event::Text(BytesText::new(";")))
            })
    }
}

impl<'a> IntoXMLElement for HandleType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "handle")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self {
            name,
            handle_kind,
            obj_type_enum,
            parent,
        } = self;
        element
            .with_opt_attribute("parent", parent.as_deref())
            .with_fmt_attribute("objtypeenum", obj_type_enum)
            .write_inner_content_(|writer| {
                writer
                    .create_element2("type")
                    .write_text_content(BytesText::new(match handle_kind {
                        HandleKind::Dispatch => "VK_DEFINE_HANDLE",
                        HandleKind::NoDispatch => "VK_DEFINE_NON_DISPATCHABLE_HANDLE",
                    }))?;
                writer.write_event(Event::Text(BytesText::new("(")))?;
                writer.create_element2("name").write_escaped_text(name)?;
                writer.write_event(Event::Text(BytesText::new(")")))
            })
    }
}

impl<'a> IntoXMLElement for EnumType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "enum")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self { name } = self;
        element.with_fmt_attribute("name", name).write_empty_()
    }
}

impl<'a> IntoXMLElement for FnPtrType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "funcpointer")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        element
            .with_opt_attribute("requires", self.requires.as_deref())
            .write_inner_content_(|writer| write_tokens(self.to_tokens_vector(), writer))
    }
}

impl<'a> IntoXMLElement for StructType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "struct")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self {
            name,
            members,
            returned_only,
            struct_extends,
            allow_duplicate,
            requires,
            comment,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_opt_attribute("returnedonly", returned_only.map(|b| b.to_string()))
            .with_opt_attribute(
                "structextends",
                struct_extends.as_deref().map(|se| se.join(",")),
            )
            .with_opt_attribute("allowduplicate", allow_duplicate.map(|b| b.to_string()))
            .with_opt_attribute("requires", requires.as_deref())
            .with_opt_attribute("comment", comment.as_deref())
            .write_children(members)
    }
}

impl<'a> IntoXMLElement for UnionType<'a> {
    const TAG: &'static str = "type";

    fn add_static_attrs<W: Write>(element: ElementWriter2<W>) -> ElementWriter2<W> {
        element.with_fmt_attribute("category", "union")
    }

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Self {
            name,
            members,
            returned_only,
            comment,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_opt_attribute("returnedonly", returned_only.map(|b| b.to_string()))
            .with_opt_attribute("comment", comment.as_deref())
            .write_children(members)
    }
}

impl<'a> IntoXMLElement for Member<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Member {
            base,
            selector,
            selection,
            values,
            limit_type,
        } = self;
        base.write_element(
            element
                .with_opt_attribute("selector", selector.as_ref())
                .with_opt_attribute("selection", selection.as_ref())
                .with_opt_attribute("values", values.as_ref())
                .with_opt_attribute("limittype", limit_type.as_ref()),
        )
    }

    const TAG: &'static str = "member";
}

impl<'a> IntoXMLElement for Enums<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Enums {
            name,
            bit_width,
            comment,
            values,
        } = self;
        let elem = element
            .with_fmt_attribute("name", name)
            .with_opt_attribute("bitwidth", bit_width.as_ref())
            .with_opt_attribute("comment", comment.as_deref());
        match values {
            EnumsValues::Constants(constants) => elem.write_children(constants),
            EnumsValues::Enum(values, unused) => elem
                .with_fmt_attribute("type", "enum")
                .write_inner_content_(|writer| {
                    for value in values.iter() {
                        value.write_xml(writer)?;
                    }
                    unused
                        .as_ref()
                        .map_or(Ok(()), |unused| unused.write_xml(writer))
                }),
            EnumsValues::Bitmask(bitmasks) => elem
                .with_fmt_attribute("type", "bitmask")
                .write_children(bitmasks),
        }
    }

    const TAG: &'static str = "enums";
}

impl<'a> IntoXMLElement for ConstantEnum<'a> {
    const TAG: &'static str = "enum";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let ConstantEnum {
            name,
            type_name,
            value,
            comment,
        } = self;
        element
            .with_fmt_attribute("type", type_name)
            .with_fmt_attribute("value", value)
            .with_fmt_attribute("name", name)
            .with_opt_attribute("comment", comment.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for ValueEnum<'a> {
    const TAG: &'static str = "enum";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let ValueEnum {
            name,
            value,
            comment,
        } = self;
        element
            .with_fmt_attribute("value", value)
            .with_fmt_attribute("name", name)
            .with_opt_attribute("comment", comment.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for BitmaskEnum<'a> {
    const TAG: &'static str = "enum";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        match self {
            BitmaskEnum::Value(value) => value.write_element(element),
            BitmaskEnum::BitPos(bitpos) => bitpos.write_element(element),
        }
    }
}

impl<'a> IntoXMLElement for BitPosEnum<'a> {
    const TAG: &'static str = "enum";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let BitPosEnum {
            name,
            bitpos,
            comment,
        } = self;
        element
            .with_fmt_attribute("bitpos", bitpos)
            .with_fmt_attribute("name", name)
            .with_opt_attribute("comment", comment.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for UnusedEnum<'a> {
    const TAG: &'static str = "unused";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let UnusedEnum { start, comment } = self;
        element
            .with_fmt_attribute("start", start)
            .with_opt_attribute("comment", comment.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for Format<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Format {
            name,
            class,
            block_size,
            texels_per_block,
            block_extent,
            packed,
            compressed,
            chroma,
            children,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_fmt_attribute("class", class)
            .with_fmt_attribute("blockSize", block_size)
            .with_fmt_attribute("texelsPerBlock", texels_per_block)
            .with_opt_attribute("blockExtent", block_extent.as_ref())
            .with_opt_attribute("packed", packed.as_ref())
            .with_opt_attribute("compressed", compressed.as_ref())
            .with_opt_attribute("chroma", chroma.as_ref())
            .write_children(children)
    }

    const TAG: &'static str = "format";
}

impl<'a> IntoXML for FormatChild<'a> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            FormatChild::Component {
                name,
                bits,
                numeric_format,
                plane_index,
            } => writer
                .create_element2("component")
                .with_fmt_attribute("name", name)
                .with_fmt_attribute("bits", bits)
                .with_fmt_attribute("numericFormat", numeric_format)
                .with_opt_attribute("planeIndex", plane_index.as_ref())
                .write_empty_(),
            FormatChild::Plane {
                index,
                width_divisor,
                height_divisor,
                compatible,
            } => writer
                .create_element2("plane")
                .with_fmt_attribute("index", index)
                .with_fmt_attribute("widthDivisor", width_divisor)
                .with_fmt_attribute("heightDivisor", height_divisor)
                .with_fmt_attribute("compatible", compatible)
                .write_empty_(),
            FormatChild::SpirvImageFormat { name } => writer
                .create_element2("spirvimageformat")
                .with_fmt_attribute("name", name)
                .write_empty_(),
        }
    }
}

impl<'a> IntoXMLElement for Command<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Command {
            proto,
            params,
            success_codes,
            error_codes,
            queues,
            cmd_buffer_level,
            description,
            implicit_extern_sync_params,
            tasks,
            video_coding,
            renderpass,
            comment,
        } = self;
        element
            .with_opt_attribute(
                "successcodes",
                success_codes.as_ref().map(Seperated::<_, ','>),
            )
            .with_opt_attribute("errorcodes", error_codes.as_deref().map(|ec| ec.join(",")))
            .with_opt_attribute("queues", queues.map(Seperated::<_, ','>))
            .with_opt_attribute("cmdbufferlevel", cmd_buffer_level.map(Seperated::<_, ','>))
            .with_opt_attribute("description", description.as_ref())
            .with_opt_attribute("tasks", tasks.map(Seperated::<_, ','>))
            .with_opt_attribute("videocoding", video_coding.as_ref())
            .with_opt_attribute("renderpass", renderpass.as_ref())
            .with_opt_attribute("comment", comment.as_ref())
            .write_inner_content_(|writer| {
                proto.write_xml(writer)?;
                for param in params.iter() {
                    param.write_xml(writer)?;
                }
                implicit_extern_sync_params
                    .as_ref()
                    .map_or(Ok(()), |t| t.write_xml(writer))
            })
    }

    const TAG: &'static str = "command";
}

impl<'a> IntoXMLElement for Proto<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Proto { base } = self;
        base.write_element(element)
    }

    const TAG: &'static str = "proto";
}

impl<'a> IntoXMLElement for CommandParam<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let CommandParam {
            base,
            valid_structs,
            stride,
        } = self;
        base.write_element(
            element
                .with_opt_attribute(
                    "validstructs",
                    valid_structs.as_deref().map(|vs| vs.join(",")),
                )
                .with_opt_attribute("stride", stride.as_deref()),
        )
    }

    const TAG: &'static str = "param";
}

impl<'a> IntoXMLElement for ImplicitExternSyncParam<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let ImplicitExternSyncParam { description } = self;
        element.write_escaped_text(description)
    }

    const TAG: &'static str = "param";
}

impl<'a> IntoXMLElement for ImplicitExternSyncParams<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let ImplicitExternSyncParams { params } = self;
        element.write_children(params)
    }

    const TAG: &'static str = "implicitexternsyncparams";
}

impl<'a> IntoXML for WrappedExtension<'a> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            WrappedExtension::Extension(e) => e.write_xml(writer),
            WrappedExtension::PseudoExtension(e) => e.write_xml(writer),
        }
    }
}

impl<'a> IntoXMLElement for Extension<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Extension {
            name,
            number,
            kind,
            supported,
            requires_core,
            requires_depencies,
            author,
            contact,
            promoted_to,
            deprecated_by,
            obsoleted_by,
            comment,
            requires,
            platform,
            provisional,
            special_use,
            sort_order,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_fmt_attribute("number", number)
            .with_opt_attribute("type", kind.as_ref())
            .with_fmt_attribute("supported", supported)
            .with_opt_attribute("requiresCore", requires_core.as_ref())
            .with_opt_attribute(
                "requires",
                requires_depencies.as_deref().map(|d| d.join(",")),
            )
            .with_opt_attribute("author", author.as_ref())
            .with_opt_attribute("contact", contact.as_ref())
            .with_opt_attribute("promotedto", promoted_to.as_ref())
            .with_opt_attribute("deprecatedby", deprecated_by.as_ref())
            .with_opt_attribute("obsoletedby", obsoleted_by.as_ref())
            .with_opt_attribute("sortorder", sort_order.as_ref())
            .with_opt_attribute("platform", platform.as_ref())
            .with_opt_attribute("provisional", provisional.as_ref())
            .with_opt_attribute("specialuse", special_use.as_ref())
            .with_opt_attribute("comment", comment.as_ref())
            .write_children(requires)
    }

    const TAG: &'static str = "extension";
}

impl<'a> IntoXMLElement for PseudoExtension<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let PseudoExtension {
            name,
            supported,
            comment,
            requires,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_fmt_attribute("supported", supported)
            .with_opt_attribute("comment", comment.as_ref())
            .write_children(requires)
    }

    const TAG: &'static str = "extension";
}

impl<'a> IntoXMLElement for Require<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Require {
            comment,
            values,
            extension,
            feature,
        } = self;
        element
            .with_opt_attribute("feature", feature.as_ref())
            .with_opt_attribute("extension", extension.as_ref())
            .with_opt_attribute("comment", comment.as_ref())
            .write_children(values)
    }

    const TAG: &'static str = "require";
}

impl<'a> IntoXML for RequireValue<'a> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            RequireValue::Type { name, comment } => writer
                .create_element2("type")
                .with_fmt_attribute("name", name)
                .with_opt_attribute("comment", comment.as_deref())
                .write_empty_(),
            RequireValue::Command { name, comment } => writer
                .create_element2("command")
                .with_fmt_attribute("name", name)
                .with_opt_attribute("comment", comment.as_deref())
                .write_empty_(),
            RequireValue::Enum(RequireEnum {
                name,
                extends,
                value,
                protect,
                comment,
            }) => {
                let elem = writer
                    .create_element2("enum")
                    .with_opt_attribute("name", name.as_deref())
                    .with_opt_attribute("extends", extends.as_deref())
                    .with_opt_attribute("protect", protect.as_deref())
                    .with_opt_attribute("comment", comment.as_deref());
                match value {
                    Some(RequireValueEnum::Alias(alias)) => elem.with_fmt_attribute("alias", alias),
                    Some(RequireValueEnum::Bitpos(bitpos)) => {
                        elem.with_fmt_attribute("bitpos", bitpos)
                    }
                    Some(RequireValueEnum::Offset {
                        extnumber,
                        offset,
                        direction,
                    }) => elem
                        .with_fmt_attribute("offset", offset)
                        .with_opt_attribute("extnumber", extnumber.as_ref())
                        .with_opt_attribute("dir", direction.as_ref()),
                    Some(RequireValueEnum::Value(value)) => elem.with_fmt_attribute("value", value),
                    None => elem,
                }
                .write_empty_()
            }
        }
    }
}

impl<'a> IntoXMLElement for Feature<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let Feature {
            name,
            api,
            number,
            comment,
            requires,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_fmt_attribute("api", api)
            .with_fmt_attribute("number", number)
            .with_opt_attribute("comment", comment.as_ref())
            .write_children(requires)
    }

    const TAG: &'static str = "feature";
}

impl<'a> IntoXMLElement for SpirvExtension<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let SpirvExtension {
            name,
            enable_extension,
            enable_version,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .write_inner_content_(|writer| {
                if let Some(ver) = enable_version {
                    ver.write_xml(writer)?;
                }
                enable_extension.write_xml(writer)
            })
    }

    const TAG: &'static str = "spirvextension";
}

impl<'a> IntoXMLElement for SpirvCapability<'a> {
    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let SpirvCapability { name, enables } = self;
        element
            .with_fmt_attribute("name", name)
            .write_children(enables)
    }

    const TAG: &'static str = "spirvcapability";
}

impl IntoXMLElement for VersionEnable {
    const TAG: &'static str = "enable";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let VersionEnable(v) = self;
        element.with_fmt_attribute("version", v).write_empty_()
    }
}

impl<'a> IntoXMLElement for ExtensionEnable<'a> {
    const TAG: &'static str = "enable";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let ExtensionEnable(v) = self;
        element.with_fmt_attribute("extension", v).write_empty_()
    }
}

impl<'a> IntoXMLElement for EnableSpirvCapability<'a> {
    const TAG: &'static str = "enable";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        match self {
            EnableSpirvCapability::Version(v) => v.write_element(element),
            EnableSpirvCapability::Extension(v) => v.write_element(element),
            EnableSpirvCapability::Struct(v) => v.write_element(element),
            EnableSpirvCapability::Property(v) => v.write_element(element),
        }
    }
}

impl<'a> IntoXMLElement for StructEnable<'a> {
    const TAG: &'static str = "enable";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let StructEnable {
            name,
            feature,
            requires,
            alias,
        } = self;
        element
            .with_fmt_attribute("struct", name)
            .with_fmt_attribute("feature", feature)
            .with_fmt_attribute("requires", requires)
            .with_opt_attribute("alias", alias.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for PropertyEnable<'a> {
    const TAG: &'static str = "enable";

    fn write_element<W: Write>(&self, element: ElementWriter2<W>) -> Result {
        let PropertyEnable {
            name,
            member,
            value,
            requires,
        } = self;
        element
            .with_fmt_attribute("property", name)
            .with_fmt_attribute("member", member)
            .with_fmt_attribute("value", value)
            .with_fmt_attribute("requires", requires)
            .write_empty_()
    }
}
