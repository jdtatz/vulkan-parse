use std::{fmt, io::Write};

use quick_xml::{
    escape::escape,
    events::{attributes::Attribute, BytesDecl, BytesStart, BytesText, Event},
    writer::Writer,
    Error,
};

use crate::*;

type Result = std::result::Result<(), Error>;

pub fn into_xml<'a, W: Write>(reg: &Registry<'a>, to: W) -> Result {
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
    fn with_fmt_attribute<'b, V: fmt::Display>(self, attr_key: &'a str, attr_value: V) -> Self;

    fn with_opt_attribute<'b, V: fmt::Display>(
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

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result;
}

impl<E: IntoXMLElement> IntoXML for E {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        self.write_element(writer.create_element2(<Self as IntoXMLElement>::TAG))
    }
}

impl<'a, E: IntoXMLElement> IntoXMLElement for &'a E {
    const TAG: &'static str = E::TAG;

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        (*self).write_element(element)
    }
}

impl<'a, T: IntoXMLElement> IntoXMLElement for DefinitionOrAlias<'a, T> {
    const TAG: &'static str = T::TAG;

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        match self {
            DefinitionOrAlias::Alias {
                name,
                alias,
                comment,
            } => element
                .with_fmt_attribute("name", name)
                .with_fmt_attribute("alias", alias)
                .with_opt_attribute("comment", comment.as_deref())
                .write_empty_(),
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        element.write_escaped_text(&self.0)
    }

    const TAG: &'static str = "comment";
}

// Specfics

impl<'a> IntoXMLElement for Registry<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        element.write_children(&self.0)
    }

    const TAG: &'static str = "registry";
}

impl<'a> IntoXML for Items<'a> {
    fn write_xml<W: Write>(&self, writer: &mut Writer<W>) -> Result {
        match self {
            Items::Platforms(platforms, c) => writer
                .create_element2("platforms")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(platforms),
            Items::Tags(tags, c) => writer
                .create_element2("tags")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(tags),
            Items::Types(types, c) => writer
                .create_element2("types")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(types),
            Items::Enums(enums) => enums.write_xml(writer),
            Items::Commands(commands, c) => writer
                .create_element2("commands")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(commands),
            Items::Features(features) => features.write_xml(writer),
            Items::Extensions(extensions, c) => writer
                .create_element2("extensions")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(extensions),
            Items::Formats(formats) => writer.create_element2("formats").write_children(formats),
            Items::SpirvExtensions(extensions, c) => writer
                .create_element2("spirvextensions")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(extensions),
            Items::SpirvCapabilities(capabilities, c) => writer
                .create_element2("spirvcapabilities")
                .with_opt_attribute("comment", c.as_deref())
                .write_children(capabilities),
        }
    }
}

impl<'a> IntoXMLElement for Platform<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        element
            .with_fmt_attribute("name", &self.name)
            .with_fmt_attribute("protect", &self.protect)
            .with_opt_attribute("comment", self.comment.as_deref())
            .write_empty_()
    }

    const TAG: &'static str = "platform";
}

impl<'a> IntoXMLElement for Tag<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        element
            .with_fmt_attribute("name", &self.name)
            .with_fmt_attribute("author", &self.author)
            .with_fmt_attribute("contact", &self.contact)
            .write_empty_()
    }

    const TAG: &'static str = "tag";
}

fn tokens_to_string(tokens: &[Token]) -> String {
    tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .concat()
}

fn write_typed_tag<W: Write>(
    name: &str,
    type_name: &TypeSpecifier,
    is_const: bool,
    pointer_kind: &Option<PointerKind>,
    name_is_tag: bool,
    writer: &mut Writer<W>,
) -> Result {
    let is_struct = matches!(
        type_name,
        TypeSpecifier::Identifier(TypeIdentifier::Struct(_))
    );
    let pre = match (is_const, is_struct) {
        (true, true) => "const struct",
        (true, false) => "const",
        (false, true) => "struct",
        (false, false) => "",
    };
    writer.write_event(Event::Text(BytesText::new(pre)))?;
    let ty_name = match type_name {
        TypeSpecifier::Identifier(TypeIdentifier::Plain(id)) => id.to_string(),
        TypeSpecifier::Identifier(TypeIdentifier::Struct(id)) => id.to_string(),
        TypeSpecifier::Identifier(_) => todo!(),
        ty => ty.to_string(),
    };
    writer
        .create_element2("type")
        .write_escaped_text(&ty_name)?;
    let post = match pointer_kind {
        Some(PointerKind::Single) => "*",
        Some(PointerKind::Double {
            inner_is_const: true,
        }) => "* const*",
        Some(PointerKind::Double {
            inner_is_const: false,
        }) => "**",
        None => "",
    };
    writer.write_event(Event::Text(BytesText::new(post)))?;
    if name_is_tag {
        writer.create_element2("name").write_escaped_text(&name)?;
    } else {
        writer.write_event(Event::Text(BytesText::new(name)))?;
    }
    Ok(())
}

impl<'a> IntoXMLElement for FieldLike<'a> {
    const TAG: &'static str = "";

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let FieldLike {
            name,
            type_name,
            is_const,
            pointer_kind,
            bitfield_size,
            array_shape,
            dynamic_shape,
            extern_sync,
            optional,
            no_auto_validity,
            object_type,
            comment,
        } = self;
        let element = match dynamic_shape {
            Some(DynamicShapeKind::Expression { latex_expr, c_expr }) => element
                .with_fmt_attribute("len", format_args!("latexmath:{}", latex_expr))
                .with_fmt_attribute("altlen", c_expr),
            Some(DynamicShapeKind::Double(l1, l2)) => {
                element.with_fmt_attribute("len", format_args!("{},{}", l1, l2))
            }
            Some(DynamicShapeKind::Single(l)) => element.with_fmt_attribute("len", l),
            None => element,
        };
        element
            .with_opt_attribute("optional", optional.as_ref())
            .with_opt_attribute("noautovalidity", no_auto_validity.as_ref())
            .with_opt_attribute("externsync", extern_sync.as_ref())
            .with_opt_attribute("objecttype", object_type.as_ref())
            .write_inner_content_(|writer| {
                write_typed_tag(name, type_name, *is_const, pointer_kind, true, writer)?;
                if let Some(bitfield_size) = bitfield_size {
                    writer.write_event(Event::Text(BytesText::new(&format!(
                        " : {}",
                        bitfield_size
                    ))))?;
                }
                if let Some(array_shape) = array_shape {
                    for shape in array_shape.iter() {
                        writer.write_event(Event::Text(BytesText::new("[")))?;
                        match shape {
                            ArrayLength::Static(n) => writer
                                .write_event(Event::Text(BytesText::new(&format!("{}", n))))?,
                            ArrayLength::Constant(c) => {
                                writer.create_element2("enum").write_escaped_text(c)?
                            }
                        }
                        writer.write_event(Event::Text(BytesText::new("]")))?;
                    }
                }
                if let Some(comment) = comment {
                    writer
                        .create_element2("comment")
                        .write_escaped_text(comment)?;
                }
                Ok(())
            })
    }
}

impl<'a> IntoXMLElement for Type<'a> {
    const TAG: &'static str = "type";

    fn write_element<'e, W: Write>(&self, mut elem: ElementWriter2<'e, W>) -> Result {
        match self {
            Type::Requires(RequiresType { name, requires }) => elem
                .with_fmt_attribute("name", name)
                .with_opt_attribute("requires", requires.as_deref())
                .write_empty_(),
            Type::Include(IncludeType {
                name,
                is_local_include,
            }) => {
                elem = elem
                    .with_fmt_attribute("category", "include")
                    .with_fmt_attribute("name", name);
                match is_local_include {
                    Some(true) if name.ends_with(".h") => {
                        elem.write_escaped_text(&format!("#include \"{}\"", name))
                    }
                    Some(true) => elem.write_escaped_text(&format!("#include \"{}.h\"", name)),
                    Some(false) if name.ends_with(".h") => {
                        elem.write_escaped_text(&format!("#include <{}>", name))
                    }
                    Some(false) => elem.write_escaped_text(&format!("#include <{}.h>", name)),
                    None => elem.write_empty_(),
                }
            }
            Type::Define(DefineType {
                name,
                comment,
                requires,
                is_disabled,
                value,
            }) => {
                elem = elem
                    .with_fmt_attribute("category", "define")
                    .with_opt_attribute("requires", requires.as_deref())
                    .with_opt_attribute("comment", comment.as_deref());
                let define_macro = if *is_disabled { "//#define" } else { "#define" };
                match value {
                    DefineTypeValue::Expression(expr) => elem.write_inner_content_(|writer| {
                        writer.write_event(Event::Text(BytesText::new(define_macro)))?;
                        writer.create_element2("name").write_escaped_text(name)?;
                        let value = format!(" {}", expr);
                        writer.write_event(Event::Text(BytesText::new(&value)))
                    }),
                    DefineTypeValue::FunctionDefine { params, expression } => elem
                        .write_inner_content_(|writer| {
                            writer.write_event(Event::Text(BytesText::new(define_macro)))?;
                            writer.create_element2("name").write_escaped_text(name)?;
                            let params = params.join(", ");
                            let value = format!("({}) {}", params, tokens_to_string(expression));
                            writer.write_event(Event::Text(BytesText::new(&value)))
                        }),
                    DefineTypeValue::MacroFunctionCall {
                        name: fn_name,
                        args,
                    } => elem.write_inner_content_(|writer| {
                        writer.write_event(Event::Text(BytesText::new(define_macro)))?;
                        writer.create_element2("name").write_escaped_text(name)?;
                        writer.write_event(Event::Text(BytesText::new(" ")))?;
                        writer.create_element2("type").write_escaped_text(fn_name)?;
                        let args = args
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ");
                        let value = format!("({})", args);
                        writer.write_event(Event::Text(BytesText::new(&value)))
                    }),
                    DefineTypeValue::Code(tokens) => elem
                        .with_fmt_attribute("name", name)
                        .write_escaped_text(&tokens_to_string(tokens)),
                }
            }
            Type::BaseType(base) => {
                elem = elem.with_fmt_attribute("category", "basetype");
                match base {
                    BaseTypeType::Forward(name) => elem.write_inner_content_(|writer| {
                        writer.write_event(Event::Text(BytesText::new("struct ")))?;
                        writer.create_element2("name").write_escaped_text(name)?;
                        writer.write_event(Event::Text(BytesText::new(";")))
                    }),
                    BaseTypeType::TypeDef(field) => elem.write_inner_content_(|writer| {
                        writer.write_event(Event::Text(BytesText::new("typedef ")))?;
                        let FieldLike {
                            name,
                            type_name,
                            is_const,
                            pointer_kind,
                            ..
                        } = field;
                        write_typed_tag(name, type_name, *is_const, pointer_kind, true, writer)?;
                        writer.write_event(Event::Text(BytesText::new(";")))
                    }),
                    BaseTypeType::DefineGuarded { pre, name, post } => {
                        elem.write_inner_content_(|writer| {
                            writer.write_event(Event::Text(BytesText::new(
                                &tokens_to_string(pre),
                            )))?;
                            writer.create_element2("name").write_escaped_text(name)?;
                            writer.write_event(Event::Text(BytesText::new(
                                &tokens_to_string(post),
                            )))
                        })
                    }
                }
            }
            Type::Bitmask(defn) => {
                elem = elem.with_fmt_attribute("category", "bitmask");
                match defn {
                    DefinitionOrAlias::Alias {
                        name,
                        alias,
                        comment,
                    } => elem
                        .with_fmt_attribute("name", name)
                        .with_fmt_attribute("alias", alias)
                        .with_opt_attribute("comment", comment.as_deref())
                        .write_empty_(),
                    DefinitionOrAlias::Definition(BitmaskType {
                        name,
                        is_64bits,
                        has_bitvalues,
                    }) => elem
                        // TODO requires / bitvalues
                        .with_opt_attribute(
                            "requires",
                            has_bitvalues.then(|| name.replace("Flags", "FlagBits")),
                        )
                        .write_inner_content_(|writer| {
                            writer.write_event(Event::Text(BytesText::new("typedef ")))?;
                            writer.create_element2("type").write_escaped_text(
                                is_64bits.then_some("VkFlags64").unwrap_or("VkFlags"),
                            )?;
                            writer.write_event(Event::Text(BytesText::new(" ")))?;
                            writer.create_element2("name").write_escaped_text(name)?;
                            writer.write_event(Event::Text(BytesText::new(";")))
                        }),
                }
            }
            Type::Handle(defn) => {
                elem = elem.with_fmt_attribute("category", "handle");
                match defn {
                    DefinitionOrAlias::Alias {
                        name,
                        alias,
                        comment,
                    } => elem
                        .with_fmt_attribute("name", name)
                        .with_fmt_attribute("alias", alias)
                        .with_opt_attribute("comment", comment.as_deref())
                        .write_empty_(),
                    DefinitionOrAlias::Definition(HandleType {
                        name,
                        handle_kind,
                        obj_type_enum,
                        parent,
                    }) => elem
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
                        }),
                }
            }
            Type::Enum(EnumType { name }) => elem
                .with_fmt_attribute("category", "enum")
                .with_fmt_attribute("name", name)
                .write_empty_(),
            Type::FnPtr(FnPtrType {
                name_and_return,
                requires,
                params,
            }) => elem
                .with_fmt_attribute("category", "funcpointer")
                .with_opt_attribute("requires", requires.as_deref())
                .write_inner_content_(|writer| {
                    writer.write_event(Event::Text(BytesText::from_escaped("typedef ")))?;
                    writer.write_event(Event::Text(BytesText::new(&name_and_return.type_name.to_string(),
                    )))?;
                    let post = match name_and_return.pointer_kind {
                        Some(PointerKind::Single) => "*",
                        Some(PointerKind::Double {
                            inner_is_const: true,
                        }) => "* const*",
                        Some(PointerKind::Double {
                            inner_is_const: false,
                        }) => "**",
                        None => "",
                    };
                    writer.write_event(Event::Text(BytesText::new(post)))?;
                    writer.write_event(Event::Text(BytesText::new(" (VKAPI_PTR *")))?;
                    writer
                        .create_element2("name")
                        .write_escaped_text(&name_and_return.name)?;
                    writer.write_event(Event::Text(BytesText::new(")(")))?;
                    if let Some(params) = params {
                        let mut is_first = true;
                        for param in params {
                            if is_first {
                                is_first = false
                            } else {
                                writer.write_event(Event::Text(BytesText::new(",\n")))?;
                            }
                            let FieldLike {
                                name,
                                type_name,
                                is_const,
                                pointer_kind,
                                ..
                            } = param;
                            write_typed_tag(
                                name,
                                type_name,
                                *is_const,
                                pointer_kind,
                                false,
                                writer,
                            )?;
                        }
                        writer.write_event(Event::Text(BytesText::new(");")))
                    } else {
                        writer.write_event(Event::Text(BytesText::new("void);")))
                    }
                }),
            Type::Struct(defn) => {
                elem = elem.with_fmt_attribute("category", "struct");
                match defn {
                    DefinitionOrAlias::Alias {
                        name,
                        alias,
                        comment,
                    } => elem
                        .with_fmt_attribute("name", name)
                        .with_fmt_attribute("alias", alias)
                        .with_opt_attribute("comment", comment.as_deref())
                        .write_empty_(),
                    DefinitionOrAlias::Definition(StructType {
                        name,
                        members,
                        returned_only,
                        struct_extends,
                        allow_duplicate,
                    }) => elem
                        .with_fmt_attribute("name", name)
                        .with_opt_attribute("returnedonly", returned_only.map(|b| b.to_string()))
                        .with_opt_attribute(
                            "structextends",
                            struct_extends.as_deref().map(|se| se.join(",")),
                        )
                        .with_opt_attribute(
                            "allowduplicate",
                            allow_duplicate.map(|b| b.to_string()),
                        )
                        .write_children(members),
                }
            }
            Type::Union(UnionType {
                name,
                members,
                returned_only,
            }) => elem
                .with_fmt_attribute("category", "union")
                .with_fmt_attribute("name", name)
                .with_opt_attribute("returnedonly", returned_only.map(|b| b.to_string()))
                .write_children(members),
        }
    }
}

impl<'a> IntoXMLElement for Member<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let Member {
            base,
            selector,
            selection,
            values,
            limit_type,
        } = self;
        base.write_element(element
            .with_opt_attribute("selector", selector.as_ref())
            .with_opt_attribute("selection", selection.as_ref())
            .with_opt_attribute("values", values.as_ref())
            .with_opt_attribute("limittype", limit_type.as_ref()))
    }

    const TAG: &'static str = "member";
}

impl<'a> IntoXMLElement for Enums<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let Enums {
            name,
            comment,
            values,
        } = self;
        let elem = element
            .with_fmt_attribute("name", name)
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

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        match self {
            BitmaskEnum::Value(value) => value.write_element(element),
            BitmaskEnum::BitPos(bitpos) => bitpos.write_element(element),
        }
    }
}

impl<'a> IntoXMLElement for BitPosEnum<'a> {
    const TAG: &'static str = "enum";

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let UnusedEnum { start, comment } = self;
        element
            .with_fmt_attribute("start", start)
            .with_opt_attribute("comment", comment.as_deref())
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for Format<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
            .with_fmt_attribute("blocksize", block_size)
            .with_fmt_attribute("texelsperblock", texels_per_block)
            .with_opt_attribute("blockextent", block_extent.as_ref())
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
            .with_opt_attribute("successcodes", success_codes.as_ref())
            .with_opt_attribute("error_codes", error_codes.as_deref().map(|ec| ec.join(",")))
            .with_opt_attribute("queues", queues.as_ref())
            .with_opt_attribute("cmdbufferlevel", cmd_buffer_level.as_ref())
            .with_opt_attribute("description", description.as_ref())
            .with_opt_attribute("tasks", tasks.as_ref())
            .with_opt_attribute("video_coding", video_coding.as_ref())
            .with_opt_attribute("renderpass", renderpass.as_ref())
            .with_opt_attribute("comment", comment.as_ref())
            .write_inner_content_(|writer| {
                let FieldLike {
                    name,
                    type_name,
                    is_const,
                    pointer_kind,
                    ..
                } = proto;
                write_typed_tag(name, type_name, *is_const, pointer_kind, true, writer)?;
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

impl<'a> IntoXMLElement for CommandParam<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let CommandParam {
            base,
            valid_structs,
            stride,
        } = self;
        base.write_element(element
            .with_opt_attribute(
                "validstructs",
                valid_structs.as_deref().map(|vs| vs.join(",")),
            )
            .with_opt_attribute("stride", stride.as_deref()))
    }

    const TAG: &'static str = "param";
}

impl<'a> IntoXMLElement for ImplicitExternSyncParams<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let ImplicitExternSyncParams { params } = self;
        element.write_inner_content_(|writer| {
            for param in params.iter() {
                writer.create_element2("param").write_escaped_text(param)?;
            }
            Ok(())
        })
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
            comment,
            requires,
        } = self;
        element
            .with_fmt_attribute("name", name)
            .with_fmt_attribute("number", number)
            .with_opt_attribute("type", kind.as_ref())
            .with_fmt_attribute("supported", supported)
            .with_opt_attribute("requirescore", requires_core.as_ref())
            .with_opt_attribute(
                "requires",
                requires_depencies.as_deref().map(|d| d.join(",")),
            )
            .with_opt_attribute("author", author.as_ref())
            .with_opt_attribute("contact", contact.as_ref())
            .with_opt_attribute("promotedto", promoted_to.as_ref())
            .with_opt_attribute("comment", comment.as_ref())
            .write_children(requires)
    }

    const TAG: &'static str = "extension";
}

impl<'a> IntoXMLElement for PseudoExtension<'a> {
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let Require { comment, values } = self;
        element
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
            RequireValue::Enum {
                extends,
                value,
                comment,
            } => {
                let elem = writer
                    .create_element2("enum")
                    .with_opt_attribute("extends", extends.as_deref())
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let SpirvCapability { name, enables } = self;
        element
            .with_fmt_attribute("name", name)
            .write_children(enables)
    }

    const TAG: &'static str = "spirvcapability";
}

impl IntoXMLElement for VersionEnable {
    const TAG: &'static str = "enable";

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let VersionEnable(v) = self;
        element.with_fmt_attribute("version", v).write_empty_()
    }
}

impl<'a> IntoXMLElement for ExtensionEnable<'a> {
    const TAG: &'static str = "enable";

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let ExtensionEnable(v) = self;
        element.with_fmt_attribute("extension", v).write_empty_()
    }
}

impl<'a> IntoXMLElement for EnableSpirvCapability<'a> {
    const TAG: &'static str = "enable";

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
        let StructEnable {
            name,
            feature,
            requires,
        } = self;
        element
            .with_fmt_attribute("struct", name)
            .with_fmt_attribute("feature", feature)
            .with_fmt_attribute("requires", requires)
            .write_empty_()
    }
}

impl<'a> IntoXMLElement for PropertyEnable<'a> {
    const TAG: &'static str = "enable";

    fn write_element<'e, W: Write>(&self, element: ElementWriter2<'e, W>) -> Result {
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
