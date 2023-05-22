use crate::{
    escape_utils::{Escaped, InterspersedDisplay, Seperator},
    DisplayEscaped,
};

pub trait MaybeIntoAttrValue {
    type Value: ?Sized;

    fn maybe_into_value(&self) -> Option<&Self::Value>;
}

impl<T: ?Sized + DisplayEscaped> MaybeIntoAttrValue for T {
    type Value = Self;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        Some(self)
    }
}
impl<T> MaybeIntoAttrValue for Vec<T> {
    type Value = Self;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        Some(self)
    }
}
impl<T: enumflags2::BitFlag> MaybeIntoAttrValue for enumflags2::BitFlags<T> {
    type Value = Self;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        Some(self)
    }
}

impl<T> MaybeIntoAttrValue for Option<T> {
    type Value = T;

    fn maybe_into_value(&self) -> Option<&Self::Value> {
        self.as_ref()
    }
}

pub trait XMLWriter {
    type Error;

    fn write_escaped<V: ?Sized + DisplayEscaped>(&mut self, escaped: &V)
    -> Result<(), Self::Error>;
    fn write_element_start(&mut self, prefix: Option<&str>, name: &str) -> Result<(), Self::Error> {
        if let Some(prefix) = prefix {
            self.write_escaped(&format_args!("<{prefix}:{name}"))
        } else {
            self.write_escaped(&format_args!("<{name}"))
        }
    }
    fn write_attribute<V: ?Sized + DisplayEscaped>(
        &mut self,
        prefix: Option<&str>,
        name: &str,
        value: &V,
    ) -> Result<(), Self::Error> {
        let value = Escaped(value);
        if let Some(prefix) = prefix {
            self.write_escaped(&format_args!(" {prefix}:{name}=\"{value}\""))
        } else {
            self.write_escaped(&format_args!(" {name}=\"{value}\""))
        }
    }
    fn write_element_open_end(&mut self) -> Result<(), Self::Error> {
        self.write_escaped(">")
    }
    fn write_element_empty_end(&mut self) -> Result<(), Self::Error> {
        self.write_escaped("/>")
    }
    fn write_element_close_end(
        &mut self,
        prefix: Option<&str>,
        name: &str,
    ) -> Result<(), Self::Error> {
        if let Some(prefix) = prefix {
            self.write_escaped(&format_args!("</{prefix}:{name}>"))
        } else {
            self.write_escaped(&format_args!("</{name}>"))
        }
    }
    fn write_declaration(
        &mut self,
        version: &str,
        encoding: Option<&str>,
        standalone: Option<&str>,
    ) -> Result<(), Self::Error> {
        match (encoding, standalone) {
            (None, None) => self.write_escaped(&format_args!("<?xml version=\"{version}\"?>")),
            (None, Some(standalone)) => self.write_escaped(&format_args!(
                "<?xml version=\"{version}\" standalone=\"{standalone}\"?>"
            )),
            (Some(encoding), None) => self.write_escaped(&format_args!(
                "<?xml version=\"{version}\" encoding=\"{encoding}\"?>"
            )),
            (Some(encoding), Some(standalone)) => self.write_escaped(&format_args!(
                "<?xml version=\"{version}\" encoding=\"{encoding}\" standalone=\"{standalone}\"?>"
            )),
        }
    }
}

pub trait IntoXMLChildren {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error>;
}

impl<T: IntoXML> IntoXMLChildren for T {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.write_xml(writer)
    }
}

impl<T: IntoXML> IntoXMLChildren for Option<T> {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.as_ref().map_or(Ok(()), |v| v.write_xml(writer))
    }
}

impl<T: IntoXML> IntoXMLChildren for [T] {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.iter().try_for_each(|v| v.write_xml(writer))
    }
}

impl<T: IntoXML> IntoXMLChildren for Vec<T> {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.iter().try_for_each(|v| v.write_xml(writer))
    }
}

#[impl_trait_for_tuples::impl_for_tuples(4)]
impl IntoXMLChildren for Tuple {
    fn write_children<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        for_tuples!( #( Tuple.write_children(writer)?; )* );
        Ok(())
    }
}

pub struct XMLElementBuilder<'t, 'w, W: 'w + ?Sized + XMLWriter> {
    tag_name: &'t str,
    writer: &'w mut W,
}

impl<'t, 'w, W: 'w + ?Sized + XMLWriter> XMLElementBuilder<'t, 'w, W> {
    pub fn new(writer: &'w mut W, name: &'t str) -> Result<Self, W::Error> {
        writer.write_element_start(None, name)?;
        Ok(Self {
            tag_name: name,
            writer,
        })
    }
    pub fn with_attribute<V: ?Sized + DisplayEscaped>(
        self,
        name: &str,
        value: &V,
    ) -> Result<Self, W::Error> {
        self.writer.write_attribute(None, name, value)?;
        Ok(self)
    }

    pub fn write_empty(self) -> Result<(), W::Error> {
        self.writer.write_element_empty_end()
    }

    pub fn write_inner_content<F>(self, closure: F) -> Result<(), W::Error>
    where
        F: FnOnce(&mut W) -> Result<(), W::Error>,
    {
        self.writer.write_element_open_end()?;
        closure(self.writer)?;
        self.writer.write_element_close_end(None, self.tag_name)
    }

    // default methods
    pub fn write_escaped_text<V: ?Sized + DisplayEscaped>(
        self,
        escaped: &V,
    ) -> Result<(), W::Error> {
        self.write_inner_content(|w| w.write_escaped(escaped))
    }

    pub fn with_escaped_attribute<V: ?Sized + MaybeIntoAttrValue>(
        self,
        attr_key: &str,
        maybe_attr_value: &V,
    ) -> Result<Self, W::Error>
    where
        V::Value: DisplayEscaped,
    {
        if let Some(attr_value) = maybe_attr_value.maybe_into_value() {
            self.with_attribute(attr_key, attr_value)
        } else {
            Ok(self)
        }
    }
    pub fn with_interspersed_attribute<'v, S: Seperator, V: MaybeIntoAttrValue>(
        self,
        attr_key: &str,
        maybe_attr_value: &'v V,
    ) -> Result<Self, W::Error>
    where
        V::Value: crate::Iterable,
        <<V::Value as crate::Iterable>::IT<'v> as IntoIterator>::Item: DisplayEscaped,
    {
        if let Some(attr_value) = maybe_attr_value.maybe_into_value() {
            self.with_attribute(attr_key, &InterspersedDisplay::<S, _>::new(attr_value))
        } else {
            Ok(self)
        }
    }
    pub fn with_escaped_attributes<V: ?Sized + IntoXMLAttributes>(
        self,
        attributes: &V,
    ) -> Result<Self, W::Error> {
        let element = self;
        attributes.write_attributes(element)
    }

    pub fn write_children<I: IntoXMLChildren>(self, children: &I) -> Result<(), W::Error> {
        self.write_inner_content(move |writer| children.write_children(writer))
    }

    pub fn write_tokens<'tok, T: crate::IntoVkXMLTokens<'tok>>(
        self,
        value: T,
    ) -> Result<(), W::Error> {
        self.write_inner_content(move |writer| {
            let mut last_ident_like = false;
            for token in value.to_tokens_vector() {
                match token {
                    crate::VkXMLToken::C(token) => {
                        let is_ident_like = token.is_ident_like();
                        if last_ident_like && is_ident_like {
                            writer.write_escaped(" ")?;
                        };
                        writer.write_escaped(&token)?;
                        last_ident_like = is_ident_like;
                    }
                    crate::VkXMLToken::TextTag { name, text } => {
                        last_ident_like = false;
                        XMLElementBuilder::new(writer, name)?.write_escaped_text(&text)?;
                    }
                }
            }
            Ok(())
        })
    }
}

pub(crate) trait IntoXML {
    fn write_xml<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error>;
}

pub(crate) trait IntoXMLElement {
    const TAG: &'static str;

    fn add_static_attrs<'w, W: ?Sized + XMLWriter>(
        element: XMLElementBuilder<'static, 'w, W>,
    ) -> Result<XMLElementBuilder<'static, 'w, W>, W::Error> {
        Ok(element)
    }

    fn write_element<'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'static, 'w, W>,
    ) -> Result<(), W::Error>;

    fn write_element_and_static_attrs<'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'static, 'w, W>,
    ) -> Result<(), W::Error> {
        self.write_element(Self::add_static_attrs(element)?)
    }
}

impl<T: IntoXMLElement> IntoXML for T {
    fn write_xml<W: ?Sized + XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        self.write_element(Self::add_static_attrs(XMLElementBuilder::new(
            writer,
            Self::TAG,
        )?)?)
    }
}

pub trait IntoXMLAttributes {
    fn write_attributes<'t, 'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error>;
}

impl<'a, T: ?Sized + IntoXMLAttributes> IntoXMLAttributes for &'a T {
    fn write_attributes<'t, 'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error> {
        (**self).write_attributes(element)
    }
}

impl<T: IntoXMLAttributes> IntoXMLAttributes for Option<T> {
    fn write_attributes<'t, 'w, W: ?Sized + XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error> {
        if let Some(v) = self {
            v.write_attributes(element)
        } else {
            Ok(element)
        }
    }
}
