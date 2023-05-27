use darling::{
    util::{Flag, SpannedValue},
    FromDeriveInput, FromField, FromMeta, FromVariant,
};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{ext::IdentExt, DeriveInput, Ident, Path, PathSegment};

#[derive(Debug)]
struct TagAttr(syn::Expr);

impl FromMeta for TagAttr {
    fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
        Ok(Self(expr.clone()))
    }
}

impl ToTokens for TagAttr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}
#[derive(Debug, Clone)]
enum WordOrList<T> {
    Word,
    Value(T),
}

impl<T: Default> WordOrList<T> {
    fn into_value_or_default(self) -> T {
        match self {
            WordOrList::Word => T::default(),
            WordOrList::Value(v) => v,
        }
    }
}

impl<T: FromMeta> FromMeta for WordOrList<T> {
    fn from_list(items: &[darling::export::NestedMeta]) -> darling::Result<Self> {
        T::from_list(items).map(WordOrList::Value)
    }

    fn from_word() -> darling::Result<Self> {
        Ok(Self::Word)
    }
}

#[derive(Debug, Clone, Default, FromMeta)]
struct XMLSerializationDeriveXAttribute {
    #[darling(default)]
    rename: Option<String>,
    #[darling(default)]
    seperator: Option<syn::ExprPath>,
    #[darling(default)]
    mapped: Option<syn::ExprPath>,
    #[darling(default)]
    flattened: Flag,
}

#[derive(Debug, FromField)]
#[darling(attributes(vkxml))]
struct XMLSerializationDeriveField {
    ident: Option<Ident>,
    // ty: syn::Type,
    // attributes
    #[darling(default, rename = "attribute")]
    attribute_opts: Option<WordOrList<XMLSerializationDeriveXAttribute>>,
    #[darling(default, rename = "child")]
    child_opts: Flag,
    #[darling(default)]
    text: Flag,
    #[darling(default)]
    flatten: Flag,
}

struct SplitFields<'s> {
    flattened: Option<Option<&'s Ident>>,
    text: Option<Option<&'s Ident>>,
    attrs: Vec<(Option<&'s Ident>, XMLSerializationDeriveXAttribute)>,
    children: Vec<Option<&'s Ident>>,
}

impl<'s> TryFrom<&'s [SpannedValue<XMLSerializationDeriveField>]> for SplitFields<'s> {
    type Error = syn::Error;

    fn try_from(
        value: &'s [SpannedValue<XMLSerializationDeriveField>],
    ) -> Result<Self, Self::Error> {
        let mut flattened = None;
        let mut text = None;
        let mut attrs = Vec::new();
        let mut children = Vec::new();
        for f in value {
            if f.flatten.is_present() {
                if f.attribute_opts.is_some() || f.child_opts.is_present() || f.text.is_present() {
                    return Err(syn::Error::new(
                        f.span(),
                        "A flattened member can't be an attribute, child, or text",
                    ));
                }
                if flattened.replace(f.ident.as_ref()).is_some() {
                    return Err(syn::Error::new(f.span(), "Only 1 member can be flattened"));
                }
            } else if let Some(ref attr) = f.attribute_opts {
                if f.child_opts.is_present() || f.text.is_present() {
                    return Err(syn::Error::new(
                        f.span(),
                        "An attribute member can't be child/text as well",
                    ));
                }
                attrs.push((f.ident.as_ref(), attr.clone().into_value_or_default()))
            } else if f.text.is_present() {
                if text.replace(f.ident.as_ref()).is_some() {
                    return Err(syn::Error::new(
                        f.span(),
                        "Only 1 member can be the text content",
                    ));
                }
            } else if f.child_opts.is_present() {
                children.push(f.ident.as_ref())
            }
        }
        if flattened.is_some() && (text.is_some() || !children.is_empty()) {
            return Err(syn::Error::new(
                Span::call_site(),
                "No child or text members if there is a flattened one",
            ));
        }
        if text.is_some() && !children.is_empty() {
            return Err(syn::Error::new(
                Span::call_site(),
                "Can have only either children members or a text content member",
            ));
        }
        Ok(Self {
            flattened,
            text,
            attrs,
            children,
        })
    }
}

fn named_field_deserialization_guard(
    tag: Option<&TagAttr>,
    discriminant: Option<&VariantDiscriminantOpts>,
) -> Option<TokenStream> {
    match (tag, discriminant) {
        (None, None) => None,
        (None, Some(VariantDiscriminantOpts { attr, value: None })) => {
            Some(quote!(node.has_attribute(#attr)))
        }
        (
            None,
            Some(VariantDiscriminantOpts {
                attr,
                value: Some(value),
            }),
        ) => Some(quote!(let Some(#value) = node.attribute(#attr))),
        (Some(tag), None) => Some(quote!(node.has_tag_name(#tag))),
        (Some(tag), Some(VariantDiscriminantOpts { attr, value: None })) => {
            Some(quote!(node.has_tag_name(#tag) && node.has_attribute(#attr)))
        }
        (
            Some(tag),
            Some(VariantDiscriminantOpts {
                attr,
                value: Some(value),
            }),
        ) => Some(quote!(node.has_tag_name(#tag) && matches!(node.attribute(#attr), Some(#value)))),
    }
}

fn named_field_deserialization_impl(
    struct_path: &Path,
    fields: &[SpannedValue<XMLSerializationDeriveField>],
    tokenized: bool,
) -> syn::Result<TokenStream> {
    let SplitFields {
        flattened,
        attrs,
        children,
        text,
    } = fields.try_into()?;
    let flattened_field = flattened.map(|name| {
        let name = name.unwrap();
        quote! { #name: crate::TryFromXML::from_xml(node)?, }
    });
    let attr_fields = attrs.iter().map(
        |(
            name,
            XMLSerializationDeriveXAttribute {
                rename,
                seperator,
                mapped,
                flattened,
            },
        )| {
            let name = name.unwrap();
            if flattened.is_present() {
                return quote! { #name : crate::TryFromAttributes::try_from_attributes(node)? };
            }
            let attr_str = rename
                .as_deref()
                .map(ToTokens::to_token_stream)
                .unwrap_or_else(|| quote! { stringify!(#name) });
            let v = if let Some(seperator) = seperator {
                quote! { crate::try_from_interspersed_attr::<#seperator, _>(node, #attr_str)? }
            } else {
                quote! { crate::try_from_attribute(node, #attr_str)? }
            };
            if let Some(mapped) = mapped {
                quote! { #name : #mapped::into(#v) }
            } else {
                quote! { #name : #v }
            }
        },
    );
    let text_field = text.map(|name| {
        let name = name.unwrap();
        quote! { #name : crate::TryFromTextContent::try_from_text(node)?, }
    });
    let child_idents: Vec<_> = children
        .iter()
        .enumerate()
        .map(|(i, name)| name.cloned().unwrap_or_else(|| format_ident!("_{}", i)))
        .collect();
    assert!(!(tokenized && !child_idents.is_empty()));
    assert!(!(tokenized && text_field.is_some()));
    if tokenized {
        Ok(quote! {
            Ok(Some(#struct_path {
                #(#attr_fields,)*
                #flattened_field
                ..TryFromTokens::try_from_node(node)?
            }))
        })
    } else if child_idents.is_empty() {
        Ok(quote! {
            Ok(Some(#struct_path {
                #(#attr_fields,)*
                #flattened_field
                #text_field
            }))
        })
    } else {
        assert!(text_field.is_none());
        Ok(quote! {
            let mut it = node.into();
            #(let #child_idents = crate::TryFromXMLChildren::try_from_children(&mut it)?;)*
            if let Some(n) = it.next() {
                return Err(crate::ErrorKind::NoMatch.with_location(&n))
            }
            Ok(Some(#struct_path {
                #(#attr_fields,)*
                #flattened_field
                #(#child_idents,)*
            }))
        })
    }
}

fn named_field_serialization_impl(
    fields: &[SpannedValue<XMLSerializationDeriveField>],
    discriminant: Option<&VariantDiscriminantOpts>,
    tokenized: bool,
) -> syn::Result<TokenStream> {
    let SplitFields {
        flattened,
        attrs,
        children,
        text,
    } = fields.try_into()?;
    let set_static_attr = discriminant.and_then(|v| v.set_static_attr_tokens());
    let flattened_field = flattened.map(|name| name.unwrap());
    let text_field = text.map(|name| name.unwrap());
    let attr_fields = attrs.iter().map(
        |(
            name,
            XMLSerializationDeriveXAttribute {
                rename,
                seperator,
                mapped,
                flattened,
            },
        )| {
            let name = name.unwrap();
            if flattened.is_present() {
                return quote! { with_escaped_attributes(#name) };
            }
            let attr_str = rename
                .as_deref()
                .map(ToTokens::to_token_stream)
                .unwrap_or_else(|| quote! { stringify!(#name) });
            let name = if let Some(mapped) = mapped {
                quote!(&#mapped::from(#name))
            } else {
                quote!(#name)
            };
            if let Some(seperator) = seperator {
                quote! { with_interspersed_attribute::<#seperator, _>(#attr_str, #name) }
            } else {
                quote! { with_escaped_attribute(#attr_str, #name) }
            }
        },
    );
    let child_idents: Vec<_> = children
        .iter()
        .enumerate()
        .map(|(i, name)| name.cloned().unwrap_or_else(|| format_ident!("_{}", i)))
        .collect();
    assert!(!(flattened_field.is_some() && !child_idents.is_empty()));
    assert!(!(flattened_field.is_some() && text.is_some()));
    if tokenized {
        Ok(quote! {
            element #set_static_attr #( . #attr_fields ? )* .write_tokens(self)
        })
    } else if let Some(flattened_field) = flattened_field {
        Ok(quote! {
            #flattened_field.write_element(element #set_static_attr #( . #attr_fields ? )*)
        })
    } else if let Some(text) = text_field {
        Ok(quote! {
            element #set_static_attr #( . #attr_fields ? )* .write_escaped_text(#text)
        })
    } else if child_idents.is_empty() {
        Ok(quote! {
            element #set_static_attr #( . #attr_fields ? )* .write_empty()
        })
    } else {
        Ok(quote! {
            element #set_static_attr #( . #attr_fields ? )* .write_inner_content(move |writer| {
                use crate::into_xml::IntoXMLChildren;
                #(#child_idents.write_children(writer)?;)*
                Ok(())
            })
        })
    }
}

#[derive(Debug, Default)]
struct VariantDiscriminantOpts {
    attr: String,
    value: Option<String>,
}

impl FromMeta for VariantDiscriminantOpts {
    fn from_list(items: &[darling::export::NestedMeta]) -> darling::Result<Self> {
        match items {
            [] => Err(darling::Error::too_few_items(1)),
            [
                darling::export::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                    path,
                    value,
                    ..
                })),
            ] => {
                if let Some(ident) = path.get_ident() {
                    let attr = ident.unraw().to_string();
                    if let syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(value),
                        ..
                    }) = value
                    {
                        Ok(VariantDiscriminantOpts {
                            attr,
                            value: Some(value.value()),
                        })
                    } else {
                        Err(darling::Error::unexpected_expr_type(value))
                    }
                } else {
                    Err(darling::Error::unknown_field_path(path))
                }
            }
            [darling::export::NestedMeta::Meta(syn::Meta::List(_))] => {
                Err(darling::Error::unsupported_format("list"))
            }
            [darling::export::NestedMeta::Meta(syn::Meta::Path(_))] => {
                Err(darling::Error::unsupported_format("word"))
            }
            [darling::export::NestedMeta::Lit(l)] => Err(darling::Error::unexpected_lit_type(l)),
            _ => Err(darling::Error::too_many_items(1)),
        }
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(Self {
            attr: value.into(),
            value: None,
        })
    }
}

impl VariantDiscriminantOpts {
    fn set_static_attr_tokens(&self) -> Option<TokenStream> {
        if let VariantDiscriminantOpts {
            attr,
            value: Some(value),
        } = self
        {
            Some(quote! { . with_escaped_attribute(#attr, #value) ? })
        } else {
            None
        }
    }
}

#[derive(Debug, FromVariant)]
#[darling(attributes(vkxml))]
struct XMLSerializationDeriveVariant {
    ident: Ident,
    fields: darling::ast::Fields<SpannedValue<XMLSerializationDeriveField>>,
    // attributes
    #[darling(default, rename = "discriminant")]
    discriminant_attr_val: Option<VariantDiscriminantOpts>,
    #[darling(default)]
    tag: Option<TagAttr>,
}

impl XMLSerializationDeriveVariant {
    fn deserialization_impl(
        &self,
        enum_ident: &Ident,
        span: proc_macro2::Span,
        enum_is_elem: bool,
    ) -> syn::Result<TokenStream> {
        let Self {
            ident,
            fields,
            tag,
            discriminant_attr_val,
        } = self;

        let mut variant_path = Path::from(PathSegment::from(enum_ident.clone()));
        variant_path.segments.push(PathSegment::from(ident.clone()));

        match (
            fields.style,
            named_field_deserialization_guard(tag.as_ref(), discriminant_attr_val.as_ref()),
            fields.fields.as_slice(),
        ) {
            (darling::ast::Style::Tuple, Some(guard), [_]) => Ok(quote! {
                if #guard {
                    Ok(Some(#variant_path(crate::TryFromXML::from_xml(node)?)))
                }
            }),
            (darling::ast::Style::Tuple, None, [_]) => Ok(quote! {
                if let Some(v) = crate::TryFromXML::try_from_xml(node)? {
                    Ok(Some(#variant_path(v)))
                }
            }),
            (darling::ast::Style::Struct, Some(guard), fields) => {
                let inner = named_field_deserialization_impl(&variant_path, fields, false)?;
                Ok(quote! {
                    if #guard {
                        #inner
                    }
                })
            }
            (darling::ast::Style::Struct, None, fields) if enum_is_elem => {
                let inner = named_field_deserialization_impl(&variant_path, fields, false)?;
                Ok(quote! {
                    if true {
                        #inner
                    }
                })
            }
            (darling::ast::Style::Unit, _, _) => {
                Err(syn::Error::new(span, "Enum unit variants are unsupported"))
            }
            (darling::ast::Style::Tuple, None, _) => Err(syn::Error::new(
                span,
                "Only enum newtype tuple variants are supported for now",
            )),
            (darling::ast::Style::Tuple, Some(_), _) => {
                Err(syn::Error::new(span, "Enum tuple variants can't be tagged"))
            }
            (darling::ast::Style::Struct, None, _) => Err(syn::Error::new(
                span,
                "Enum struct variants must be be tagged",
            )),
        }
    }

    fn serialization_impl(
        &self,
        enum_ident: &Ident,
        span: proc_macro2::Span,
        enum_is_elem: bool,
    ) -> syn::Result<TokenStream> {
        let Self {
            ident,
            fields,
            tag,
            discriminant_attr_val,
        } = self;

        let mut variant_path = Path::from(PathSegment::from(enum_ident.clone()));
        variant_path.segments.push(PathSegment::from(ident.clone()));

        match (fields.style, tag, enum_is_elem, fields.fields.as_slice()) {
            (_, Some(_), true, _) => Err(syn::Error::new(
                span,
                "Enum variant can't be tagged, if the enum is tagged",
            )),
            (darling::ast::Style::Tuple, _, true, [_]) => {
                let set_static_attr = discriminant_attr_val
                    .as_ref()
                    .and_then(|v| v.set_static_attr_tokens());
                Ok(quote! {
                    #variant_path(v) => crate::IntoXMLElement::write_element_and_static_attrs(v, element #set_static_attr ),
                })
            }
            (darling::ast::Style::Tuple, None, _, [_]) => Ok(quote! {
                #variant_path(v) => crate::IntoXML::write_xml(v, writer),
            }),
            (darling::ast::Style::Struct, None, true, fields) => {
                let field_idents = fields.iter().map(|f| f.ident.as_ref().unwrap());
                let inner =
                    named_field_serialization_impl(fields, discriminant_attr_val.as_ref(), false)?;
                Ok(quote! {
                    #variant_path { #(#field_idents),* } => {
                        #inner
                    }
                })
            }
            (darling::ast::Style::Struct, Some(tag), false, fields) => {
                let field_idents = fields.iter().map(|f| f.ident.as_ref().unwrap());
                let inner =
                    named_field_serialization_impl(fields, discriminant_attr_val.as_ref(), false)?;
                Ok(quote! {
                    #variant_path { #(#field_idents),* } => {
                        let element = crate::XMLElementBuilder::new(writer, #tag)?;
                        #inner
                    }
                })
            }
            (darling::ast::Style::Unit, _, _, _) => {
                Err(syn::Error::new(span, "Enum unit variants are unsupported"))
            }
            (darling::ast::Style::Tuple, None, _, _) => Err(syn::Error::new(
                span,
                "Only enum newtype tuple variants are supported for now",
            )),
            (darling::ast::Style::Tuple, Some(_), _, _) => {
                Err(syn::Error::new(span, "Enum tuple variants can't be tagged"))
            }
            (darling::ast::Style::Struct, None, _, _) => Err(syn::Error::new(
                span,
                "Enum struct variants must be be tagged",
            )),
        }
    }
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(vkxml))]
pub struct XMLSerializationDeriveInput {
    ident: Ident,
    generics: syn::Generics,
    data: darling::ast::Data<
        SpannedValue<XMLSerializationDeriveVariant>,
        SpannedValue<XMLSerializationDeriveField>,
    >,
    // attributes
    #[darling(default)]
    tag: Option<TagAttr>,
    #[darling(default)]
    tokenized: Flag,
    #[darling(default)]
    discriminant: Option<VariantDiscriminantOpts>,
}

fn enum_impl(
    enum_ident: &Ident,
    ty_generics: syn::TypeGenerics,
    de_impl_generics: syn::ImplGenerics,
    ser_impl_generics: syn::ImplGenerics,
    de_where_clause: Option<&syn::WhereClause>,
    ser_where_clause: Option<&syn::WhereClause>,
    variants: &[SpannedValue<XMLSerializationDeriveVariant>],
    tag: Option<&TagAttr>,
    tokenized: bool,
) -> syn::Result<TokenStream> {
    if tokenized {
        if let Some(tag) = tag {
            return Ok(quote! {
                impl #de_impl_generics crate::TryFromXML<'de> for #enum_ident #ty_generics #de_where_clause {
                    fn try_from_xml<'input: 'de>(node: roxmltree::Node<'de, 'input>) -> crate::ParseResult<Option<Self>> {
                        if node.has_tag_name(#tag) {
                            Ok(Some(crate::TryFromTokens::try_from_node(node)?))
                        } else {
                            Ok(None)
                        }
                    }
                }

                impl #ser_impl_generics crate::IntoXMLElement for #enum_ident #ty_generics #ser_where_clause {
                    fn write_element<'w, W: ?Sized + crate::XMLWriter>(
                        &self,
                        element: crate::XMLElementBuilder<'static, 'w, W>,
                    ) -> Result<(), W::Error> {
                        element.write_tokens(self)
                    }
                }
            });
        } else {
            return Err(syn::Error::new(
                Span::call_site(),
                "tokenized enums must be tagged",
            ));
        }
    }
    let deser = variants
        .iter()
        .map(|v| v.deserialization_impl(enum_ident, v.span(), tag.is_some()))
        .collect::<syn::Result<Vec<_>>>()?;
    let ser = variants
        .iter()
        .map(|v| v.serialization_impl(enum_ident, v.span(), tag.is_some()))
        .collect::<syn::Result<Vec<_>>>()?;

    if let Some(tag) = tag {
        // FIXME
        Ok(quote! {
            impl #de_impl_generics crate::TryFromXML<'de> for #enum_ident #ty_generics #de_where_clause {
                fn try_from_xml<'input: 'de>(node: roxmltree::Node<'de, 'input>) -> crate::ParseResult<Option<Self>> {
                    if !node.has_tag_name(#tag) {
                        Ok(None)
                    } else
                    #(
                        #deser else
                    )*
                    {
                        Ok(None)
                    }
                }
            }

            impl #ser_impl_generics crate::IntoXMLElement for #enum_ident #ty_generics #ser_where_clause {
                fn write_element<'w, W: ?Sized + crate::XMLWriter>(
                    &self,
                    element: crate::XMLElementBuilder<'static, 'w, W>,
                ) -> Result<(), W::Error> {
                    match self {
                        #(#ser)*
                    }
                }
            }
        })
    } else {
        Ok(quote! {
            impl #de_impl_generics crate::TryFromXML<'de> for #enum_ident #ty_generics #de_where_clause {
                fn try_from_xml<'input: 'de>(node: roxmltree::Node<'de, 'input>) -> crate::ParseResult<Option<Self>> {
                    #(
                        #deser else
                    )*
                    {
                        Ok(None)
                    }
                }
            }

            impl #ser_impl_generics crate::IntoXML for #enum_ident #ty_generics #ser_where_clause {
                fn write_xml<W: ?Sized + crate::XMLWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
                    match self {
                        #(#ser)*
                    }
                }
            }
        })
    }
}

fn struct_impl(
    struct_ident: &Ident,
    ty_generics: syn::TypeGenerics,
    de_impl_generics: syn::ImplGenerics,
    ser_impl_generics: syn::ImplGenerics,
    de_where_clause: Option<&syn::WhereClause>,
    ser_where_clause: Option<&syn::WhereClause>,
    fields: &darling::ast::Fields<SpannedValue<XMLSerializationDeriveField>>,
    tag: Option<&TagAttr>,
    discriminant: Option<&VariantDiscriminantOpts>,
    tokenized: bool,
) -> syn::Result<TokenStream> {
    let struct_path = Path::from(PathSegment::from(struct_ident.clone()));
    let deser = named_field_deserialization_impl(&struct_path, &fields.fields, tokenized)?;
    let ser = named_field_serialization_impl(&fields.fields, discriminant, tokenized)?;
    let field_idents = fields
        .iter()
        .filter(|f| {
            f.flatten.is_present()
                || f.attribute_opts.is_some()
                || f.child_opts.is_present()
                || f.text.is_present()
        })
        .map(|f| f.ident.as_ref().unwrap());
    let deser_impl = if let Some(guard) = named_field_deserialization_guard(tag, discriminant) {
        quote! {
            if #guard {
                #deser
            } else {
                Ok(None)
            }
        }
    } else {
        deser
    };
    Ok(quote! {
        impl #de_impl_generics crate::TryFromXML<'de> for #struct_ident #ty_generics #de_where_clause {
            fn try_from_xml<'input: 'de>(node: roxmltree::Node<'de, 'input>) -> crate::ParseResult<Option<Self>> {
                #deser_impl
            }
        }

        impl #ser_impl_generics crate::IntoXMLElement for #struct_ident #ty_generics #ser_where_clause {
            fn write_element<'w, W: ?Sized + crate::XMLWriter>(&self, element: crate::XMLElementBuilder<'static, 'w, W>) -> Result<(), W::Error> {
                let Self { #(#field_idents,)* .. } = self;
                #ser
            }
        }
    })
}

impl XMLSerializationDeriveInput {
    fn into_token_stream(&self) -> syn::Result<TokenStream> {
        let ident = &self.ident;

        let (_, ty_generics, _) = self.generics.split_for_impl();
        let mut de_generics = self.generics.clone();
        let mut ser_generics = self.generics.clone();

        let deserialization_lifetime = syn::GenericParam::Lifetime(syn::LifetimeParam {
            attrs: Default::default(),
            lifetime: syn::Lifetime::new("'de", proc_macro2::Span::call_site()),
            colon_token: Default::default(),
            bounds: self
                .generics
                .lifetimes()
                .map(|def| def.lifetime.clone())
                .collect(),
        });
        de_generics.params.push(deserialization_lifetime);
        de_generics
            .type_params_mut()
            .for_each(|typ| typ.bounds.push(syn::parse_quote!(crate::TryFromXML<'de>)));
        ser_generics.type_params_mut().for_each(|typ| {
            typ.bounds.push(if self.tag.is_some() {
                syn::parse_quote!(crate::IntoXMLElement)
            } else {
                syn::parse_quote!(crate::IntoXML)
            })
        });

        let (de_impl_generics, _, de_where_clause) = de_generics.split_for_impl();
        let (ser_impl_generics, _, ser_where_clause) = ser_generics.split_for_impl();

        let xml_impl = match &self.data {
            darling::ast::Data::Enum(variants) => enum_impl(
                ident,
                ty_generics,
                de_impl_generics,
                ser_impl_generics,
                de_where_clause,
                ser_where_clause,
                &variants,
                self.tag.as_ref(),
                self.tokenized.is_present(),
            ),
            darling::ast::Data::Struct(fields) => struct_impl(
                ident,
                ty_generics,
                de_impl_generics,
                ser_impl_generics,
                de_where_clause,
                ser_where_clause,
                &fields,
                self.tag.as_ref(),
                self.discriminant.as_ref(),
                self.tokenized.is_present(),
            ),
        }?;
        Ok(if let Some(tag) = self.tag.as_ref() {
            let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
            quote! {
                impl #impl_generics crate::Tagged for #ident #ty_generics #where_clause {
                    const TAG: &'static str = #tag;
                }
                #xml_impl
            }
        } else {
            xml_impl
        })
    }
}

pub fn derive_xml_serialization(input: &DeriveInput) -> TokenStream {
    (XMLSerializationDeriveInput::from_derive_input(input)).map_or_else(
        |e| e.write_errors(),
        |opts| match opts.into_token_stream() {
            Ok(ts) => ts,
            Err(e) => e.into_compile_error(),
        },
    )
}
