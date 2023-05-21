mod serialization;
use crate::serialization::derive_xml_serialization;

#[proc_macro_derive(XMLSerialization, attributes(xml))]
pub fn xml_serialization(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    derive_xml_serialization(&input).into()
}

#[proc_macro_derive(TryFromEscapedStr)]
pub fn try_from_esc_str(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    match derive_try_from_esc_str(&input) {
        Ok(ts) => ts,
        Err(e) => e.into_compile_error(),
    }
    .into()
}

#[proc_macro_derive(DisplayEscaped)]
pub fn display_escaped(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    match derive_display_escaped(&input) {
        Ok(ts) => ts,
        Err(e) => e.into_compile_error(),
    }
    .into()
}

fn derive_try_from_esc_str(input: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let syn::DeriveInput {
        ident, generics, ..
    } = input;

    let mut de_generics = generics.clone();
    let deserialization_lifetime = syn::LifetimeParam {
        attrs: Default::default(),
        lifetime: syn::Lifetime::new("'de", proc_macro2::Span::call_site()),
        colon_token: Default::default(),
        bounds: generics
            .lifetimes()
            .map(|def| def.lifetime.clone())
            .collect(),
    };
    let is_owned = deserialization_lifetime.bounds.is_empty();
    de_generics
        .params
        .push(syn::GenericParam::Lifetime(deserialization_lifetime));

    let (_, ty_generics, _) = generics.split_for_impl();
    let (impl_generics, _, where_clause) = de_generics.split_for_impl();

    if is_owned {
        Ok(quote::quote! {
            impl #impl_generics crate::TryFromEscapedStr<'de> for #ident #ty_generics #where_clause {
                type Error = <Self as core::str::FromStr>::Err;

                fn try_from_escaped_str(s: &'de str) -> Result<Self, Self::Error> {
                    core::str::FromStr::from_str(s)
                }
            }
        })
    } else {
        Ok(quote::quote! {
            impl #impl_generics crate::TryFromEscapedStr<'de> for #ident #ty_generics #where_clause {
                type Error = <Self as TryFrom<&'de str>>::Error;

                fn try_from_escaped_str(s: &'de str) -> Result<Self, Self::Error> {
                    TryFrom::try_from(s)
                }
            }
        })
    }
}

fn derive_display_escaped(input: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let syn::DeriveInput {
        ident, generics, ..
    } = input;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    Ok(quote::quote! {
        impl #impl_generics crate::DisplayEscaped for #ident #ty_generics #where_clause {}
    })
}
