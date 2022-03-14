use proc_macro::TokenStream;

use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, LitStr};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_builder(input) {
        Ok(stream) => stream,
        Err(e) => e.to_compile_error().into(),
    }
}

fn derive_builder(input: DeriveInput) -> syn::Result<TokenStream> {
    let name = &input.ident;
    let builder_id = syn::Ident::new(&format!("{name}Builder"), name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        named
    } else {
        unimplemented!();
    };

    let builder_fields = fields.iter().map(builder_field);
    let methods = fields
        .iter()
        .map(setters)
        .collect::<Result<Vec<_>, syn::Error>>()?;
    let build_fields = fields.iter().map(build_field);
    let init_fields = fields.iter().map(init_field);

    let expanded = quote! {
        pub struct #builder_id {
            #(#builder_fields,)*
        }

        impl #builder_id {
            #(#methods)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#build_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_id {
                #builder_id {
                    #(#init_fields,)*
                }
            }
        }
    };
    Ok(expanded.into())
}

fn builder_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let ty = &field.ty;
    let value = if option_inner_type(ty).is_some() || vec_inner_type(ty).is_some() {
        quote! { #ty }
    } else {
        quote! { std::option::Option<#ty> }
    };
    quote! { #name: #value }
}

fn init_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let value = if vec_inner_type(&field.ty).is_some() {
        quote! { std::vec::Vec::new() }
    } else {
        quote! { std::option::Option::None }
    };
    quote! { #name: #value }
}

fn setters(field: &syn::Field) -> syn::Result<proc_macro2::TokenStream> {
    let name = &field.ident;
    let ty = &field.ty;

    let each_method = each_method(field)?;
    let name_conflicts = if let Some(n) = &name {
        each_method
            .as_ref()
            .map(|(name, _)| n == name)
            .unwrap_or(false)
    } else {
        false
    };

    if name_conflicts {
        return Ok(each_method.unwrap().1);
    }

    let each_method = each_method.map(|(_, method)| method);
    let setter = if vec_inner_type(ty).is_some() {
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = #name;
                self
            }
        }
    } else {
        let inner_ty = option_inner_type(ty).unwrap_or(ty);
        quote! {
            pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    };

    Ok(quote! {
        #each_method
        #setter
    })
}

fn each_method(field: &syn::Field) -> syn::Result<Option<(String, proc_macro2::TokenStream)>> {
    let name = match field.ident.as_ref() {
        Some(name) => name,
        None => return Ok(None),
    };
    let ty = &field.ty;

    let builder_attr = field.attrs.iter().find(|a| a.path.is_ident("builder"));
    let builder_attr = match builder_attr {
        Some(ba) => ba,
        None => return Ok(None),
    };
    let meta = builder_attr.parse_meta()?;
    let each_name = match extract_each_attr_method_name(&meta)? {
        Some(each_name) => each_name,
        None => return Ok(None),
    };
    let lit = syn::Ident::new(&each_name.value(), each_name.span());

    let vec_ty = vec_inner_type(ty);
    let tokens = quote! {
        pub fn #lit(&mut self, #lit: #vec_ty) -> &mut Self {
            self.#name.push(#lit);
            self
        }
    };

    Ok(Some((each_name.value(), tokens)))
}

fn extract_each_attr_method_name(builder_meta: &syn::Meta) -> syn::Result<Option<LitStr>> {
    let builder_attrs = match builder_meta {
        syn::Meta::List(l) => &l.nested,
        m => return Err(syn::Error::new(m.span(), "expected list of attributes")),
    };

    for attr_meta in builder_attrs {
        let attr = match attr_meta {
            syn::NestedMeta::Meta(syn::Meta::NameValue(attr_meta)) => attr_meta,
            m => {
                return Err(syn::Error::new(
                    m.span(),
                    "expected only name-value attributes",
                ))
            }
        };
        if !attr.path.is_ident("each") {
            return Err(syn::Error::new_spanned(
                builder_meta,
                "expected `builder(each = \"...\")`",
            ));
        }

        let method_name = match &attr.lit {
            syn::Lit::Str(lit) => lit.clone(),
            lit => return Err(syn::Error::new(lit.span(), "expected string literal")),
        };
        return Ok(Some(method_name));
    }

    Ok(None)
}

fn build_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    if option_inner_type(&field.ty).is_some() || vec_inner_type(&field.ty).is_some() {
        quote! {
            #name: self.#name.clone()
        }
    } else {
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    }
}

fn inner_type<'a>(ty: &'a syn::Type, outer: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(p) = ty {
        if !(p.path.segments.len() == 1 && p.path.segments[0].ident == outer) {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(inner_ty) = &p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    inner_type(ty, "Option")
}

fn vec_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    inner_type(ty, "Vec")
}
