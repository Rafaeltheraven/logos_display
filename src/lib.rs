
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::matches;

use proc_macro2::{TokenTree, Spacing};
use syn::{parse_macro_input, Result, spanned::Spanned, DeriveInput, DataEnum, Ident, Lit};
use quote::{ToTokens, quote};

#[proc_macro_derive(Display, attributes(display_override))]
pub fn logos_display(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let span = ast.span();
    let ident = ast.ident;
    let resp = match ast.data {
        syn::Data::Enum(e) => logos_display_derive(e, &ident),
        _ => Err(syn::Error::new(span, "Can only derive display for enums"))
    };
    match resp {
        Ok(stream) => {
            quote! {
                impl std::fmt::Display for #ident {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        let ret = match &self {
                            #stream
                        };
                        write!(f, "{}", ret)
                    }
                }
            }.into()
        },
        Err(e) => e.to_compile_error().into()
    }
}

fn logos_display_derive(e: DataEnum, ident: &Ident) -> Result<TokenStream2> {
    let mut repr_map: Vec<(TokenStream2, TokenStream2)> = Vec::with_capacity(e.variants.len());
    for variant in e.variants.into_iter() {
        let args = match variant.fields {
            syn::Fields::Named(_) | syn::Fields::Unnamed(_) => quote!{(..)},
            syn::Fields::Unit => TokenStream2::new(),
        };
        let ident = variant.ident;
        let mut repr = ident.to_string().into_token_stream();
        let mut found = None;
        for attr in variant.attrs.into_iter() {
            if let syn::Meta::List(l) = attr.meta {
                if l.path.is_ident("display_override") {
                    found = Some(l.tokens.to_string());
                    break;
                } else if l.path.is_ident("token") {
                    let mut new_stream = TokenStream2::new();
                    let span = l.span();
                    for tt in l.tokens.into_iter() {
                        if matches!(tt, TokenTree::Punct(ref punct) if punct.as_char() == ',' && punct.spacing() == Spacing::Alone) {
                            break;
                        } else {
                            new_stream.extend(Some(tt));
                        }
                    }
                    let lit: Lit = syn::parse2(new_stream)?;
                    if let Lit::Str(s) = lit {
                        let string = s.value();
                        if let Some(f) = found {
                            found = Some(format!("{}/{}", f, string))
                        } else {
                            found = Some(string);
                        }
                    } else {
                        return Err(syn::Error::new(span, "Error extracting token from attribute, not a string"))
                    }
                }
            }
        }
        if let Some(string) = found {
            repr = string.into_token_stream();
        }
        repr_map.push((quote!(#ident #args), repr));
    }
    let arms: Vec<TokenStream2> = repr_map.iter().map(|(k, v)| quote!(#ident::#k => #v,)).collect();
    Ok(quote!(#( #arms )*))
} 
