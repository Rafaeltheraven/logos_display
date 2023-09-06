
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::matches;

use proc_macro2::{TokenTree, Spacing};
use syn::{Result, spanned::Spanned, DeriveInput, DataEnum, Ident, Lit, LitStr};
use quote::{ToTokens, quote};

#[proc_macro_derive(Display, attributes(display_override, display_concat))]
pub fn logos_display(input: TokenStream) -> TokenStream {
    _logos_display(input.into(), false).into()
}

#[proc_macro_derive(Debug, attributes(display_override, display_concat))]
pub fn logos_debug(input: TokenStream) -> TokenStream {
    _logos_display(input.into(), true).into()
}

fn _logos_display(input: TokenStream2, debug: bool) -> TokenStream2 {
    let ast = match syn::parse2::<DeriveInput>(input) {
        Ok(res) => res,
        Err(e) => return e.to_compile_error()
    };
    let span = ast.span();
    let ident = ast.ident;
    let mut concat = Some("/".to_string());
    for attr in ast.attrs.into_iter() {
        if let syn::Meta::List(l) = attr.meta {
            if l.path.is_ident("display_concat") {
                let as_str = l.tokens.to_string();
                let cand = match syn::parse2::<LitStr>(l.tokens) {
                    Ok(res) => Ok(res.value()),
                    Err(e) => {
                        let resp = as_str;
                        if resp == "None" {
                            Ok(resp)
                        } else {
                            Err(syn::Error::new(e.span(), "Concat must be either a string or None"))
                        }
                    }
                };
                let litstr = match cand {
                    Ok(res) => res,
                    Err(e) => return e.to_compile_error()
                };
                if litstr == "None" {
                    concat = None;
                } else {
                    concat = Some(litstr);
                }
                break;
            }
        }
    }
    let resp = match ast.data {
        syn::Data::Enum(e) => logos_display_derive(e, &ident, concat, debug),
        _ => Err(syn::Error::new(span, "Can only derive display for enums"))
    };
    let traitt = if debug {
        quote!(std::fmt::Debug)
    } else {
        quote!(std::fmt::Display)
    };
    match resp {
        Ok(stream) => {
            quote! {
                impl #traitt for #ident {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        let ret = match &self {
                            #stream
                        };
                        write!(f, "{}", ret)
                    }
                }
            }
        },
        Err(e) => e.to_compile_error()
    }
}

fn gen_anon_args(n: usize) -> TokenStream2 {
    let mut args = Vec::new();
    for i in 1..=n {
        let arg_ident = Ident::new(&format!("_arg{}", i), proc_macro2::Span::call_site());
        args.push(quote!(#arg_ident));
    }
    quote!(#(#args),*)
}

fn logos_display_derive(e: DataEnum, ident: &Ident, concat: Option<String>, include_inner: bool) -> Result<TokenStream2> {
    let mut repr_map: Vec<(TokenStream2, TokenStream2)> = Vec::with_capacity(e.variants.len());
    for variant in e.variants.into_iter() {
        let args = match variant.fields {
            syn::Fields::Named(f) if include_inner => {
                let list = gen_anon_args(f.named.len());
                Some(quote!({#list}))
            }
            syn::Fields::Unnamed(f) if include_inner => {
                let list = gen_anon_args(f.unnamed.len());
                Some(quote!((#list)))
            },
            syn::Fields::Named(..) => Some(quote!({..})),
            syn::Fields::Unnamed(..) => Some(quote!((..))),
            syn::Fields::Unit => None,
        };
        let id = variant.ident;
        let mut repr = id.to_string().into_token_stream();
        let mut found = None;
        for attr in variant.attrs.into_iter() {
            if let syn::Meta::List(l) = attr.meta {
                if l.path.is_ident("display_override") {
                    let litstr = match syn::parse2::<LitStr>(l.tokens) {
                        Ok(res) => res,
                        Err(e) => return Err(e)
                    };
                    found = Some(litstr.value());
                    break;
                } else if l.path.is_ident("token") || l.path.is_ident("regex") {
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
                            if let Some(ref conc) = concat {
                                found = Some(format!("{}{}{}", f, conc, string))
                            } else {
                                found = Some(string);
                            }
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
        if let Some(list) = args {
            if include_inner {
                repr_map.push((quote!(#id #list), quote!(format!("{}{:?}", #repr, vec!#list))));
            } else {
                repr_map.push((quote!(#id #list), quote!(#repr.to_string())));
            }
        } else {
            repr_map.push((quote!(#id), quote!(#repr.to_string())));
        }
    }
    let arms: Vec<TokenStream2> = repr_map.iter().map(|(k, v)| quote!(#ident::#k => #v,)).collect();
    Ok(quote!(#( #arms )*))
} 

#[cfg(test)]
mod tests {
    use super::_logos_display;
    use proc_macro2::TokenStream;
    use quote::quote;
    use assert_tokenstreams_eq::assert_tokenstreams_eq;

    fn expect(arms: TokenStream, debug: bool) -> TokenStream {
        let traitt = if debug {
            quote!(std::fmt::Debug)
        } else {
            quote!(std::fmt::Display)
        };
        quote!(
            impl #traitt for A {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    let ret = match &self {
                        #arms
                    };
                    write!(f, "{}", ret)
                }
            }
        )
    }

    #[test]
    fn test_basic() {
        let input = quote!(
            enum A {
                #[token("{")]
                LCur,

                #[regex("}")]
                RCur
            }
        );
        let arms = quote!(
            A::LCur => "{".to_string(),
            A::RCur => "}".to_string(),
        );
        let expected = expect(arms, false);
        let result = _logos_display(input, false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_override() {
        let input = quote!(
            enum A {
                #[display_override("fancy curly thing")]
                #[token("{")]
                LCur,

                #[regex("}")]
                RCur,

                #[token("-")]
                #[display_override("dash")]
                Minus
            }
        );
        let arms = quote!(
            A::LCur => "fancy curly thing".to_string(),
            A::RCur => "}".to_string(),
            A::Minus => "dash".to_string(),
        );
        let expected = expect(arms, false);
        let result = _logos_display(input, false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_quoted_string() {
        let input = quote!(
            enum A {
                #[token("{")]
                LCur,

                #[regex("\".*\"")]
                RCur
            }
        );
        let arms = quote!(
            A::LCur => "{".to_string(),
            A::RCur => "\".*\"".to_string(),
        );
        let expected = expect(arms, false);
        let result = _logos_display(input,false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_raw_string() {
        let input = quote!(
            enum A {
                #[token("{")]
                LCur,

                #[regex(r#"".*""#)]
                RCur
            }
        );
        let arms = quote!(
            A::LCur => "{".to_string(),
            A::RCur => "\".*\"".to_string()
        );
        let expected = expect(arms, false);
        let result = _logos_display(input, false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_concat_basic() {
        let input = quote!(
            enum A {
                #[token("{")]
                #[token("}")]
                Cur,
            }
        );
        let arms = quote!(
            A::Cur => "{/}".to_string(),
        );
        let expected = expect(arms, false);
        let result = _logos_display(input, false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_concat_some() {
        let input = quote!(
            #[display_concat(" or ")]
            enum A {
                #[token("{")]
                #[token("}")]
                Cur,
            }
        );
        let arms = quote!(
            A::Cur => "{ or }".to_string(),
        );
        let expected = expect(arms, false);
        let result = _logos_display(input, false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_concat_none() {
        let input = quote!(
            #[display_concat(None)]
            enum A {
                #[token("{")]
                #[token("}")]
                Cur,
            }
        );
        let arms = quote!(
            A::Cur => "}".to_string(),
        );
        let expected = expect(arms, false);
        let result = _logos_display(input, false);
        assert_tokenstreams_eq!(&result, &expected);
    }

    #[test]
    fn test_with_args() {
        let input = quote!(
            enum A {
                #[regex("[a-z]", |lex| funny_business(lex.slice()))]
                Reg(First, Second, Third),

                #[regex("[A-Z]", |lex| more_funny(lex.slice()))]
                Reg2 {
                    first: Type,
                    second: Another
                }
            }
        );
        let arms_debug = quote!(
            A::Reg(_arg1, _arg2, _arg3) => format!("{}{:?}", "[a-z]", vec!(_arg1, _arg2, _arg3)),
            A::Reg2{_arg1, _arg2} => format!("{}{:?}", "[A-Z]", vec!{_arg1, _arg2}),
        );
        let expected_debug = expect(arms_debug, true);
        let result_debug = _logos_display(input.clone(), true);
        assert_tokenstreams_eq!(&result_debug, &expected_debug);
        let arms_display = quote!(
            A::Reg(..) => "[a-z]".to_string(),
            A::Reg2{..} => "[A-Z]".to_string(),
        );
        let expected_display = expect(arms_display, false);
        let result_display = _logos_display(input, false);
        assert_tokenstreams_eq!(&result_display, &expected_display);
    }
}