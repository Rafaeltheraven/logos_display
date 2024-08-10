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
        quote!(core::fmt::Debug)
    } else {
        quote!(core::fmt::Display)
    };
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
    match resp {
        Ok(stream) => {
            quote! {
                impl #impl_generics #traitt for #ident #ty_generics #where_clause {
                    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                        use core::fmt::Write;
                        match &self {
                            #stream
                        }
                    }
                }
            }
        }
        Err(e) => e.to_compile_error()
    }
}

fn gen_anon_args(n: usize) -> Vec<proc_macro2::TokenStream> {
    let mut args = Vec::new();
    for i in 1..=n {
        let arg_ident = Ident::new(&format!("_arg{}", i), proc_macro2::Span::call_site());
        args.push(quote!(#arg_ident));
    }
    args
}

fn logos_display_derive(e: DataEnum, ident: &Ident, concat: Option<String>, include_inner: bool) -> Result<TokenStream2> {
    let mut repr_map: Vec<(TokenStream2, TokenStream2)> = Vec::with_capacity(e.variants.len());
    for variant in e.variants.into_iter() {
        let res = match variant.fields {
            syn::Fields::Named(f) if include_inner => {
                let args = gen_anon_args(f.named.len());
                let key_list = quote!(#(#args),*);
                let key_list = quote!({#key_list});
                let ref_array = quote!(#(&#args),*);
                Some((key_list, Some(ref_array)))
            }
            syn::Fields::Unnamed(f) if include_inner => {
                let args = gen_anon_args(f.unnamed.len());
                let key_list = quote!(#(#args),*);
                let key_list = quote!((#key_list));
                let ref_array = quote!(#(&#args),*);
                Some((key_list, Some(ref_array)))
            }
            syn::Fields::Named(..) => {
                let key_list = quote!({..});
                Some((key_list, None))
            }
            syn::Fields::Unnamed(..) => {
                let key_list = quote!((..));
                Some((key_list, None))
            }
            _ => None,
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
                        return Err(syn::Error::new(span, "Error extracting token from attribute, not a string"));
                    }
                }
            }
        }
        if let Some(string) = found {
            repr = string.into_token_stream();
        }
        if let Some((key_list, ref_array)) = res {
            match ref_array {
                None => repr_map.push((quote!(#id #key_list), quote!(write!(f, "{}", #repr)))),
                Some(ref_array) => repr_map.push((quote!(#id #key_list), quote!(write!(f, "{}{:?}", #repr, [#ref_array]))))
            }
        } else {
            repr_map.push((quote!(#id), quote!(write!(f, "{}", #repr))));
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


    fn expect(arms: TokenStream, debug: bool, generic_lifetimes: Option<&[TokenStream]>, where_clauses: Option<&[TokenStream]>) -> TokenStream {
        let traitt = if debug {
            quote!(core::fmt::Debug)
        } else {
            quote!(core::fmt::Display)
        };

        let generic_lifetimes = match generic_lifetimes {
            None => quote!(),
            Some(generic_lifetimes) => quote!(<#(#generic_lifetimes),*>)
        };

        let where_clauses = match where_clauses {
            None => quote!(),
            Some(where_clauses) => quote!(where #(#where_clauses),*)
        };

        quote!(
            impl #generic_lifetimes #traitt for A #generic_lifetimes #where_clauses {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    use core::fmt::Write;
                    match &self {
                        #arms
                    }
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
            A::LCur => write!(f, "{}" ,"{"),
            A::RCur => write!(f, "{}", "}"),
        );
        let expected = expect(arms, false, None, None);
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
            A::LCur => write!(f, "{}", "fancy curly thing"),
            A::RCur => write!(f, "{}", "}"),
            A::Minus => write!(f, "{}", "dash"),
        );
        let expected = expect(arms, false, None, None);
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
            A::LCur => write!(f, "{}", "{"),
            A::RCur => write!(f, "{}", "\".*\""),
        );
        let expected = expect(arms, false, None, None);
        let result = _logos_display(input, false);
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
            A::LCur => write!(f, "{}", "{"),
            A::RCur => write!(f, "{}", "\".*\""),
        );
        let expected = expect(arms, false, None, None);
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
            A::Cur => write!(f, "{}", "{/}"),
        );
        let expected = expect(arms, false, None, None);
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
            A::Cur => write!(f, "{}", "{ or }"),
        );
        let expected = expect(arms, false, None, None);
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
            A::Cur => write!(f, "{}", "}"),
        );
        let expected = expect(arms, false, None, None);
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
            A::Reg(_arg1, _arg2, _arg3) => write!(f, "{}{:?}", "[a-z]", [&_arg1, &_arg2, &_arg3]),
            A::Reg2{_arg1, _arg2} => write!(f, "{}{:?}", "[A-Z]", [&_arg1, &_arg2]),
        );
        let expected_debug = expect(arms_debug, true, None, None);
        let result_debug = _logos_display(input.clone(), true);
        assert_tokenstreams_eq!(&result_debug, &expected_debug);
        let arms_display = quote!(
            A::Reg(..) => write!(f, "{}", "[a-z]"),
            A::Reg2{..} => write!(f, "{}", "[A-Z]"),
        );
        let expected_display = expect(arms_display, false, None, None);
        let result_display = _logos_display(input, false);
        assert_tokenstreams_eq!(&result_display, &expected_display);
    }

    #[test]
    fn test_with_generics()
    {
        let input = quote!(
            enum A<'a, 'b, T, U> where T:U {
                #[regex("[a-z]", |lex| funny_business(lex.slice()))]
                Reg(First<'a>, Second<'b>, Third<'a>, T),

                #[regex("[A-Z]", |lex| more_funny(lex.slice()))]
                Reg2 {
                    first: Type<'a>,
                    second: Another<'b>,
                    third: U
                }
            }
        );
        let arms_debug = quote!(
            A::Reg(_arg1, _arg2, _arg3, _arg4) => write!(f, "{}{:?}", "[a-z]", [&_arg1, &_arg2, &_arg3, &_arg4]),
            A::Reg2{_arg1, _arg2, _arg3} => write!(f, "{}{:?}", "[A-Z]", [&_arg1, &_arg2, &_arg3]),
        );
        let generics = [quote!('a), quote!('b), quote!(T), quote!(U)];
        let wheres = [quote!(T:U)];
        let expected_debug = expect(arms_debug, true, Some(&generics), Some(&wheres));
        let result_debug = _logos_display(input.clone(), true);
        assert_tokenstreams_eq!(&result_debug, &expected_debug);
        let arms_display = quote!(
            A::Reg(..) => write!(f, "{}", "[a-z]"),
            A::Reg2{..} => write!(f, "{}", "[A-Z]"),
        );
        let expected_display = expect(arms_display, false, Some(&generics), Some(&wheres));
        let result_display = _logos_display(input, false);
        assert_tokenstreams_eq!(&result_display, &expected_display);
    }
}