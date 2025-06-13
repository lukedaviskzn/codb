use std::str::FromStr;

use syn::LitStr;

#[proc_macro]
pub fn id(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse::<LitStr>(input).expect("parameter not a string literal");
    let value = input.value();
    let _ = codb_core::Ident::from_str(&value).expect("parameter is not a valid nested identifier");
    
    quote::quote! {
        {
            use core::str::FromStr;
            codb_core::Ident::from_str(#input).expect("unreachable")
        }
    }.into()
}

#[proc_macro]
pub fn nested_id(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse::<LitStr>(input).expect("parameter not a string literal");
    let value = input.value();
    let _ = codb_core::NestedIdent::from_str(&value).expect("parameter is not a valid nested identifier");
    
    quote::quote! {
        {
            use core::str::FromStr;
            codb_core::NestedIdent::from_str(#input).expect("unreachable")
        }
    }.into()
}

#[proc_macro]
pub fn id_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse::<LitStr>(input).expect("parameter not a string literal");
    let value = input.value();
    let _ = codb_core::IdentPath::from_str(&value).expect("parameter is not a valid nested identifier");
    
    quote::quote! {
        {
            use core::str::FromStr;
            codb_core::IdentPath::from_str(#input).expect("unreachable")
        }
    }.into()
}

#[proc_macro]
pub fn parse_u8(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse::<LitStr>(input).expect("parameter not a string literal");
    let value = input.value();
    let _ = u8::from_str(&value).expect("parameter is not a valid u8");
    
    quote::quote! {
        {
            use core::str::FromStr;
            codb_core::Ident::from_str(#input).expect("unreachable")
        }
    }.into()
}
