use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{ItemStruct, Lit, parse_macro_input};

use crate::{FieldData, get_fields_to_add};

pub fn uci_fields_struct_impl(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let ItemStruct {
        attrs: _,
        vis,
        struct_token: _,
        ident,
        generics,
        fields: _,
        semi_token: _,
    } = parse_macro_input!(input as ItemStruct);

    let optional_ident = format_ident!("{}AsOptions", ident);
    let fields_to_add_data = get_fields_to_add();
    let new_fields = fields_to_add_data.iter().map(|d| format_as_pub_field(d, false));
    let new_fields_as_options = fields_to_add_data.iter().map(|d| format_as_pub_field(d, true));
    let field_names = fields_to_add_data.iter().map(|f| &f.0);
    let assign_default_values = fields_to_add_data.iter().map(assign_default_value);
    let print_uci_options = fields_to_add_data.iter().map(|f| print_field_uci_option(f));

    quote! {
        /// Fields will be added to this struct according to the fields listed in fox-chess-proc/src/lib.rs.
        /// See the macro attached to this struct for more details.
        #vis struct #ident #generics {
            #(#new_fields),*
        }

        impl Default for #ident {
            fn default() -> Self {
                Self {
                    #(#assign_default_values),*
                }
            }
        }

        impl #ident {
            pub fn print_uci_options() {
                #(#print_uci_options)*
            }
        }

        #[derive(std::default::Default)]
        #vis struct #optional_ident {
            #(#new_fields_as_options),*
        }

        impl #optional_ident {
            pub fn convert(&self) -> #ident {
                #ident {
                    #(#field_names: self.#field_names.clone().expect(&format!("{} should have a value", stringify!(#field_names)))),*
                }
            }
        }
    }.into()
}

fn format_as_pub_field(field_data: &FieldData, make_optional: bool) -> TokenStream {
    let ident = &field_data.0;
    let ty = &field_data.1;
    if make_optional {
        quote! {
            pub #ident: Option<#ty>
        }
    } else {
        quote! {
            pub #ident: #ty
        }
    }
}

fn assign_default_value(field_data: &FieldData) -> TokenStream {
    let ident = &field_data.0;
    let default_value = &field_data.2;

    match default_value {
        Lit::Str(_) => {
            quote! {
                #ident: #default_value.to_string()
            }
        }
        _ => quote! {
            #ident: #default_value
        },
    }
}

fn print_field_uci_option(field_data: &FieldData) -> TokenStream {
    let ident = &field_data.0;
    // let ty = &field_data.1;
    let default_value = &field_data.2;

    // let type_ident = ty.path.segments.last().unwrap().ident.to_string();
    let uci_type_and_others = match default_value {
        Lit::Str(lit_str) => format!("type string default {}", lit_str.value()),
        Lit::Int(lit_int) => format!(
            "type spin default {} min {} max {}",
            lit_int.base10_digits(),
            i32::MIN,
            i32::MAX
        ),
        Lit::Float(lit_float) => format!(
            "type spin default {} min {} max {}",
            lit_float.base10_digits(),
            f64::MIN,
            f64::MAX
        ),
        Lit::Bool(lit_bool) => format!("type check default {}", lit_bool.value()),
        _ => panic!("Unexpected literal type for {}", ident),
    };

    quote! {
        std::println!("option name {} {}", stringify!(#ident), #uci_type_and_others);
    }
}
