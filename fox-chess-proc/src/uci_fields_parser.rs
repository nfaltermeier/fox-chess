use quote::quote;
use syn::{
    Arm, ExprMatch, Ident, Item, Pat, Path, parse_macro_input, parse_quote,
    visit_mut::{self, VisitMut},
};

use crate::{FieldData, get_fields_to_add};

struct FieldsParserVisitor {
    self_uci_fields_as_options_field: Ident,
}

impl VisitMut for FieldsParserVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        if i.attrs.iter().any(|a| path_is("uci_fields_parser", a.path())) {
            i.attrs.retain(|a| !path_is("uci_fields_parser", a.path()));

            let fields_to_add_data = get_fields_to_add();
            let new_arms = fields_to_add_data
                .iter()
                .map(|f| create_new_arm(f, &self.self_uci_fields_as_options_field));

            i.arms.extend(new_arms);

            // Ensure the wildcard test (if it exists) is last
            i.arms.sort_by_key(|a| matches!(a.pat, Pat::Wild(_)));
        }

        visit_mut::visit_expr_match_mut(self, i);
    }
}

pub fn uci_fields_parser_impl(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let self_uci_fields_as_options_field = parse_macro_input!(args as Ident);
    let mut input = parse_macro_input!(input as Item);

    let mut fields_parser_visitor = FieldsParserVisitor {
        self_uci_fields_as_options_field,
    };

    visit_mut::visit_item_mut(&mut fields_parser_visitor, &mut input);

    quote! { #input }.into()
}

fn create_new_arm(field: &FieldData, self_uci_fields_as_options_field: &Ident) -> Arm {
    let ident = &field.0;
    let ty = &field.1;
    let ident_str = ident.to_string().to_ascii_lowercase();

    parse_quote! {
        #ident_str => {
            if let Some(value) = value {
                let parsed_value = value.parse::<#ty>();
                if let Ok(parsed_value) = parsed_value {
                    self.#self_uci_fields_as_options_field.#ident = Some(parsed_value);
                } else {
                    log::error!("Failed to parse value as a {}: {}", stringify!(#ty), parsed_value.unwrap_err());
                }
            } else {
                log::error!("Expected a value for option {}", #ident_str);
            }
        }
    }
}

fn path_is(target_name: &str, path: &Path) -> bool {
    path.get_ident()
        .is_some_and(|ident| ident == &Ident::new(target_name, ident.span()))
}
