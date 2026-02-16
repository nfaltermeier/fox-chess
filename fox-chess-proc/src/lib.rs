use quote::format_ident;
use syn::{Ident, Lit, TypePath, parse_str};

use crate::{uci_fields_parser::uci_fields_parser_impl, uci_fields_struct::uci_fields_struct_impl};

mod uci_fields_parser;
mod uci_fields_struct;

pub(crate) type FieldData = (Ident, TypePath, Lit);

pub(crate) fn get_fields_to_add() -> Vec<FieldData> {
    // Order in tuple is variable name (also uci option name), type to parse and store as, and default value
    let fields_data: Vec<(&str, &str, &str)> = Vec::from([]);

    fields_data
        .iter()
        .map(|(ident, ty, lit)| {
            (
                format_ident!("{}", ident),
                parse_str(ty).unwrap(),
                parse_str(lit).unwrap(),
            )
        })
        .collect()
}

/// Marks a struct to be rewritten to have fields for each of the fields listed in the macro crate's [get_fields_to_add()] method in lib.rs.
///
/// Another struct will also be generated, named `<existing struct>AsOptions`. This struct will have the same fields, with each value wrapped in an [Option].
/// This alternate struct will also have a method `convert()` that will unwrap each of the fields and return an instance of the main struct.
#[proc_macro_attribute]
pub fn uci_fields_struct(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    uci_fields_struct_impl(args, input)
}

/// Meant to be attached to a method. This macro looks for `match` statements that themselves have the `#[uci_fields_parser]` attribute in the attached method.
/// It will add additional arms to the match statement to parse and store the values for the fields listed in the macro crate's [get_fields_to_add()] method in lib.rs.
/// The match is expected to be matching on a string and there should be a variable named `value` of type `Option<String>` in scope with the value that is to be assigned.
///
/// An argument will be expected when the macro is attached to a method. The argument should be the name of the variable in `self` that the parsed values should be stored in.
/// It is generally expected to be an instance of `<existing struct>AsOptions` from [uci_fields_struct()]. No argument is expected when this attribute is applied to a `match` statement.
#[proc_macro_attribute]
pub fn uci_fields_parser(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    uci_fields_parser_impl(args, input)
}
