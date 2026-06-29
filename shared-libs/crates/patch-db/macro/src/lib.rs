extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(HasModel, attributes(model))]
pub fn model(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    patch_db_macro_internals::build_model(&item).into()
}
