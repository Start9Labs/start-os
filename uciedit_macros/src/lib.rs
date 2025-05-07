use darling::{FromDeriveInput, FromField};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Expr, GenericArgument, Ident, Path, Type,
};

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(uci))]
struct UciSectionOpts {
    ty: Option<String>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum ParseMode {
    FromStr,
    Inpt,
    Bool,
}

use ParseMode::*;

#[derive(Copy, Clone, PartialEq, Eq)]
enum AggMode {
    Optional,
    Single,
    List,
}

use AggMode::*;

#[derive(FromField, Default)]
#[darling(default, attributes(uci))]
struct UciFieldOpts {
    rename: Option<String>,
    default: bool,
    default_value: Option<Expr>,
    inpt: bool,
}

struct UciField {
    placehold: Ident,
    field: Ident,
    name: String,
    default: bool,
    default_value: Option<Expr>,
    agg: AggMode,
    parse: ParseMode,
    crat: Path,
}

impl UciField {
    fn read_decl(&self) -> TokenStream {
        let UciField { placehold, .. } = self;
        match self.agg {
            Optional | Single => quote! { let mut #placehold = None; },
            List => quote! { let mut #placehold = Vec::new(); },
        }
    }

    fn read_option_arm(&self) -> TokenStream {
        if self.agg == List {
            return TokenStream::new();
        }
        let UciField {
            placehold, name, ..
        } = self;
        match self.parse {
            FromStr => quote! {
                #name if #placehold.is_none() => #placehold = Some(value.parse_fromstr(index)?),
            },
            Inpt => quote! {
                #name if #placehold.is_none() => #placehold = Some(value.parse_inpt(index, arena)?),
            },
            Bool => quote! {
                #name if #placehold.is_none() => #placehold = Some(value.parse_bool(index)?),
            },
        }
    }

    fn read_list_arm(&self) -> TokenStream {
        if self.agg != List {
            return TokenStream::new();
        }
        let UciField {
            placehold, name, ..
        } = self;
        match self.parse {
            FromStr => quote! {
                #name => #placehold.push(item.parse_fromstr(index)?),
            },
            Inpt => quote! {
                #name => #placehold.push(item.parse_inpt(index, arena)?),
            },
            Bool => quote! {
                #name => #placehold.push(item.parse_bool(index)?),
            },
        }
    }

    fn read_init(&self) -> TokenStream {
        let UciField {
            placehold,
            field,
            crat,
            name,
            ..
        } = self;
        match (&self.default_value, self.agg, self.default) {
            (Some(value), Optional, _) => {
                quote! { #field: #placehold.or_else(|| #value), }
            }
            (Some(value), List, _) => {
                quote! { #field: if #placehold.is_empty() { #value } else { #placehold }, }
            }
            (Some(value), Single, _) => {
                quote! { #field: #placehold.unwrap_or_else(|| #value), }
            }
            (None, Optional | List, _) => quote! { #field: #placehold, },
            (None, Single, true) => quote! { #field: #placehold.unwrap_or_default(), },
            (None, Single, false) => {
                quote! { #field: #placehold.ok_or(#crat::Error::MissingOption { line_number: start_index, missing: #name.into() })?, }
            }
        }
    }

    fn write_decl(&self) -> TokenStream {
        let UciField {
            placehold,
            field,
            crat,
            name,
            ..
        } = self;
        match (self.agg, self.parse) {
            (Single, Bool) => quote! {
                let mut #placehold = [#crat::Line::option_from_bool(#name, self.#field, arena)].into_iter();
            },
            (Optional, Bool) => quote! {
                let mut #placehold = self.#field.iter().map(|v| #crat::Line::option_from_bool(#name, *v, arena));
            },
            (List, Bool) => quote! {
                let mut #placehold = #crat::Line::list_from_bool(#name, &self.#field, arena);
            },
            (Single, Inpt | FromStr) => quote! {
                let mut #placehold = [#crat::Line::option_from_display(#name, &self.#field, arena)].into_iter();
            },
            (Optional, Inpt | FromStr) => quote! {
                let mut #placehold = self.#field.iter().map(|v| #crat::Line::option_from_display(#name, v, arena));
            },
            (List, Inpt | FromStr) => quote! {
                let mut #placehold = #crat::Line::list_from_display(#name, &self.#field, arena);
            },
        }
    }

    fn write_option_arm(&self) -> TokenStream {
        if self.agg == List {
            return TokenStream::new();
        }
        let UciField {
            placehold, name, ..
        } = self;
        quote! {
            #name => #placehold.next(),
        }
    }

    fn write_list_arm(&self) -> TokenStream {
        if self.agg != List {
            return TokenStream::new();
        }
        let UciField {
            placehold, name, ..
        } = self;
        quote! {
            #name => #placehold.next(),
        }
    }
}

fn chained_write_iters(fields: &[UciField]) -> Option<TokenStream> {
    let mut chained = None;
    for UciField { placehold, .. } in fields.iter().rev() {
        chained = Some(match chained {
            Some(joined) => quote! { #placehold.chain(#joined) },
            None => quote! { #placehold },
        });
    }
    chained
}

fn read_body(fields: &[UciField], struc: Ident, _ty: String, crat: Path) -> TokenStream {
    let decl = fields.iter().map(UciField::read_decl);
    let option_arm = fields.iter().map(UciField::read_option_arm);
    let list_arm = fields.iter().map(UciField::read_list_arm);
    let init = fields.iter().map(UciField::read_init);
    quote! {
        let Some(#crat::Line::Section { .. }) = lines.get(index) else {
            return Err(#crat::Error::ExpectedSection { line_number: index })
        };
        #(#decl)*

        loop {
            index += 1;
            match lines.get(index) {
                Some(#crat::Line::Option { option, value, .. }) => match &*option.as_str() {
                    #(#option_arm)*
                    _ => continue,
                },
                Some(#crat::Line::List { list, item, .. }) => match &*list.as_str() {
                    #(#list_arm)*
                    _ => continue,
                },
                None | Some(#crat::Line::Section { .. }) => break,
                _ => continue,
            }
        }

        Ok(#struc {
            #(#init)*
        })
    }
}

fn write_body(fields: &[UciField], _struc: Ident, ty: String, crat: Path) -> TokenStream {
    let decl = fields.iter().map(UciField::write_decl);
    let option_arm = fields.iter().map(UciField::write_option_arm);
    let list_arm = fields.iter().map(UciField::write_list_arm);
    let chain = chained_write_iters(fields);
    quote! {
        let Some(#crat::Line::Section { ty, .. }) = lines.get(index) else {
            return Err(#crat::Error::ExpectedSection { line_number: index })
        };
        if ty.as_str() != #ty {
            return Err(#crat::Error::ExpectedSectionType { line_number: index, expected: (#ty).into(), found: ty.as_str().into(), })
        }

        #(#decl)*

        let mut insert_after = index;
        loop {
            index += 1;
            let Some(line) = lines.get_mut(index) else {
                break;
            };
            if line.is_in_section() {
                insert_after = index;
            }
            *line = match line {
                #crat::Line::Option { option, .. } => match &*option.as_str() {
                    #(#option_arm)*
                    _ => continue,
                },
                #crat::Line::List { list, .. } => match &*list.as_str() {
                    #(#list_arm)*
                    _ => continue,
                },
                #crat::Line::Section { .. } => break,
                _ => continue,
            }
            .unwrap_or(#crat::Line::Skip);
            insert_after = index;
        }

        lines.splice(
            insert_after+1..insert_after+1,
            #chain,
        );

        Ok(())
    }
}

fn append_body(fields: &[UciField], _struc: Ident, ty: String, crat: Path) -> TokenStream {
    let decl = fields.iter().map(UciField::write_decl);
    let chain = chained_write_iters(fields);
    quote! {
        #(#decl)*

        if !lines.is_empty() {
            lines.push(#crat::Line::Empty);
        }

        lines.push(#crat::Line::section_from(#ty, name, arena));
        lines.extend(#chain);

        Ok(())
    }
}

fn is_collection_with_generic(
    ty: &Type,
    collection: &str,
    check: impl FnOnce(&Type) -> bool,
) -> bool {
    if let Type::Path(path) = ty {
        if let Some(segment) = path.path.segments.first() {
            if segment.ident == collection {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if args.args.len() == 1 {
                        if let GenericArgument::Type(arg) = &args.args[0] {
                            return check(arg);
                        }
                    }
                }
            }
        }
    }
    false
}

fn is_primative(ty: &Type, ident: &str) -> bool {
    if let Type::Path(path) = ty {
        if path.path.segments.len() == 1 {
            if let Some(segment) = path.path.segments.first() {
                if segment.ident == ident {
                    return true;
                }
            }
        }
    }
    false
}

#[proc_macro_derive(UciSection, attributes(uci))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let opts = UciSectionOpts::from_derive_input(&input).expect("Wrong options");

    let crat: Path = parse_quote! { ::uciedit };
    let struc = input.ident;
    let ty = opts.ty.unwrap_or(struc.to_string().to_lowercase());
    let Data::Struct(struct_data) = input.data else {
        panic!("only structs are supported")
    };
    let fields = struct_data
        .fields
        .into_iter()
        .map(|f| {
            let o = UciFieldOpts::from_field(&f)?;
            let i = f.ident.unwrap();
            Ok(UciField {
                placehold: format_ident!("field_{}", i),
                field: i.clone(),
                name: match o.rename {
                    None => i.to_string(),
                    Some(rename) => rename,
                },
                default: o.default,
                default_value: o.default_value,
                agg: if is_collection_with_generic(&f.ty, "Option", |_| true) {
                    Optional
                } else if is_collection_with_generic(&f.ty, "Vec", |_| true) {
                    List
                } else {
                    Single
                },
                parse: if o.inpt {
                    Inpt
                } else if is_primative(&f.ty, "bool")
                    || is_collection_with_generic(&f.ty, "Option", |t| is_primative(t, "bool"))
                    || is_collection_with_generic(&f.ty, "Vec", |t| is_primative(t, "bool"))
                {
                    Bool
                } else {
                    FromStr
                },
                crat: crat.clone(),
            })
        })
        .collect::<Result<Vec<_>, syn::Error>>();
    let fields = match fields {
        Ok(f) => f,
        Err(err) => return err.to_compile_error().into(),
    };

    let read_body = read_body(&fields, struc.clone(), ty.clone(), crat.clone());
    let write_body = write_body(&fields, struc.clone(), ty.clone(), crat.clone());
    let append_body = append_body(&fields, struc.clone(), ty.clone(), crat.clone());

    let (_, type_generics, where_clause) = input.generics.split_for_impl();
    let mut lt_generics = input.generics.clone();
    lt_generics.params.push(parse_quote! { 'a });
    let (impl_generics, _, _) = lt_generics.split_for_impl();

    quote! {
        impl #impl_generics #struc #type_generics #where_clause {
            pub const TY: &'static str = #ty;
        }

        impl #impl_generics #crat::UciSection<'a> for #struc #type_generics #where_clause {
            fn is_type(ty: &str) -> bool {
                ty == #ty
            }

            fn read(
                lines: &#crat::Lines<'a>,
                arena: &'a #crat::Arena,
                mut index: usize,
            ) -> Result<Self, #crat::Error> {
                let start_index = index;
                #read_body
            }

            #[allow(unused_mut)]
            fn write(
                &self,
                lines: &mut #crat::Lines<'a>,
                arena: &'a #crat::Arena,
                mut index: usize,
            ) -> Result<(), #crat::Error> {
                #write_body
            }

            #[allow(unused_mut)]
            fn append(
                &self,
                lines: &mut #crat::Lines<'a>,
                arena: &'a #crat::Arena,
                name: Option<&'a str>,
            ) -> Result<(), #crat::Error> {
                #append_body
            }
        }

    }
    .into_token_stream()
    .into()
}
