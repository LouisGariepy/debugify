use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    ConstParam, GenericParam, LifetimeParam, TypeParam,
};

/// # debugify
///
/// Derive macro for `std::fmt::Debug` focused on reducing boilerplate. Supports both format strings and formatter functions.
///
/// ## Formats
///
/// Formats can be specified either as a format string or as the path to a formatter function.
/// A formatter function must adhere to the following signature: `fn(&T) -> String`.
///
/// ## Attributes
///
/// In case of a conflict between attributes, the order of precedence is
///
/// 1. field attribute
/// 2. field name
/// 3. field type
///
/// If no format is specified, the default format is used.
///
/// ### Item attributes
///
/// These attributes are applied to a struct or enum.
///
/// #### `field_name`
///
/// Applies to the formatting of all fields with the given names inside the item.
///
/// ```rust
/// use debugify::Debugify;
///
/// #[derive(Debugify)]
/// #[debugify(field_name(
///     [bar, biz] = "foobar{:?}",
///     baz = "foobaz{:?}",
/// ))]
/// struct Foo {
///     bar: i32,
///     baz: String,
///     biz: &'static str,
///     qux: i64,
///
/// }
///
/// let foo = Foo {
///     bar: 123,
///     baz: "hello".to_string(),
///     biz: "world",
///     qux: 456,
/// };
///
/// let foo_debug = format!("{:?}", foo);
/// assert_eq!(foo_debug, "Foo { bar: foobar123, baz: foobaz\"hello\", biz: foobar\"world\", qux: 456 }");
/// ```
///
/// #### `field_type`
///
/// Applies to the formatting of all fields with the given types inside the item.
///
/// ```rust
/// use debugify::Debugify;
///
/// #[derive(Debugify)]
/// #[debugify(field_type(
///     [i32, &'static str] = "foobar{:?}",
///     String = "foobaz{:?}",
/// ))]
/// struct Foo {
///     bar: i32,
///     baz: String,
///     biz: &'static str,
///     qux: i64,
/// }
///
/// let foo = Foo {
///     bar: 123,
///     baz: "hello".to_string(),
///     biz: "world",
///     qux: 456,
/// };
///
/// let foo_debug = format!("{:?}", foo);
/// assert_eq!(foo_debug, "Foo { bar: foobar123, baz: foobaz\"hello\", biz: foobar\"world\", qux: 456 }");
/// ```
///
/// ### Field attributes
///
/// Currently the only field attribute support is a format specifier.
///
/// ```rust
/// use debugify::Debugify;
///
/// #[derive(Debugify)]
/// #[debugify(field_name(bar = "foo{:?}"))]
/// struct Foo {
///     #[debugify("bar{:?}")]
///     bar: i32,
///     baz: String,
/// }
///
/// let foo = Foo {
///     bar: 123,
///     baz: "hello".to_string(),
/// };
///
/// let foo_debug = format!("{:?}", foo);
/// assert_eq!(foo_debug, "Foo { bar: bar123, baz: \"hello\" }");
/// ```
///
/// Field attributes take precedence over item attributes.
///
/// ## Enums
///
/// Enums are supported as well. Item attributes are apply to all
/// variants, and each variant is treated essentially as a struct.
///
/// ```rust
/// use debugify::Debugify;
///
/// #[derive(Debugify)]
/// #[debugify(field_name([biz, qux] = "foo{:?}"))]
/// enum Foo {
///     Bar {
///         biz: i32,
///         qux: String,
///     },
///     Baz {
///         biz: i32,
///         #[debugify("qux{:?}")]
///         qux: String,
///         quant: i64,
///     }
/// }
///
/// let foo_1 = Foo::Bar {
///     biz: 123,
///     qux: "hello".to_string(),
/// };
/// let foo_2 = Foo::Baz {
///     biz: 456,
///     qux: "world".to_string(),
///     quant: 789,
/// };
///
/// let foo_1_debug = format!("{:?}", foo_1);
/// assert_eq!(foo_1_debug, "Bar { biz: foo123, qux: foo\"hello\" }");
///
/// let foo_2_debug = format!("{:?}", foo_2);
/// assert_eq!(foo_2_debug, "Baz { biz: foo456, qux: qux\"world\", quant: 789 }");
/// ```
///
/// Tuple and unit variants are essentially ignored by `debugify`. They will be debug-printed as they normally would.

#[proc_macro_derive(Debugify, attributes(debugify))]
pub fn debugify(tokens: TokenStream) -> TokenStream {
    let item = syn::parse_macro_input!(tokens as syn::Item);
    match item {
        syn::Item::Enum(item) => debugify_enum(item),
        syn::Item::Struct(item) => debugify_struct(item),
        _ => syn::Error::new_spanned(item, "expected enum or struct")
            .to_compile_error()
            .into(),
    }
}

fn debugify_enum(item: syn::ItemEnum) -> TokenStream {
    let item_ident = item.ident;

    // Parse item attributes into rule maps
    let rules = aggregate_format_rules(&item.attrs);

    // Early-return if there was an error while parsing the item attributes
    let (field_name, field_type) = match rules {
        Ok(rules) => rules,
        Err(e) => return e.to_compile_error().into(),
    };

    // Parse generic parameters
    let generics = &item.generics;
    let generic_parameters = generic_params(generics);

    // Parse variants
    let variants = item
        .variants
        .into_iter()
        .map(|variant| {
            let variant_ident = variant.ident;
            Ok(match variant.fields {
                syn::Fields::Named(_) => {
                    let field_names = variant
                        .fields
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap())
                        .collect::<Vec<_>>();
                    let fields =
                        field_attributes(&variant.fields, &field_name, &field_type, false)?;
                    quote! {
                        #item_ident::#variant_ident { #(#field_names),* } => {
                            f.debug_struct(
                                stringify!(#variant_ident)
                            )
                            #(#fields)*
                            .finish()
                        }
                    }
                }
                syn::Fields::Unnamed(_) => {
                    let field_names = variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(field_nb, _)| quote::format_ident!("field_{field_nb}"))
                        .collect::<Vec<_>>();
                    let fields = field_names.iter().map(|field_name| {
                        quote! {
                            .field(
                                #field_name
                            )
                        }
                    });
                    quote! {
                        #item_ident::#variant_ident ( #(#field_names),* ) => {
                            f.debug_tuple(
                                stringify!(#variant_ident)
                            )
                            #(#fields)*
                            .finish()
                        }
                    }
                }
                syn::Fields::Unit => {
                    quote! {
                        #item_ident::#variant_ident => {
                            f.debug_struct(
                                stringify!(#variant_ident)
                            )
                            .finish()
                        }
                    }
                }
            })
        })
        .collect::<syn::Result<Vec<_>>>();

    // Early-return if there was an error parsing the item attributes
    let variants = match variants {
        Ok(variants) => variants,
        Err(e) => return e.to_compile_error().into(),
    };

    // Generate the debug impl
    quote! {
        impl #generics std::fmt::Debug for #item_ident <#(#generic_parameters),*> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(#variants)*
                }
            }
        }
    }
    .into()
}

fn debugify_struct(item: syn::ItemStruct) -> TokenStream {
    let item_ident = item.ident;

    // Parse item attributes to rules
    let rules = aggregate_format_rules(&item.attrs);

    // Early-return if there was an error parsing the item attributes
    let (field_name, field_type) = match rules {
        Ok(rules) => rules,
        Err(e) => return e.to_compile_error().into(),
    };

    // Parse generic parameters
    let generics = &item.generics;
    let generic_parameters = generic_params(generics);

    // Parse field attributes
    let fields = field_attributes(&item.fields, &field_name, &field_type, true);

    // Early-return if there was an error parsing the field attributes
    let fields = match fields {
        Ok(fields) => fields,
        Err(e) => return e.to_compile_error().into(),
    };

    // Generate the debug impl
    quote! {
        impl #generics std::fmt::Debug for #item_ident <#(#generic_parameters),*> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(
                    stringify!(#item_ident)
                )
                #(#fields)*
                .finish()
            }
        }
    }
    .into()
}

pub(crate) struct ItemAttributeMetaItem<T: Parse> {
    pub(crate) values: ItemAttributeValues<T>,
    pub(crate) _eq: syn::token::Eq,
    pub(crate) format: Format,
}

impl<T: Parse> Parse for ItemAttributeMetaItem<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            values: input.parse()?,
            _eq: input.parse()?,
            format: input.parse()?,
        })
    }
}

pub(crate) enum ItemAttributeValues<T: Parse> {
    Single(ItemAttributeIdentsSingle<T>),
    Multiple(ItemAttributeIdentsMultiple<T>),
}

impl<T: Parse> Parse for ItemAttributeValues<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            input
                .parse::<ItemAttributeIdentsSingle<T>>()
                .map(ItemAttributeValues::Single)
        } else if lookahead.peek(syn::token::Bracket) {
            input
                .parse::<ItemAttributeIdentsMultiple<T>>()
                .map(ItemAttributeValues::Multiple)
        } else {
            Err(lookahead.error())
        }
    }
}

pub(crate) struct ItemAttributeIdentsSingle<T: Parse> {
    pub(crate) value: T,
}

impl<T: Parse> Parse for ItemAttributeIdentsSingle<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            value: input.parse()?,
        })
    }
}

pub(crate) struct ItemAttributeIdentsMultiple<T: Parse> {
    pub(crate) _bracket: syn::token::Bracket,
    pub(crate) values: syn::punctuated::Punctuated<T, syn::Token![,]>,
}

impl<T: Parse> Parse for ItemAttributeIdentsMultiple<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            _bracket: syn::bracketed!(content in input),
            values: content.parse_terminated(T::parse, syn::Token![,])?,
        })
    }
}

fn aggregate_format_rules(
    attrs: &[syn::Attribute],
) -> syn::Result<(HashMap<syn::Ident, Format>, HashMap<syn::Type, Format>)> {
    // Build maps out of attributes
    attrs.iter().try_fold(
        (
            HashMap::<syn::Ident, Format>::new(),
            HashMap::<syn::Type, Format>::new(),
        ),
        |(mut field_names, mut field_types), attr| {
            // If the attribute's path is not "debugify", don't aggregate it
            if !attr.path().is_ident("debugify") {
                return syn::Result::Ok((field_names, field_types));
            };

            // Parse the attribute's content and append the rules to the maps
            append_item_format_rules(attr, &mut field_names, &mut field_types)?;

            Ok((field_names, field_types))
        },
    )
}

/// Parses the content of an item attribute and appends the rules to the maps
fn append_item_format_rules(
    attr: &syn::Attribute,
    field_name: &mut HashMap<syn::Ident, Format>,
    field_type: &mut HashMap<syn::Type, Format>,
) -> syn::Result<()> {
    attr.parse_nested_meta(|meta| {
        let content;
        syn::parenthesized!(content in meta.input);
        if meta.path.is_ident("field_name") {
            insert_rules(content, field_name)?;
        } else if meta.path.is_ident("field_type") {
            insert_rules(content, field_type)?;
        } else {
            return Err(syn::Error::new_spanned(
                meta.path,
                "expected `field_name` or `field_type`",
            ));
        };
        Ok(())
    })
}

/// Parses a comma separated list of rules and inserts them into the map
fn insert_rules<T: syn::parse::Parse + std::hash::Hash + Eq>(
    content: syn::parse::ParseBuffer,
    rules: &mut HashMap<T, Format>,
) -> syn::Result<()> {
    let meta_items = content.parse_terminated(ItemAttributeMetaItem::<T>::parse, syn::Token![,])?;
    for meta_item in meta_items {
        match meta_item.values {
            ItemAttributeValues::Single(ty) => {
                rules.insert(ty.value, meta_item.format);
            }
            ItemAttributeValues::Multiple(tys) => {
                for ty in tys.values {
                    rules.insert(ty, meta_item.format.clone());
                }
            }
        }
    }
    Ok(())
}

/// Generates the generic parameters for the debug impl
fn generic_params(generics: &syn::Generics) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    generics.params.iter().map(|param| match param {
        GenericParam::Lifetime(LifetimeParam { lifetime, .. }) => quote! {#lifetime},
        GenericParam::Type(TypeParam { ident, .. }) => quote! {#ident},
        GenericParam::Const(ConstParam { ident, .. }) => quote! {#ident},
    })
}

/// Generates the debug fields for the debug impl
fn field_attributes(
    fields: &syn::Fields,
    field_name: &HashMap<syn::Ident, Format>,
    field_type: &HashMap<syn::Type, Format>,
    has_self: bool,
) -> Result<Vec<proc_macro2::TokenStream>, syn::Error> {
    fields
        .iter()
        .map(|field| {
            let Some(field_ident) = field.ident.as_ref() else {
                return Err(syn::Error::new_spanned(field, "expected named field"));
            };

            // Get format from
            //     1. last attribute, or
            //     2. field name rule, or
            //     3. field type rule, or
            //     4. default format
            let format = field
                .attrs
                .iter()
                .rev()
                .find(|attr| attr.path().is_ident("debugify"))
                .map(|attr| attr.parse_args::<Format>())
                .transpose()?
                .or_else(|| field_name.get(field_ident).cloned())
                .or_else(|| field_type.get(&field.ty).cloned());

            Ok(debug_field(field_ident, format, has_self))
        })
        .collect::<syn::Result<Vec<_>>>()
}

/// Generates the debug field call for a field
fn debug_field(
    field_ident: &syn::Ident,
    format: Option<Format>,
    has_self: bool,
) -> proc_macro2::TokenStream {
    let value = if has_self {
        quote! { &self.#field_ident }
    } else {
        quote! { #field_ident }
    };

    let value = match format {
        Some(Format::Function(format)) => quote! {
            &std::format_args!("{}", #format(#value))
        },
        Some(Format::String(format)) => quote! {
            &std::format_args!(#format, #value)
        },
        None => quote! { #value },
    };

    quote! {
        .field(
            stringify!(#field_ident),
            #value
        )
    }
}

#[derive(Clone)]
enum Format {
    String(syn::LitStr),
    Function(syn::Path),
}

impl syn::parse::Parse for Format {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitStr) {
            input.parse::<syn::LitStr>().map(Format::String)
        } else if lookahead.peek(syn::Ident) {
            input.parse::<syn::Path>().map(Format::Function)
        } else {
            Err(lookahead.error())
        }
    }
}
