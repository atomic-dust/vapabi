#![recursion_limit="256"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate quote;
extern crate heck;
extern crate vapabi;

mod constructor;
mod contract;
mod event;
mod function;

use std::{env, fs};
use std::path::PathBuf;
use heck::SnakeCase;
use syn::export::Span;
use vapabi::{Result, ResultExt, Contract, Param, ParamType};

const ERROR_MSG: &str = "`derive(VapabiContract)` failed";

#[proc_macro_derive(VapabiContract, attributes(vapabi_contract_options))]
pub fn vapabi_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let ast = syn::parse(input).expect(ERROR_MSG);
	let gen = impl_vapabi_derive(&ast).expect(ERROR_MSG);
	gen.into()
}

fn impl_vapabi_derive(ast: &syn::DeriveInput) -> Result<proc_macro2::TokenStream> {
	let options = get_options(&ast.attrs, "vapabi_contract_options")?;
	let path = get_option(&options, "path")?;
	let normalized_path = normalize_path(&path)?;
	let source_file = fs::File::open(&normalized_path)
		.chain_err(|| format!("Cannot load contract abi from `{}`", normalized_path.display()))?;
	let contract = Contract::load(source_file)?;
	let c = contract::Contract::from(&contract);
	Ok(c.generate())
}

fn get_options(attrs: &[syn::Attribute], name: &str) -> Result<Vec<syn::NestedMeta>> {
	let options = attrs.iter()
		.flat_map(syn::Attribute::interpret_meta)
		.find(|meta| meta.name() == name);


	match options {
		Some(syn::Meta::List(list)) => Ok(list.nested.into_iter().collect()),
		_ => Err("Unexpected meta item".into())
	}
}

fn get_option(options: &[syn::NestedMeta], name: &str) -> Result<String> {
	let item = options.iter()
		.flat_map(|nested| match *nested {
			syn::NestedMeta::Meta(ref meta) => Some(meta),
			_ => None,
		})
		.find(|meta| meta.name() == name)
		.chain_err(|| format!("Expected to find option {}", name))?;
	str_value_of_meta_item(item, name)
}

fn str_value_of_meta_item(item: &syn::Meta, name: &str) -> Result<String> {
	if let syn::Meta::NameValue(ref name_value) = *item {
		if let syn::Lit::Str(ref value) = name_value.lit {
			return Ok(value.value());
		}
	}

	Err(format!(r#"`{}` must be in the form `#[{}="something"]`"#, name, name).into())
}

fn normalize_path(relative_path: &str) -> Result<PathBuf> {
	// workaround for https://github.com/rust-lang/rust/issues/43860
	let cargo_toml_directory = env::var("CARGO_MANIFEST_DIR").chain_err(|| "Cannot find manifest file")?;
	let mut path: PathBuf = cargo_toml_directory.into();
	path.push(relative_path);
	Ok(path)
}

fn to_syntax_string(param_type: &vapabi::ParamType) -> proc_macro2::TokenStream {
	match *param_type {
		ParamType::Address => quote! { vapabi::ParamType::Address },
		ParamType::Bytes => quote! { vapabi::ParamType::Bytes },
		ParamType::Int(x) => quote! { vapabi::ParamType::Int(#x) },
		ParamType::Uint(x) => quote! { vapabi::ParamType::Uint(#x) },
		ParamType::Bool => quote! { vapabi::ParamType::Bool },
		ParamType::String => quote! { vapabi::ParamType::String },
		ParamType::Array(ref param_type) => {
			let param_type_quote = to_syntax_string(param_type);
			quote! { vapabi::ParamType::Array(Box::new(#param_type_quote)) }
		},
		ParamType::FixedBytes(x) => quote! { vapabi::ParamType::FixedBytes(#x) },
		ParamType::FixedArray(ref param_type, ref x) => {
			let param_type_quote = to_syntax_string(param_type);
			quote! { vapabi::ParamType::FixedArray(Box::new(#param_type_quote), #x) }
		}
	}
}

fn to_vapabi_param_vec<'a, P: 'a>(params: P) -> proc_macro2::TokenStream
	where P: IntoIterator<Item = &'a Param>
{
	let p = params.into_iter().map(|x| {
		let name = &x.name;
		let kind = to_syntax_string(&x.kind);
		quote! {
			vapabi::Param {
				name: #name.to_owned(),
				kind: #kind
			}
		}
	}).collect::<Vec<_>>();

	quote! { vec![ #(#p),* ] }
}

fn rust_type(input: &ParamType) -> proc_macro2::TokenStream {
	match *input {
		ParamType::Address => quote! { vapabi::Address },
		ParamType::Bytes => quote! { vapabi::Bytes },
		ParamType::FixedBytes(32) => quote! { vapabi::Hash },
		ParamType::FixedBytes(size) => quote! { [u8; #size] },
		ParamType::Int(_) => quote! { vapabi::Int },
		ParamType::Uint(_) => quote! { vapabi::Uint },
		ParamType::Bool => quote! { bool },
		ParamType::String => quote! { String },
		ParamType::Array(ref kind) => {
			let t = rust_type(&*kind);
			quote! { Vec<#t> }
		},
		ParamType::FixedArray(ref kind, size) => {
			let t = rust_type(&*kind);
			quote! { [#t, #size] }
		}
	}
}

fn template_param_type(input: &ParamType, index: usize) -> proc_macro2::TokenStream {
	let t_ident = syn::Ident::new(&format!("T{}", index), Span::call_site());
	let u_ident = syn::Ident::new(&format!("U{}", index), Span::call_site());
	match *input {
		ParamType::Address => quote! { #t_ident: Into<vapabi::Address> },
		ParamType::Bytes => quote! { #t_ident: Into<vapabi::Bytes> },
		ParamType::FixedBytes(32) => quote! { #t_ident: Into<vapabi::Hash> },
		ParamType::FixedBytes(size) => quote! { #t_ident: Into<[u8; #size]> },
		ParamType::Int(_) => quote! { #t_ident: Into<vapabi::Int> },
		ParamType::Uint(_) => quote! { #t_ident: Into<vapabi::Uint> },
		ParamType::Bool => quote! { #t_ident: Into<bool> },
		ParamType::String => quote! { #t_ident: Into<String> },
		ParamType::Array(ref kind) => {
			let t = rust_type(&*kind);
			quote! {
				#t_ident: IntoIterator<Item = #u_ident>, #u_ident: Into<#t>
			}
		},
		ParamType::FixedArray(ref kind, size) => {
			let t = rust_type(&*kind);
			quote! {
				#t_ident: Into<[#u_ident; #size]>, #u_ident: Into<#t>
			}
		}
	}
}

fn from_template_param(input: &ParamType, name: &syn::Ident) -> proc_macro2::TokenStream {
	match *input {
		ParamType::Array(_) => quote! { #name.into_iter().map(Into::into).collect::<Vec<_>>() },
		ParamType::FixedArray(_, _) => quote! { (Box::new(#name.into()) as Box<[_]>).into_vec().into_iter().map(Into::into).collect::<Vec<_>>() },
		_ => quote! {#name.into() },
	}
}

fn to_token(name: &proc_macro2::TokenStream, kind: &ParamType) -> proc_macro2::TokenStream {
	match *kind {
		ParamType::Address => quote! { vapabi::Token::Address(#name) },
		ParamType::Bytes => quote! { vapabi::Token::Bytes(#name) },
		ParamType::FixedBytes(_) => quote! { vapabi::Token::FixedBytes(#name.as_ref().to_vec()) },
		ParamType::Int(_) => quote! { vapabi::Token::Int(#name) },
		ParamType::Uint(_) => quote! { vapabi::Token::Uint(#name) },
		ParamType::Bool => quote! { vapabi::Token::Bool(#name) },
		ParamType::String => quote! { vapabi::Token::String(#name) },
		ParamType::Array(ref kind) => {
			let inner_name = quote! { inner };
			let inner_loop = to_token(&inner_name, kind);
			quote! {
				// note the double {{
				{
					let v = #name.into_iter().map(|#inner_name| #inner_loop).collect();
					vapabi::Token::Array(v)
				}
			}
		}
		ParamType::FixedArray(ref kind, _) => {
			let inner_name = quote! { inner };
			let inner_loop = to_token(&inner_name, kind);
			quote! {
				// note the double {{
				{
					let v = #name.into_iter().map(|#inner_name| #inner_loop).collect();
					vapabi::Token::FixedArray(v)
				}
			}
		},
	}
}

fn from_token(kind: &ParamType, token: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
	match *kind {
		ParamType::Address => quote! { #token.to_address().expect(INTERNAL_ERR) },
		ParamType::Bytes => quote! { #token.to_bytes().expect(INTERNAL_ERR) },
		ParamType::FixedBytes(32) => quote! {
			{
				let mut result = [0u8; 32];
				let v = #token.to_fixed_bytes().expect(INTERNAL_ERR);
				result.copy_from_slice(&v);
				vapabi::Hash::from(result)
			}
		},
		ParamType::FixedBytes(size) => {
			let size: syn::Index = size.into();
			quote! {
				{
					let mut result = [0u8; #size];
					let v = #token.to_fixed_bytes().expect(INTERNAL_ERR);
					result.copy_from_slice(&v);
					result
				}
			}
		},
		ParamType::Int(_) => quote! { #token.to_int().expect(INTERNAL_ERR) },
		ParamType::Uint(_) => quote! { #token.to_uint().expect(INTERNAL_ERR) },
		ParamType::Bool => quote! { #token.to_bool().expect(INTERNAL_ERR) },
		ParamType::String => quote! { #token.to_string().expect(INTERNAL_ERR) },
		ParamType::Array(ref kind) => {
			let inner = quote! { inner };
			let inner_loop = from_token(kind, &inner);
			quote! {
				#token.to_array().expect(INTERNAL_ERR).into_iter()
					.map(|#inner| #inner_loop)
					.collect()
			}
		},
		ParamType::FixedArray(ref kind, size) => {
			let inner = quote! { inner };
			let inner_loop = from_token(kind, &inner);
			let to_array = vec![quote! { iter.next() }; size];
			quote! {
				{
					let iter = #token.to_array().expect(INTERNAL_ERR).into_iter()
						.map(|#inner| #inner_loop);
					[#(#to_array),*]
				}
			}
		},
	}
}

fn input_names(inputs: &[Param]) -> Vec<syn::Ident> {
	inputs
		.iter()
		.enumerate()
		.map(|(index, param)| if param.name.is_empty() {
			syn::Ident::new(&format!("param{}", index), Span::call_site())
		} else {
			syn::Ident::new(&rust_variable(&param.name), Span::call_site())
		})
		.collect()
}

fn get_template_names(kinds: &[proc_macro2::TokenStream]) -> Vec<syn::Ident> {
	kinds.iter().enumerate()
		.map(|(index, _)| syn::Ident::new(&format!("T{}", index), Span::call_site()))
		.collect()
}

fn get_output_kinds(outputs: &[Param]) -> proc_macro2::TokenStream {
	match outputs.len() {
		0 => quote! {()},
		1 => {
			let t = rust_type(&outputs[0].kind);
			quote! { #t }
		},
		_ => {
			let outs: Vec<_> = outputs
				.iter()
				.map(|param| rust_type(&param.kind))
				.collect();
			quote! { (#(#outs),*) }
		}
	}
}

/// Convert input into a rust variable name.
///
/// Avoid using keywords by escaping them.
fn rust_variable(name: &str) -> String {
	// avoid keyword parameters
	match name {
		"self" => "_self".to_string(),
		other => other.to_snake_case(),
	}
}
