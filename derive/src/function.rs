use {syn, vapabi};
use heck::SnakeCase;
use proc_macro2::TokenStream;
use syn::export::Span;

use super::{
	input_names, template_param_type, rust_type, get_template_names, from_template_param, to_token,
	to_vapabi_param_vec, get_output_kinds, from_token
};

struct TemplateParam {
	/// Template param declaration.
	///
	/// ```text
	/// [T0: Into<Uint>, T1: Into<Bytes>, T2: IntoIterator<Item = U2>, U2 = Into<Uint>]
	/// ```
	declaration: TokenStream,
	/// Template param definition.
	///
	/// ```text
	/// [param0: T0, hello_world: T1, param2: T2]
	/// ```
	definition: TokenStream,
}

struct Inputs {
	/// Collects template params into vector.
	///
	/// ```text
	/// [Token::Uint(param0.into()), Token::Bytes(hello_world.into()), Token::Array(param2.into_iter().map(Into::into).collect())]
	/// ```
	tokenize: Vec<TokenStream>,
	/// Template params.
	template_params: Vec<TemplateParam>,
	/// Quote used to recreate `Vec<vapabi::Param>`
	recreate_quote: TokenStream,
}

struct Outputs {
	/// Decoding implementation.
	implementation: TokenStream,
	/// Decode result.
	result: TokenStream,
	/// Quote used to recreate `Vec<vapabi::Param>`.
	recreate_quote: TokenStream,
}

/// Structure used to generate contract's function interface.
pub struct Function {
	/// Function name.
	name: String,
	/// Function input params.
	inputs: Inputs,
	/// Function output params.
	outputs: Outputs,
	/// Constant function.
	constant: bool,
}

impl<'a> From<&'a vapabi::Function> for Function {
	fn from(f: &'a vapabi::Function) -> Self {
		// [param0, hello_world, param2]
		let input_names = input_names(&f.inputs);

		// [T0: Into<Uint>, T1: Into<Bytes>, T2: IntoIterator<Item = U2>, U2 = Into<Uint>]
		let declarations = f.inputs.iter().enumerate()
			.map(|(index, param)| template_param_type(&param.kind, index));

		// [Uint, Bytes, Vec<Uint>]
		let kinds: Vec<_> = f.inputs
			.iter()
			.map(|param| rust_type(&param.kind))
			.collect();

		// [T0, T1, T2]
		let template_names: Vec<_> = get_template_names(&kinds);

		// [param0: T0, hello_world: T1, param2: T2]
		let definitions = input_names.iter().zip(template_names.iter())
			.map(|(param_name, template_name)| quote! { #param_name: #template_name });

		let template_params = declarations.zip(definitions)
			.map(|(declaration, definition)| TemplateParam { declaration, definition })
			.collect();

		// [Token::Uint(param0.into()), Token::Bytes(hello_world.into()), Token::Array(param2.into_iter().map(Into::into).collect())]
		let tokenize: Vec<_> = input_names.iter().zip(f.inputs.iter())
			.map(|(param_name, param)| to_token(&from_template_param(&param.kind, &param_name), &param.kind))
			.collect();

		let output_result = get_output_kinds(&f.outputs);

		let output_implementation = match f.outputs.len() {
			0 => quote! {
				let _output = output;
				Ok(())
			},
			1 => {
				let o = quote! { out };
				let from_first = from_token(&f.outputs[0].kind, &o);
				quote! {
					let out = self.0.decode_output(output)?.into_iter().next().expect(INTERNAL_ERR);
					Ok(#from_first)
				}
			},
			_ => {
				let o = quote! { out.next().expect(INTERNAL_ERR) };
				let outs: Vec<_> = f.outputs
					.iter()
					.map(|param| from_token(&param.kind, &o))
					.collect();

				quote! {
					let mut out = self.0.decode_output(output)?.into_iter();
					Ok(( #(#outs),* ))
				}
			},
		};

		Function {
			name: f.name.clone(),
			inputs: Inputs {
				tokenize,
				template_params,
				recreate_quote: to_vapabi_param_vec(&f.inputs),
			},
			outputs: Outputs {
				implementation: output_implementation,
				result: output_result,
				recreate_quote: to_vapabi_param_vec(&f.outputs),
			},
			constant: f.constant,
		}
	}
}

impl Function {
	/// Generates the interface for contract's function.
	pub fn generate(&self) -> TokenStream {
		let name = &self.name;
		let module_name = syn::Ident::new(&self.name.to_snake_case(), Span::call_site());
		let tokenize = &self.inputs.tokenize;
		let declarations: &Vec<_> = &self.inputs.template_params.iter().map(|i| &i.declaration).collect();
		let definitions: &Vec<_> = &self.inputs.template_params.iter().map(|i| &i.definition).collect();
		let recreate_inputs = &self.inputs.recreate_quote;
		let recreate_outputs = &self.outputs.recreate_quote;
		let constant = &self.constant;
		let outputs_result = &self.outputs.result;
		let outputs_implementation = &self.outputs.implementation;

		quote! {
			pub mod #module_name {
				use vapabi;
				use super::INTERNAL_ERR;

				fn function() -> vapabi::Function {
					vapabi::Function {
						name: #name.into(),
						inputs: #recreate_inputs,
						outputs: #recreate_outputs,
						constant: #constant,
					}
				}

				/// Generic function output decoder.
				pub struct Decoder(vapabi::Function);

				impl vapabi::FunctionOutputDecoder for Decoder {
					type Output = #outputs_result;

					fn decode(&self, output: &[u8]) -> vapabi::Result<Self::Output> {
						#outputs_implementation
					}
				}

				/// Encodes function input.
				pub fn encode_input<#(#declarations),*>(#(#definitions),*) -> vapabi::Bytes {
					let f = function();
					let tokens = vec![#(#tokenize),*];
					f.encode_input(&tokens).expect(INTERNAL_ERR)
				}

				/// Decodes function output.
				pub fn decode_output(output: &[u8]) -> vapabi::Result<#outputs_result> {
					vapabi::FunctionOutputDecoder::decode(&Decoder(function()), output)
				}

				/// Encodes function output and creates a `Decoder` instance.
				pub fn call<#(#declarations),*>(#(#definitions),*) -> (vapabi::Bytes, Decoder) {
					let f = function();
					let tokens = vec![#(#tokenize),*];
					(f.encode_input(&tokens).expect(INTERNAL_ERR), Decoder(f))
				}
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use vapabi;
	use super::Function;

	#[test]
	fn test_no_params() {
		let vapabi_function = vapabi::Function {
			name: "empty".into(),
			inputs: vec![],
			outputs: vec![],
			constant: false,
		};

		let f = Function::from(&vapabi_function);

		let expected = quote! {
			pub mod empty {
				use vapabi;
				use super::INTERNAL_ERR;

				fn function() -> vapabi::Function {
					vapabi::Function {
						name: "empty".into(),
						inputs: vec![],
						outputs: vec![],
						constant: false,
					}
				}

				/// Generic function output decoder.
				pub struct Decoder(vapabi::Function);

				impl vapabi::FunctionOutputDecoder for Decoder {
					type Output = ();

					fn decode(&self, output: &[u8]) -> vapabi::Result<Self::Output> {
						let _output = output;
						Ok(())
					}
				}

				/// Encodes function input.
				pub fn encode_input<>() -> vapabi::Bytes {
					let f = function();
					let tokens = vec![];
					f.encode_input(&tokens).expect(INTERNAL_ERR)
				}

				/// Decodes function output.
				pub fn decode_output(output: &[u8]) -> vapabi::Result<()> {
					vapabi::FunctionOutputDecoder::decode(&Decoder(function()), output)
				}

				/// Encodes function output and creates a `Decoder` instance.
				pub fn call<>() -> (vapabi::Bytes, Decoder) {
					let f = function();
					let tokens = vec![];
					(f.encode_input(&tokens).expect(INTERNAL_ERR), Decoder(f))
				}
			}
		};

		assert_eq!(expected.to_string(), f.generate().to_string());
	}

	#[test]
	fn test_one_param() {
		let vapabi_function = vapabi::Function {
			name: "hello".into(),
			inputs: vec![
				vapabi::Param {
					name: "foo".into(),
					kind: vapabi::ParamType::Address,
				}
			],
			outputs: vec![
				vapabi::Param {
					name: "bar".into(),
					kind: vapabi::ParamType::Uint(256),
				}
			],
			constant: false,
		};

		let f = Function::from(&vapabi_function);

		let expected = quote! {
			pub mod hello {
				use vapabi;
				use super::INTERNAL_ERR;

				fn function() -> vapabi::Function {
					vapabi::Function {
						name: "hello".into(),
						inputs: vec![vapabi::Param {
							name: "foo".to_owned(),
							kind: vapabi::ParamType::Address
						}],
						outputs: vec![vapabi::Param {
							name: "bar".to_owned(),
							kind: vapabi::ParamType::Uint(256usize)
						}],
						constant: false,
					}
				}

				/// Generic function output decoder.
				pub struct Decoder(vapabi::Function);

				impl vapabi::FunctionOutputDecoder for Decoder {
					type Output = vapabi::Uint;

					fn decode(&self, output: &[u8]) -> vapabi::Result<Self::Output> {
						let out = self.0.decode_output(output)?.into_iter().next().expect(INTERNAL_ERR);
						Ok(out.to_uint().expect(INTERNAL_ERR))
					}
				}

				/// Encodes function input.
				pub fn encode_input<T0: Into<vapabi::Address> >(foo: T0) -> vapabi::Bytes {
					let f = function();
					let tokens = vec![vapabi::Token::Address(foo.into())];
					f.encode_input(&tokens).expect(INTERNAL_ERR)
				}

				/// Decodes function output.
				pub fn decode_output(output: &[u8]) -> vapabi::Result<vapabi::Uint> {
					vapabi::FunctionOutputDecoder::decode(&Decoder(function()), output)
				}

				/// Encodes function output and creates a `Decoder` instance.
				pub fn call<T0: Into<vapabi::Address> >(foo: T0) -> (vapabi::Bytes, Decoder) {
					let f = function();
					let tokens = vec![vapabi::Token::Address(foo.into())];
					(f.encode_input(&tokens).expect(INTERNAL_ERR), Decoder(f))
				}
			}
		};

		assert_eq!(expected.to_string(), f.generate().to_string());
	}

	#[test]
	fn test_multiple_params() {
		let vapabi_function = vapabi::Function {
			name: "multi".into(),
			inputs: vec![
				vapabi::Param {
					name: "foo".into(),
					kind: vapabi::ParamType::FixedArray(Box::new(vapabi::ParamType::Address), 2),
				},
				vapabi::Param {
					name: "bar".into(),
					kind: vapabi::ParamType::Array(Box::new(vapabi::ParamType::Uint(256))),
				}
			],
			outputs: vec![
				vapabi::Param {
					name: "".into(),
					kind: vapabi::ParamType::Uint(256),
				},
				vapabi::Param {
					name: "".into(),
					kind: vapabi::ParamType::String,
				}
			],
			constant: false,
		};

		let f = Function::from(&vapabi_function);

		let expected = quote! {
			pub mod multi {
				use vapabi;
				use super::INTERNAL_ERR;

				fn function() -> vapabi::Function {
					vapabi::Function {
						name: "multi".into(),
						inputs: vec![vapabi::Param {
							name: "foo".to_owned(),
							kind: vapabi::ParamType::FixedArray(Box::new(vapabi::ParamType::Address), 2usize)
						}, vapabi::Param {
							name: "bar".to_owned(),
							kind: vapabi::ParamType::Array(Box::new(vapabi::ParamType::Uint(256usize)))
						}],
						outputs: vec![vapabi::Param {
							name: "".to_owned(),
							kind: vapabi::ParamType::Uint(256usize)
						}, vapabi::Param {
							name: "".to_owned(),
							kind: vapabi::ParamType::String
						}],
						constant: false,
					}
				}

				/// Generic function output decoder.
				pub struct Decoder(vapabi::Function);

				impl vapabi::FunctionOutputDecoder for Decoder {
					type Output = (vapabi::Uint, String);

					fn decode(&self, output: &[u8]) -> vapabi::Result<Self::Output> {
						let mut out = self.0.decode_output(output)?.into_iter();
						Ok((out.next().expect(INTERNAL_ERR).to_uint().expect(INTERNAL_ERR), out.next().expect(INTERNAL_ERR).to_string().expect(INTERNAL_ERR)))
					}
				}

				/// Encodes function input.
				pub fn encode_input<T0: Into<[U0; 2usize]>, U0: Into<vapabi::Address>, T1: IntoIterator<Item = U1>, U1: Into<vapabi::Uint> >(foo: T0, bar: T1) -> vapabi::Bytes {
					let f = function();
					let tokens = vec![{
						let v = (Box::new(foo.into()) as Box<[_]>).into_vec().into_iter().map(Into::into).collect::<Vec<_>>().into_iter().map(|inner| vapabi::Token::Address(inner)).collect();
						vapabi::Token::FixedArray(v)
					}, {
						let v = bar.into_iter().map(Into::into).collect::<Vec<_>>().into_iter().map(|inner| vapabi::Token::Uint(inner)).collect();
						vapabi::Token::Array(v)
					}];
					f.encode_input(&tokens).expect(INTERNAL_ERR)
				}

				/// Decodes function output.
				pub fn decode_output(output: &[u8]) -> vapabi::Result<(vapabi::Uint, String)> {
					vapabi::FunctionOutputDecoder::decode(&Decoder(function()), output)
				}

				/// Encodes function output and creates a `Decoder` instance.
				pub fn call<T0: Into<[U0; 2usize]>, U0: Into<vapabi::Address>, T1: IntoIterator<Item = U1>, U1: Into<vapabi::Uint> >(foo: T0, bar: T1) -> (vapabi::Bytes, Decoder) {
					let f = function();
					let tokens = vec![{
						let v = (Box::new(foo.into()) as Box<[_]>).into_vec().into_iter().map(Into::into).collect::<Vec<_>>().into_iter().map(|inner| vapabi::Token::Address(inner)).collect();
						vapabi::Token::FixedArray(v)
					}, {
						let v = bar.into_iter().map(Into::into).collect::<Vec<_>>().into_iter().map(|inner| vapabi::Token::Uint(inner)).collect();
						vapabi::Token::Array(v)
					}];
					(f.encode_input(&tokens).expect(INTERNAL_ERR), Decoder(f))
				}
			}
		};

		assert_eq!(expected.to_string(), f.generate().to_string());
	}
}
