use vapabi;
use proc_macro2::TokenStream;
use constructor::Constructor;
use function::Function;
use event::Event;

/// Structure used to generate rust interface for solidity contract.
pub struct Contract {
	constructor: Option<Constructor>,
	functions: Vec<Function>,
	events: Vec<Event>,
}

impl<'a> From<&'a vapabi::Contract> for Contract {
	fn from(c: &'a vapabi::Contract) -> Self {
		Contract {
			constructor: c.constructor.as_ref().map(Into::into),
			functions: c.functions().map(Into::into).collect(),
			events: c.events().map(Into::into).collect(),
		}
	}
}

impl Contract {
	/// Generates rust interface for a contract.
	pub fn generate(&self) -> TokenStream {
		let constructor = self.constructor.as_ref().map(Constructor::generate);
		let functions: Vec<_> = self.functions.iter().map(Function::generate).collect();
		let events: Vec<_> = self.events.iter().map(Event::generate_event).collect();
		let logs: Vec<_> = self.events.iter().map(Event::generate_log).collect();
		quote! {
			use vapabi;
			const INTERNAL_ERR: &'static str = "`vapabi_derive` internal error";

			#constructor

			/// Contract's functions.
			pub mod functions {
				use super::INTERNAL_ERR;
				#(#functions)*
			}

			/// Contract's events.
			pub mod events {
				use super::INTERNAL_ERR;
				#(#events)*
			}

			/// Contract's logs.
			pub mod logs {
				use super::INTERNAL_ERR;
				use vapabi;
				#(#logs)*
			}
		}
	}
}

#[cfg(test)]
mod test {
	use vapabi;
	use super::Contract;

	#[test]
	fn test_no_body() {
		let vapabi_contract = vapabi::Contract {
			constructor: None,
			functions: Default::default(),
			events: Default::default(),
			fallback: false,
		};

		let c = Contract::from(&vapabi_contract);

		let expected = quote! {
			use vapabi;
			const INTERNAL_ERR: &'static str = "`vapabi_derive` internal error";

			/// Contract's functions.
			pub mod functions {
				use super::INTERNAL_ERR;
			}

			/// Contract's events.
			pub mod events {
				use super::INTERNAL_ERR;
			}

			/// Contract's logs.
			pub mod logs {
				use super::INTERNAL_ERR;
				use vapabi;
			}
		};

		assert_eq!(expected.to_string(), c.generate().to_string());
	}
}
