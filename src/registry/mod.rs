mod type_registry;
mod function_registry;

pub use type_registry::*;
pub use function_registry::*;

use crate::{expr::{Branch, ControlFlow, Expression, FunctionInvocation, MatchControlFlow}, typesystem::{function::{FunctionArg, UserFunction}, value::{ScalarValue, ScalarValueInner, Value}}};

pub struct Registry {
    types: TypeRegistry,
    functions: FunctionRegistry,
}

impl Registry {
    pub fn new() -> Registry {
        let mut registry = Registry {
            types: TypeRegistry::new(),
            functions: FunctionRegistry::new(),
        };

        let result_ttype_id = registry.types().get_id_by_name("Result").expect("no result type");
        
        let function_unwrap = UserFunction::new(
            &registry,
            vec![
                FunctionArg::new("value".parse().expect("not an ident"), result_ttype_id),
            ],
            TTypeId::UNIT,
            Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
                param: Expression::NestedIdent("value".parse().expect("not an ident")),
                ret_type: TTypeId::UNIT,
                branches: btreemap! {
                    "Ok".parse().expect("not an ident") => Branch::new(
                        "_".parse().expect("unreachable"),
                        Expression::Value(Value::UNIT),
                    ),
                    "Err".parse().expect("not an ident") => Branch::new(
                        "_".parse().expect("unreachable"),
                        Expression::FunctionInvocation(FunctionInvocation::new(
                            "core::panic".parse().expect("not an ident"),
                            [
                                Expression::Value(
                                    ScalarValue::new(registry.types(), TTypeId::STRING, ScalarValueInner::String("".into())).expect("not a string").into(),
                                )
                            ],
                        )),
                    ),
                },
            })))
        ).expect("could not create function `unwrap`");

        registry.functions_mut().add("core::unwrap".parse().expect("not an ident"), function_unwrap).expect("failed to add unwrap function");

        registry
    }

    pub fn types(&self) -> &TypeRegistry {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut TypeRegistry {
        &mut self.types
    }

    pub fn functions(&self) -> &FunctionRegistry {
        &self.functions
    }

    pub fn functions_mut(&mut self) -> &mut FunctionRegistry {
        &mut self.functions
    }
}
