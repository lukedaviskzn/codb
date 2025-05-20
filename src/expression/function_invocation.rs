use std::{collections::HashSet, fmt::Debug};

use codb_core::IdentPath;

use crate::{db::registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}, typesystem::{function::{FunctionEntry, InterpreterFunctionAction}, value::{ScalarValue, Value}, TypeError}};

use super::{EvalError, Expression};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct FunctionInvocation {
    function: IdentPath,
    args: Box<[Expression]>,
}

impl FunctionInvocation {
    pub fn new(function: IdentPath, args: impl Into<Box<[Expression]>>) -> FunctionInvocation {
        FunctionInvocation {
            function,
            args: args.into(),
        }
    }

    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let function = registry.function(&self.function)
            .ok_or_else(|| TypeError::FunctionNotFound(self.function.clone()))?;

        let ret_type = function.result_type();

        if function.args().len() != self.args.len() {
            return Err(TypeError::FunctionArgLen {
                expected: function.args().len(),
                got: self.args.len(),
            });
        }

        let mut arg_names = HashSet::new();

        for (arg, expression) in function.args().iter().zip(self.args.iter()) {
            let expression_type_id = expression.eval_types(registry, scopes)?;

            arg.ttype_id().must_eq(&expression_type_id)?;
            
            if !arg_names.insert(arg.name()) {
                return Err(TypeError::FunctionDuplicateArg {
                    arg: arg.name().clone(),
                })
            }
        }

        Ok(ret_type.clone())
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let function = registry.function(&self.function).ok_or_else(
            || TypeError::FunctionNotFound(self.function.clone()))?;
        let mut args = Vec::new();

        for arg in &self.args {
            args.push(arg.eval(registry, scopes)?);
        }

        match function {
            FunctionEntry::UserFunction(function) => Ok(function.invoke(registry, args)?),
            FunctionEntry::InterpreterFunction(function) => match function.action() {
                InterpreterFunctionAction::Panic => {
                    let message = args.get(0).map(|value| {
                        match value {
                            Value::Scalar(scalar_value) => if let ScalarValue::String(string) = scalar_value {
                                string.clone()
                            } else {
                                Default::default()
                            },
                            _ => Default::default(),
                        }
                    }).unwrap_or_default();

                    Err(EvalError::Panic(message))
                },
            },
        }
    }
}

impl Debug for FunctionInvocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_tuple("");
        for arg in &self.args {
            d.field(arg);
        }
        d.finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{db::registry::Registry, expression::{EvalError, Expression}, typesystem::value::{EnumValue, ScalarValue, Value}};

    use super::*;

    #[test]
    fn functions() {
        let registry = Registry::new();

        let expr_panic = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("unwrap"),
            args: [
                Expression::Value(
                    EnumValue::new(
                        &registry,
                        id_path!("Result").into(),
                        id!("Err"),
                        ScalarValue::String("my_error".into()).into(),
                    ).unwrap().into()
                ),
            ].into(),
        });

        expr_panic.eval_types(&registry, &Default::default()).unwrap();
        let panic_err = expr_panic.eval(&registry, &Default::default()).unwrap_err();

        assert_eq!(EvalError::Panic("my_error".into()), panic_err);

        let expr_pass = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("unwrap"),
            args: [
                Expression::Value(
                    EnumValue::new(
                        &registry,
                        id_path!("Result").into(),
                        id!("Ok"),
                        ScalarValue::Unit.into(),
                    ).unwrap().into()
                ),
            ].into(),
        });

        expr_panic.eval_types(&registry, &Default::default()).unwrap();
        let value = expr_pass.eval(&registry, &Default::default()).unwrap();

        assert_eq!(Value::UNIT, value);
    }
}
