use std::{collections::HashSet, fmt::Debug};

use codb_core::IdentPath;

use crate::{db::{registry::{Registry, TTypeId}, relation::Relation, DbRelations}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::Value, TypeError}};

use super::{EvalError, Expression};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
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
            let expression_type_id = expression.eval_types(registry, relations, scopes)?;

            arg.ttype_id().must_eq(&expression_type_id)?;
            
            if !arg_names.insert(arg.name()) {
                return Err(TypeError::FunctionDuplicateArg {
                    arg: arg.name().clone(),
                })
            }
        }

        Ok(ret_type.clone())
    }

    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let function = registry.function(&self.function).ok_or_else(
            || TypeError::FunctionNotFound(self.function.clone()))?;
        let mut args = Vec::new();

        for arg in &self.args {
            args.push(arg.eval(registry, relations, scopes)?);
        }

        function.invoke(registry, relations, args)
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
    use crate::{db::{registry::Registry, relation::memory::MemoryRelation, DbRelations}, expression::{EvalError, Expression}, typesystem::value::{EnumValue, ScalarValue, Value}};

    use super::*;

    #[test]
    fn functions() {
        let relations = DbRelations::<MemoryRelation>::new();
        let registry = Registry::new(&relations);

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

        expr_panic.eval_types(&registry, &relations, &Default::default()).unwrap();
        let panic_err = expr_panic.eval(&registry, &relations, &Default::default()).unwrap_err();

        let EvalError::UserPanic(panic_message) = panic_err else {
            panic!();
        };

        assert_eq!("my_error", panic_message);

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

        expr_panic.eval_types(&registry, &relations, &Default::default()).unwrap();
        let value = expr_pass.eval(&registry, &relations, &Default::default()).unwrap();

        assert_eq!(Value::UNIT, value);
    }
}
