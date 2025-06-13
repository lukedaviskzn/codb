use std::{collections::HashSet, fmt::Debug, sync::{Arc, Mutex}};

use codb_core::IdentPath;

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::Value, TypeError}};

use super::{EvalError, Expression};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct FunctionInvocation {
    function: IdentPath,
    #[bw(calc = args.len() as u64)]
    args_len: u64,
    #[br(count = args_len)]
    args: Vec<Expression>,
}

impl FunctionInvocation {
    pub fn new(function: IdentPath, args: impl Into<Vec<Expression>>) -> FunctionInvocation {
        FunctionInvocation {
            function,
            args: args.into(),
        }
    }

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
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
            let expression_type_id = expression.eval_types(pager.clone(), registry, relations, scopes)?;

            arg.ttype_id().must_eq(&expression_type_id)?;
            
            if !arg_names.insert(arg.name()) {
                return Err(TypeError::FunctionDuplicateArg {
                    arg: arg.name().clone(),
                });
            }
        }

        Ok(ret_type.clone())
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let function = registry.function(&self.function).expect("function not found");
        let mut args = Vec::new();

        for arg in &self.args {
            args.push(arg.eval(pager.clone(), registry, relations, scopes)?);
        }

        function.invoke(pager, registry, relations, args)
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
    use std::sync::{Arc, Mutex};

    use crate::{db::{registry::Registry, DbRelationSet}, expression::{EnumLiteral, EvalError, Expression, Literal}, typesystem::value::ScalarValue};

    use super::*;

    #[test]
    fn functions() {
        let pager = Arc::new(Mutex::new(Pager::new_memory()));
        let relations = DbRelationSet::new();
        let registry = Registry::new(pager.clone(), &relations);

        let expr_panic = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("unwrap"),
            args: [
                Expression::Literal(
                    EnumLiteral::new(
                        id_path!("Result").into(),
                        id!("Err"),
                        Expression::Literal(ScalarValue::String("my_error".into()).into()),
                    ).into()
                ),
            ].into(),
        });

        expr_panic.eval_types(pager.clone(), &registry, &relations, &Default::default()).unwrap();
        let panic_err = expr_panic.eval(pager.clone(), &registry, &relations, &Default::default()).unwrap_err();

        let EvalError::UserPanic(panic_message) = panic_err;

        assert_eq!("my_error", panic_message);

        let expr_pass = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("unwrap"),
            args: [
                Expression::Literal(
                    EnumLiteral::new(
                        id_path!("Result").into(),
                        id!("Ok"),
                        Expression::Literal(Literal::UNIT),
                    ).into()
                ),
            ].into(),
        });

        expr_panic.eval_types(pager.clone(), &registry, &relations, &Default::default()).unwrap();
        let value = expr_pass.eval(pager.clone(), &registry, &relations, &Default::default()).unwrap();

        assert_eq!(Value::UNIT, value);
    }
}
