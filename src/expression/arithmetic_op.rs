use std::fmt::Debug;

use crate::{db::registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}, typesystem::{value::{ScalarValue, Value}, TypeError}};

use super::{EvalError, Expression};


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ArithmeticOp {
    Add(Expression, Expression),
    Sub(Expression, Expression),
    Mul(Expression, Expression),
    Div(Expression, Expression),
}

impl Debug for ArithmeticOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(left, right) => write!(f, "({left:?} + {right:?})"),
            Self::Sub(left, right) => write!(f, "({left:?} - {right:?})"),
            Self::Mul(left, right) => write!(f, "({left:?} * {right:?})"),
            Self::Div(left, right) => write!(f, "({left:?} / {right:?})"),
        }
    }
}

impl ArithmeticOp {
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let left_ttype_id = left.eval_types(registry, scopes)?;
                let right_ttype_id = right.eval_types(registry, scopes)?;
                
                left_ttype_id.must_eq(&TTypeId::INT32)?;
                right_ttype_id.must_eq(&TTypeId::INT32)?;

                Ok(TTypeId::INT32)
            },
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            ArithmeticOp::Add(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left + right).into())
            },
            ArithmeticOp::Sub(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left - right).into())
            },
            ArithmeticOp::Mul(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left * right).into())
            },
            ArithmeticOp::Div(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left / right).into())
            },
        }
    }
}
