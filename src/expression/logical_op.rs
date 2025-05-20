use std::fmt::Debug;

use crate::{db::registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}, typesystem::{value::{ScalarValue, Value}, TypeError}};

use super::{EvalError, Expression};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum LogicalOp {
    And(Expression, Expression),
    Or(Expression, Expression),
    Not(Expression),
    Eq(Expression, Expression),
    Lt(Expression, Expression),
    Gt(Expression, Expression),
    Lte(Expression, Expression),
    Gte(Expression, Expression),
}

impl Debug for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And(left, right) => write!(f, "({left:?} && {right:?})"),
            Self::Or(left, right) => write!(f, "({left:?} || {right:?})"),
            Self::Not(value) => write!(f, "!({value:?})"),
            Self::Eq(left, right) => write!(f, "({left:?} == {right:?})"),
            Self::Lt(left, right) => write!(f, "({left:?} < {right:?})"),
            Self::Gt(left, right) => write!(f, "({left:?} > {right:?})"),
            Self::Lte(left, right) => write!(f, "({left:?} >= {right:?})"),
            Self::Gte(left, right) => write!(f, "({left:?} <= {right:?})"),
        }
    }
}

impl LogicalOp {
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            LogicalOp::And(left, right) |
            LogicalOp::Or(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                left_type_id.must_eq(&TTypeId::BOOL)?;
                right_type_id.must_eq(&TTypeId::BOOL)?;
                
                Ok(TTypeId::BOOL)
            },
            LogicalOp::Lt(left, right) |
            LogicalOp::Gt(left, right) |
            LogicalOp::Lte(left, right) |
            LogicalOp::Gte(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                left_type_id.must_eq(&TTypeId::BOOL)?;
                right_type_id.must_eq(&TTypeId::BOOL)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Eq(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                right_type_id.must_eq(&left_type_id)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Not(expr) => {
                let expr_type_id = expr.eval_types(registry, scopes)?;

                expr_type_id.must_eq(&TTypeId::BOOL);
                
                Ok(TTypeId::BOOL)
            },
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            LogicalOp::And(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;

                left.ttype_id().must_eq(&TTypeId::BOOL)?;
                right.ttype_id().must_eq(&TTypeId::BOOL)?;
                
                Ok(ScalarValue::Bool(left == Value::TRUE && right == Value::TRUE).into())
            },
            LogicalOp::Or(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::BOOL)?;
                right.ttype_id().must_eq(&TTypeId::BOOL)?;
                
                Ok(ScalarValue::Bool(left == Value::TRUE || right == Value::TRUE).into())
            },
            LogicalOp::Not(expr) => {
                let value = expr.eval(registry, scopes)?;
                
                value.ttype_id().must_eq(&TTypeId::BOOL)?;
                
                Ok(ScalarValue::Bool(value == Value::FALSE).into())
            },
            LogicalOp::Eq(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                right.ttype_id().must_eq(&left.ttype_id());
                
                Ok(ScalarValue::Bool(left == right).into())
            },
            LogicalOp::Lt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left < right).into())
            },
            LogicalOp::Gt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left > right).into())
            },
            LogicalOp::Lte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left <= right).into())
            },
            LogicalOp::Gte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left >= right).into())
            },
        }
    }
}
