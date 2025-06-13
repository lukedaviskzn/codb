use std::{fmt::Debug, sync::{Arc, Mutex}};

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::{ScalarValue, Value}, TypeError}};

use super::{EvalError, Expression};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum LogicalOp {
    #[brw(magic = 0u8)]
    And(Expression, Expression),
    #[brw(magic = 1u8)]
    Or(Expression, Expression),
    #[brw(magic = 2u8)]
    Not(Expression),
    #[brw(magic = 3u8)]
    Eq(Expression, Expression),
    #[brw(magic = 4u8)]
    Lt(Expression, Expression),
    #[brw(magic = 5u8)]
    Gt(Expression, Expression),
    #[brw(magic = 6u8)]
    Lte(Expression, Expression),
    #[brw(magic = 7u8)]
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
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            LogicalOp::And(left, right) |
            LogicalOp::Or(left, right) => {
                let left_type_id = left.eval_types(pager.clone(), registry, relations, scopes)?;
                let right_type_id = right.eval_types(pager.clone(), registry, relations, scopes)?;

                left_type_id.must_eq(&TTypeId::BOOL)?;
                right_type_id.must_eq(&TTypeId::BOOL)?;
                
                Ok(TTypeId::BOOL)
            },
            LogicalOp::Lt(left, right) |
            LogicalOp::Gt(left, right) |
            LogicalOp::Lte(left, right) |
            LogicalOp::Gte(left, right) => {
                let left_type_id = left.eval_types(pager.clone(), registry, relations, scopes)?;
                let right_type_id = right.eval_types(pager.clone(), registry, relations, scopes)?;

                left_type_id.must_eq(&TTypeId::INT32)?;
                right_type_id.must_eq(&TTypeId::INT32)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Eq(left, right) => {
                let left_type_id = left.eval_types(pager.clone(), registry, relations, scopes)?;
                let right_type_id = right.eval_types(pager.clone(), registry, relations, scopes)?;

                right_type_id.must_eq(&left_type_id)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Not(expr) => {
                let expr_type_id = expr.eval_types(pager.clone(), registry, relations, scopes)?;

                expr_type_id.must_eq(&TTypeId::BOOL)?;
                
                Ok(TTypeId::BOOL)
            },
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            LogicalOp::And(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;

                Ok(ScalarValue::Bool(left == Value::TRUE && right == Value::TRUE).into())
            },
            LogicalOp::Or(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(left == Value::TRUE || right == Value::TRUE).into())
            },
            LogicalOp::Not(expr) => {
                let value = expr.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(value == Value::FALSE).into())
            },
            LogicalOp::Eq(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(left == right).into())
            },
            LogicalOp::Lt(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(left < right).into())
            },
            LogicalOp::Gt(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(left > right).into())
            },
            LogicalOp::Lte(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(left <= right).into())
            },
            LogicalOp::Gte(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                Ok(ScalarValue::Bool(left >= right).into())
            },
        }
    }
}
