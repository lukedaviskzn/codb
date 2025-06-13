use std::{fmt::Debug, sync::{Arc, Mutex}};

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::{ScalarValue, Value}, TypeError}};

use super::{EvalError, Expression};

// todo: negation operator
#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ArithmeticOp {
    #[brw(magic = 0u8)]
    Add(Expression, Expression),
    #[brw(magic = 1u8)]
    Sub(Expression, Expression),
    #[brw(magic = 2u8)]
    Mul(Expression, Expression),
    #[brw(magic = 3u8)]
    Div(Expression, Expression),
    #[brw(magic = 4u8)]
    Neg(Expression),
}

impl Debug for ArithmeticOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(left, right) => write!(f, "({left:?} + {right:?})"),
            Self::Sub(left, right) => write!(f, "({left:?} - {right:?})"),
            Self::Mul(left, right) => write!(f, "({left:?} * {right:?})"),
            Self::Div(left, right) => write!(f, "({left:?} / {right:?})"),
            Self::Neg(expr) => write!(f, "-({expr:?})"),
        }
    }
}

impl ArithmeticOp {
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let left_ttype_id = left.eval_types(pager.clone(), registry, relations, scopes)?;
                let right_ttype_id = right.eval_types(pager, registry, relations, scopes)?;
                
                left_ttype_id.must_eq(&TTypeId::INT32)?;
                right_ttype_id.must_eq(&TTypeId::INT32)?;

                Ok(TTypeId::INT32)
            },
            ArithmeticOp::Neg(expr) => {
                let expr_ttype_id = expr.eval_types(pager.clone(), registry, relations, scopes)?;
                
                expr_ttype_id.must_eq(&TTypeId::INT32)?;

                Ok(TTypeId::INT32)
            },
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let (left, right) = match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let left = left.eval(pager.clone(), registry, relations, scopes)?;
                let right = right.eval(pager.clone(), registry, relations, scopes)?;
                
                let Value::Scalar(ScalarValue::Int32(left)) = left else {
                    panic!("left value is not an int32")
                };
                let Value::Scalar(ScalarValue::Int32(right)) = right else {
                    panic!("right value is not an int32")
                };

                (left, right)
            },
            ArithmeticOp::Neg(expr) => {
                let expr = expr.eval(pager.clone(), registry, relations, scopes)?;
                
                let Value::Scalar(ScalarValue::Int32(expr)) = expr else {
                    panic!("expr value is not an int32")
                };

                (expr, 0)
            },
        };
        
        match self {
            ArithmeticOp::Add(_, _) => Ok(ScalarValue::Int32(left + right).into()),
            ArithmeticOp::Sub(_, _) => Ok(ScalarValue::Int32(left - right).into()),
            ArithmeticOp::Mul(_, _) => Ok(ScalarValue::Int32(left * right).into()),
            ArithmeticOp::Div(_, _) => Ok(ScalarValue::Int32(left / right).into()),
            ArithmeticOp::Neg(_) => Ok(ScalarValue::Int32((-left).into()).into()),
        }
    }
}
