use std::fmt::{Debug, Display};

use codb_core::NestedIdent;

use crate::{db::registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}, typesystem::{value::Value, TypeError}};

mod logical_op;
mod arithmetic_op;
mod control_flow;
mod function_invocation;

pub use logical_op::*;
pub use arithmetic_op::*;
pub use control_flow::*;
pub use function_invocation::*;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error("{0:?}")]
    TypeError(#[from] TypeError),
    #[error("evaluation panicked{}{}", if .0.is_empty() { "!" } else { ": " }, .0)]
    Panic(String),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    NestedIdent(NestedIdent),
    Value(Value),
    Op(Box<Op>),
    ControlFlow(Box<ControlFlow>),
    FunctionInvocation(FunctionInvocation),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NestedIdent(nested_ident) => Display::fmt(&nested_ident.join("."), f),
            Self::Value(value) => Debug::fmt(value, f),
            Self::Op(op) => Debug::fmt(op, f),
            Self::ControlFlow(cf) => Debug::fmt(cf, f),
            Self::FunctionInvocation(function) => Debug::fmt(function, f),
        }
    }
}

impl Expression {
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Expression::NestedIdent(nested_ident) => scopes.get_nested(registry, nested_ident),
            Expression::Value(value) => Ok(value.ttype_id()),
            Expression::Op(op) => op.eval_types(registry, scopes),
            Expression::ControlFlow(control_flow) => control_flow.eval_types(registry, scopes),
            Expression::FunctionInvocation(function) => function.eval_types(registry, scopes),
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Expression::NestedIdent(nested_ident) => Ok(scopes.get_nested(nested_ident)?),
            Expression::Value(value) => Ok(value.clone()),
            Expression::Op(op) => Ok(op.eval(registry, scopes)?),
            Expression::ControlFlow(cf) => Ok(cf.eval(registry, scopes)?),
            Expression::FunctionInvocation(function) => function.eval(registry, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Op {
    Logical(LogicalOp),
    Arithmetic(ArithmeticOp),
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Logical(op) => Debug::fmt(op, f),
            Self::Arithmetic(op) => Debug::fmt(op, f),
        }
    }
}

impl Op {
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Op::Logical(op) => op.eval_types(registry, scopes),
            Op::Arithmetic(op) => op.eval_types(registry, scopes),
        }
    }
    
    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Op::Logical(op) => op.eval(registry, scopes),
            Op::Arithmetic(op) => op.eval(registry, scopes),
        }
    }
}
