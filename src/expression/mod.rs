use std::fmt::{Debug, Display};

use codb_core::NestedIdent;

use crate::{db::{registry::{Registry, TTypeId}, relation::Relation, DbRelations}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::Value, TypeError}};

mod logical_op;
mod arithmetic_op;
mod control_flow;
mod function_invocation;
mod action;
mod literal;

pub use logical_op::*;
pub use arithmetic_op::*;
pub use control_flow::*;
pub use function_invocation::*;
pub use action::*;
pub use literal::*;

#[derive(Debug, thiserror::Error)]
pub enum EvalError {
    #[error("panic{}{}", if .0.is_empty() { "!" } else { ": " }, .0)]
    UserPanic(String),
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    NestedIdent(NestedIdent),
    Literal(Literal),
    Op(Box<Op>),
    ControlFlow(Box<ControlFlow>),
    FunctionInvocation(FunctionInvocation),
    Action(InterpreterAction),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NestedIdent(nested_ident) => Display::fmt(&nested_ident.join("."), f),
            Self::Literal(value) => Debug::fmt(value, f),
            Self::Op(op) => Debug::fmt(op, f),
            Self::ControlFlow(cf) => Debug::fmt(cf, f),
            Self::FunctionInvocation(function) => Debug::fmt(function, f),
            Self::Action(action) => Debug::fmt(action, f),
        }
    }
}

impl Expression {
    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Expression::NestedIdent(nested_ident) => scopes.get_nested(registry, nested_ident),
            Expression::Literal(literal) => literal.eval_types(registry, relations, scopes),
            Expression::Op(op) => op.eval_types(registry, relations, scopes),
            Expression::ControlFlow(control_flow) => control_flow.eval_types(registry, relations, scopes),
            Expression::FunctionInvocation(function) => function.eval_types(registry, relations, scopes),
            Expression::Action(action) => action.eval_types(registry, relations, scopes),
        }
    }

    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Expression::NestedIdent(nested_ident) => Ok(scopes.get_nested(nested_ident).expect("could not access nested value on scope")),
            Expression::Literal(literal) => literal.eval(registry, relations, scopes),
            Expression::Op(op) => op.eval(registry, relations, scopes),
            Expression::ControlFlow(cf) => cf.eval(registry, relations, scopes),
            Expression::FunctionInvocation(function) => function.eval(registry, relations, scopes),
            Expression::Action(action) => action.eval(registry, relations, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Op::Logical(op) => op.eval_types(registry, relations, scopes),
            Op::Arithmetic(op) => op.eval_types(registry, relations, scopes),
        }
    }
    
    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Op::Logical(op) => op.eval(registry, relations, scopes),
            Op::Arithmetic(op) => op.eval(registry, relations, scopes),
        }
    }
}
