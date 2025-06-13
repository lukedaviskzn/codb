use std::{fmt::{Debug, Display}, sync::{Arc, Mutex}};

use codb_core::NestedIdent;

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::Value, TypeError}};

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

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    #[brw(magic = 0u8)]
    NestedIdent(NestedIdent),
    #[brw(magic = 1u8)]
    Literal(Literal),
    #[brw(magic = 2u8)]
    Op(Box<Op>),
    #[brw(magic = 3u8)]
    ControlFlow(Box<ControlFlow>),
    #[brw(magic = 4u8)]
    FunctionInvocation(FunctionInvocation),
    #[brw(magic = 5u8)]
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
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Expression::NestedIdent(nested_ident) => scopes.get_nested(registry, nested_ident),
            Expression::Literal(literal) => literal.eval_types(pager, registry, relations, scopes),
            Expression::Op(op) => op.eval_types(pager, registry, relations, scopes),
            Expression::ControlFlow(control_flow) => control_flow.eval_types(pager, registry, relations, scopes),
            Expression::FunctionInvocation(function) => function.eval_types(pager, registry, relations, scopes),
            Expression::Action(action) => action.eval_types(pager, registry, relations, scopes),
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            // this was a user error in eval_types and db error here because it should have been caught there
            Expression::NestedIdent(nested_ident) => Ok(scopes.get_nested(nested_ident).expect("scope should have been checked")),
            Expression::Literal(literal) => literal.eval(pager, registry, relations, scopes),
            Expression::Op(op) => op.eval(pager, registry, relations, scopes),
            Expression::ControlFlow(cf) => cf.eval(pager, registry, relations, scopes),
            Expression::FunctionInvocation(function) => function.eval(pager, registry, relations, scopes),
            Expression::Action(action) => action.eval(pager, registry, relations, scopes),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Op {
    #[brw(magic = 0u8)]
    Logical(LogicalOp),
    #[brw(magic = 1u8)]
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
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Op::Logical(op) => op.eval_types(pager, registry, relations, scopes),
            Op::Arithmetic(op) => op.eval_types(pager, registry, relations, scopes),
        }
    }
    
    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Op::Logical(op) => op.eval(pager, registry, relations, scopes),
            Op::Arithmetic(op) => op.eval(pager, registry, relations, scopes),
        }
    }
}
