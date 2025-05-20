use std::{borrow::Cow, fmt::Debug};

use codb_core::Ident;

use crate::{expr::{EvalError, Expression}, registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}};

use super::{ttype::StructType, value::{StructValue, Value}, TypeError};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionEntry<'a> {
    UserFunction(&'a UserFunction),
    InterpreterFunction(&'a InterpreterFunction),
}

impl<'a> FunctionEntry<'a> {
    pub fn args(&self) -> &[FunctionArg] {
        match self {
            Self::UserFunction(function) => function.args(),
            Self::InterpreterFunction(function) => function.args(),
        }
    }

    pub fn result_type(&self) -> &TTypeId {
        match self {
            Self::UserFunction(function) => function.result_type(),
            Self::InterpreterFunction(function) => function.result_type(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct UserFunction {
    args: Box<[FunctionArg]>,
    result_type_id: TTypeId,
    expression: Expression,
}

impl UserFunction {
    pub fn new<T: Into<FunctionArg>>(registry: &Registry, args: impl IntoIterator<Item = T>, result_type_id: TTypeId, expression: Expression) -> Result<UserFunction, TypeError> {
        let args: Box<[FunctionArg]> = args.into_iter().map(|arg| arg.into()).collect();

        let mut scope_types = btreemap! {};

        for arg in &args {
            scope_types.insert(arg.name.clone(), arg.ttype_id().clone());
        }

        let arg_scope = ScopeTypes::one(Cow::Owned(StructType::new(scope_types)));

        let expression_type_id = expression.eval_types(registry, &arg_scope)?;

        registry.types().expect_type(&result_type_id, &expression_type_id)?;

        Ok(UserFunction {
            args,
            result_type_id,
            expression,
        })
    }

    pub fn args(&self) -> &[FunctionArg] {
        &self.args
    }

    pub fn result_type(&self) -> &TTypeId {
        &self.result_type_id
    }

    pub fn invoke(&self, registry: &Registry, args: impl Into<Box<[Value]>>) -> Result<Value, EvalError> {
        let args = args.into();
        if args.len() != self.args.len() {
            return Err(TypeError::FunctionArgLen {
                expected: self.args.len(),
                got: args.len(),
            }.into());
        }

        let mut arg_types = btreemap! {};
        let mut arg_values = btreemap! {};

        for (arg, value) in self.args.iter().zip(args) {
            arg_types.insert(arg.name().clone(), arg.ttype_id().clone());
            arg_values.insert(arg.name().clone(), value);
        }

        let arg_types = StructType::new(arg_types);
        let arg_values = StructValue::new(
            registry.types(),
            TTypeId::Anonymous(Box::new(arg_types.clone().into())),
            arg_values,
        )?;

        let arg_values = ScopeValues::one(Cow::Owned(arg_values));

        self.expression.eval(registry, &arg_values)
    }
}

impl Debug for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut function = f.debug_tuple("");
        for arg in &self.args {
            function.field(arg);
        }
        function.finish()?;

        write!(f, " -> {:?}", self.result_type_id)?;

        f.debug_set()
            .entry(&self.expression)
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct FunctionArg {
    name: Ident,
    ttype_id: TTypeId,
}

impl FunctionArg {
    pub fn new(name: Ident, ttype_id: TTypeId) -> FunctionArg {
        FunctionArg {
            name,
            ttype_id,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn ttype_id(&self) -> &TTypeId {
        &self.ttype_id
    }
}

impl Debug for FunctionArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.name, self.ttype_id)
    }
}

impl From<(Ident, TTypeId)> for FunctionArg {
    fn from((name, ttype_id): (Ident, TTypeId)) -> Self {
        Self {
            name,
            ttype_id,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum InterpreterFunctionAction {
    Panic,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct InterpreterFunction {
    args: Box<[FunctionArg]>,
    result_type_id: TTypeId,
    action: InterpreterFunctionAction,
}

impl InterpreterFunction {
    pub fn new<T: Into<FunctionArg>>(args: impl IntoIterator<Item = T>, result_type_id: TTypeId, action: InterpreterFunctionAction) -> Result<InterpreterFunction, TypeError> {
        Ok(InterpreterFunction {
            args: args.into_iter().map(|arg| arg.into()).collect(),
            result_type_id,
            action,
        })
    }

    pub fn args(&self) -> &[FunctionArg] {
        &self.args
    }

    pub fn result_type(&self) -> &TTypeId {
        &self.result_type_id
    }

    pub fn action(&self) -> &InterpreterFunctionAction {
        &self.action
    }
}

impl Debug for InterpreterFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut function = f.debug_tuple("");
        for arg in &self.args {
            function.field(arg);
        }
        function.finish()?;

        write!(f, " -> {:?}", self.result_type_id)?;
        f.debug_set()
            .entry(&"interpreter feature")
            .finish()
    }
}
