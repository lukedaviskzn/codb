use std::{borrow::Cow, fmt::Debug};

use codb_core::Ident;

use crate::{db::{registry::{Registry, TTypeId}, relation::Relation, DbRelations}, expression::{EvalError, Expression}};

use super::{scope::{ScopeTypes, ScopeValues}, ttype::StructType, value::{StructValue, Value}, TypeError};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Function {
    args: Box<[FunctionArg]>,
    result_type_id: TTypeId,
    expression: Expression,
}

impl Function {
    pub fn new<T: Into<FunctionArg>, R: Relation>(registry: &Registry, relations: &DbRelations<R>, args: impl IntoIterator<Item = T>, result_type_id: TTypeId, expression: Expression) -> Result<Function, TypeError> {
        let args: Box<[FunctionArg]> = args.into_iter().map(|arg| arg.into()).collect();

        let mut scope_types = indexmap! {};

        for arg in &args {
            scope_types.insert(arg.name.clone(), arg.ttype_id().clone());
        }

        let arg_scope = ScopeTypes::one(Cow::Owned(StructType::new(scope_types)));

        let expression_type_id = expression.eval_types(registry, relations, &arg_scope)?;

        expression_type_id.must_eq(&result_type_id)?;

        Ok(Function {
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

    pub fn invoke<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, args: impl Into<Box<[Value]>>) -> Result<Value, EvalError> {
        let args = args.into();
        if args.len() != self.args.len() {
            panic!("attempted to invoke function with wrong number of arguments, got {}, expected {}", args.len(), self.args.len());
        }

        let mut arg_types = indexmap! {};
        let mut arg_values = btreemap! {};

        for (arg, value) in self.args.iter().zip(args) {
            arg_types.insert(arg.name().clone(), arg.ttype_id().clone());
            arg_values.insert(arg.name().clone(), value);
        }

        let arg_types = StructType::new(arg_types);
        // SAFETY: eval_types should have already checked that this is valid
        let arg_values = unsafe { StructValue::new_unchecked(
            TTypeId::new_anonymous(arg_types.clone().into()),
            arg_values,
        ) };

        let arg_values = ScopeValues::one(Cow::Owned(arg_values));

        self.expression.eval(registry, relations, &arg_values)
    }
}

impl Debug for Function {
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

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
