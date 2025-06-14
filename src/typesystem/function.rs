use std::{borrow::Cow, fmt::Debug, sync::{Arc, Mutex}};

use codb_core::Ident;
use indexmap::IndexMap;

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, expression::{EvalError, Expression}};

use super::{scope::{ScopeTypes, ScopeValues}, ttype::StructType, value::{StructValue, Value}, TypeError};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Function {
    #[bw(calc = args.len() as u64)]
    args_len: u64,
    #[br(count = args_len, map = |fields: Vec<(Ident, TTypeId)>| IndexMap::from_iter(fields.into_iter()))]
    #[bw(map = |fields| Vec::<(Ident, TTypeId)>::from_iter(fields.clone().into_iter()))]
    args: IndexMap<Ident, TTypeId>,
    result_type_id: TTypeId,
    expression: Expression,
}

impl Function {
    pub fn new(pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, args: IndexMap<Ident, TTypeId>, result_type_id: TTypeId, expression: Expression) -> Result<Function, TypeError> {
        let mut scope_types = indexmap! {};

        for (name, ttype_id) in &args {
            scope_types.insert(name.clone(), ttype_id.clone());
        }

        let arg_scope = ScopeTypes::one(Cow::Owned(StructType::new(scope_types)));

        let expression_type_id = expression.eval_types(pager, registry, relations, &arg_scope)?;

        expression_type_id.must_eq(&result_type_id)?;

        Ok(Function {
            args,
            result_type_id,
            expression,
        })
    }

    pub fn args(&self) -> &IndexMap<Ident, TTypeId> {
        &self.args
    }

    pub fn result_type(&self) -> &TTypeId {
        &self.result_type_id
    }

    pub fn invoke(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, args: impl Into<Box<[Value]>>) -> Result<Value, EvalError> {
        let args = args.into();
        if args.len() != self.args.len() {
            panic!("attempted to invoke function with wrong number of arguments, got {}, expected {}", args.len(), self.args.len());
        }

        let mut arg_types = indexmap! {};
        let mut arg_values = indexmap! {};

        for ((name, ttype_id), value) in self.args.iter().zip(args) {
            arg_types.insert(name.clone(), ttype_id.clone());
            arg_values.insert(name.clone(), value);
        }

        // SAFETY: eval_types should have already checked that this is valid
        let arg_values = unsafe { StructValue::new_unchecked(
            arg_values,
        ) };

        let arg_values = ScopeValues::one(Cow::Owned(arg_values));

        self.expression.eval(pager, registry, relations, &arg_values)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut function = f.debug_tuple("");
        for (name, ttype_id) in &self.args {
            function.field(&format!("{name:?}: {ttype_id:?}"));
        }
        function.finish()?;

        write!(f, " -> {:?}", self.result_type_id)?;

        f.debug_set()
            .entry(&self.expression)
            .finish()
    }
}
