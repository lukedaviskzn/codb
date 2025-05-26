use std::{borrow::Cow, collections::BTreeMap, fmt::Debug};

use codb_core::Ident;

use crate::{db::{registry::{Registry, TTypeId}, relation::Relation, DbRelations}, typesystem::{scope::{ScopeTypes, ScopeValues}, ttype::{CompositeType, EnumType, ScalarType, StructType, TType}, value::{CompositeValue, StructValue, Value}, TypeError}};

use super::{EvalError, Expression};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ControlFlow {
    If(IfControlFlow),
    Match(MatchControlFlow),
}

impl Debug for ControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If(cf) => Debug::fmt(cf, f),
            Self::Match(cf) => Debug::fmt(cf, f),
        }
    }
}

impl ControlFlow {
    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ControlFlow::If(cf) => cf.eval_types(registry, relations, scopes),
            ControlFlow::Match(cf) => cf.eval_types(registry, relations, scopes),
        }
    }

    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            ControlFlow::If(cf) => cf.eval(registry, relations, scopes),
            ControlFlow::Match(cf) => cf.eval(registry, relations, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct IfControlFlow {
    pub condition: Expression,
    pub ret_type: TTypeId,
    pub then: Expression,
    pub otherwise: Expression,
}

impl Debug for IfControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {:?} {{ {:?} }} else {{ {:?} }}", self.condition, self.then, self.otherwise)
    }
}

impl IfControlFlow {
    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let cond_type_id = self.condition.eval_types(registry, relations, scopes)?;

        cond_type_id.must_eq(&TTypeId::BOOL)?;
        
        let then_type_id = self.then.eval_types(registry, relations, scopes)?;
        let otherwise_type_id = self.then.eval_types(registry, relations, scopes)?;

        then_type_id.must_eq(&self.ret_type)?;
        otherwise_type_id.must_eq(&self.ret_type)?;

        Ok(then_type_id)
    }

    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let cond_value = self.condition.eval(registry, relations, scopes)?;
                
        cond_value.ttype_id().must_eq(&TTypeId::BOOL)?;

        if cond_value == Value::TRUE {
            self.then.eval(registry, relations, scopes)
        } else {
            self.otherwise.eval(registry, relations, scopes)
        }
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct MatchControlFlow {
    pub param: Expression,
    pub ret_type: TTypeId,
    pub branches: BTreeMap<Ident, Branch>,
}

impl Debug for MatchControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "match {:?} {{ ", self.param)?;

        for (i, (pattern, branch)) in self.branches.iter().enumerate() {
            write!(f, "{pattern:?}{branch:?}")?;
            if i < self.branches.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, " }}")
    }
}

impl MatchControlFlow {
    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let param_type_id = self.param.eval_types(registry, relations, scopes)?;

        if param_type_id == TTypeId::NEVER {
            return Ok(TTypeId::NEVER);
        }
        
        let param_type = registry.ttype(&param_type_id)
            .ok_or_else(|| TypeError::TypeNotFound(param_type_id.clone()))?;

        let TType::Composite(CompositeType::Enum(param_type)) = param_type else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: param_type,
            });
        };

        for tag in param_type.tags().keys() {
            if self.branches.keys().all(|pattern| pattern != tag) {
                return Err(TypeError::MissingTag(tag.clone()));
            }
        }

        for pattern in self.branches.keys() {
            if param_type.tags().keys().all(|tag| tag != pattern) {
                return Err(TypeError::UnknownTag(pattern.clone()));
            }
        }

        let Some((first_pattern_tag, first_branch)) = self.branches.first_key_value() else {
            return Ok(ScalarType::Never.into());
        };
        
        let new_scope = StructType::new(btreemap! {
            first_branch.ident.clone() => param_type.tags()[first_pattern_tag].clone(),
        });

        let mut new_scopes = scopes.clone();
        new_scopes.push(Cow::Owned(new_scope));

        // all branches return same type
        for (tag, branch) in &self.branches {
            let new_scope = StructType::new(btreemap! {
                branch.ident.clone() => param_type.tags()[tag].clone(),
            });

            let mut new_scopes = scopes.clone();
            new_scopes.push(Cow::Owned(new_scope));

            let branch_type_id = branch.expression.eval_types(registry, relations, &new_scopes)?;

            branch_type_id.must_eq(&self.ret_type)?;
        }
        
        Ok(self.ret_type.clone())
    }

    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let scope_types = scopes.types()?;
        
        let param_type = self.param.eval_types(registry, relations, &scope_types)?;
        let param_type = registry.ttype(&param_type)
            .ok_or_else(|| TypeError::from(TypeError::TypeNotFound(param_type.clone())))?;
        let TType::Composite(CompositeType::Enum(param_type)) = param_type else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: param_type,
            }.into());
        };

        let param_value = self.param.eval(registry, relations, scopes)?;

        let param_value = match param_value {
            Value::Composite(CompositeValue::Enum(param_value)) => param_value,
            param_value => return Err(TypeError::ValueTypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: param_value,
            }.into()),
        };

        let Some(branch) = self.branches.get(param_value.tag()) else {
            return Err(TypeError::UnknownTag(param_value.tag().clone()).into());
        };

        let scope_type = TTypeId::new_anonymous(StructType::new(btreemap! {
            branch.ident.clone() => param_type.tags()[param_value.tag()].clone(),
        }).into());
        
        let new_scope = StructValue::new(registry, scope_type, btreemap! {
            branch.ident.clone() => param_value.into_inner_value(),
        })?;

        let mut new_scopes = scopes.clone();
        new_scopes.push(Cow::Owned(new_scope));
        
        branch.expression.eval(registry, relations, &new_scopes)
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Branch {
    ident: Ident,
    expression: Expression,
}

impl Branch {
    pub fn new(ident: Ident, expression: Expression) -> Branch {
        Branch {
            ident,
            expression,
        }
    }
}

impl Debug for Branch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}) => {:?}", self.ident, self.expression)
    }
}

#[cfg(test)]
mod tests {
    use crate::{db::relation::memory::MemoryRelation, typesystem::value::{EnumValue, ScalarValue}};

    use super::*;

    #[test]
    fn match_expr() {
        let relations = DbRelations::<MemoryRelation>::new();
        let registry = Registry::new(&relations);

        let result_ttype_id = TTypeId::from(id_path!("Result"));

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(EnumValue::new(
                &registry, result_ttype_id.clone(),
                id!("Ok"),
                ScalarValue::Unit.into()
            ).unwrap().into()),
            ret_type: TTypeId::INT32,
            branches: btreemap! {
                id!("Ok") => Branch::new(id!("_"), Expression::Value(ScalarValue::Int32(10).into())),
                id!("Err") => Branch::new(id!("_"), Expression::Value(ScalarValue::Int32(8).into())),
            },
        })));

        expr.eval_types(&registry, &relations, &ScopeTypes::EMPTY).unwrap();
        assert_eq!(Value::Scalar(ScalarValue::Int32(10).into()), expr.eval(&registry, &relations, &ScopeValues::EMPTY).unwrap());

        let expr = Expression::ControlFlow(Box::new(
            ControlFlow::Match(MatchControlFlow {
                param: Expression::Value(EnumValue::new(&registry, result_ttype_id, id!("Ok"), ScalarValue::Unit.into()).unwrap().into()),
                ret_type: TTypeId::INT32,
                branches: btreemap! {
                    id!("Ok") => Branch::new(id!("_"), Expression::Value(ScalarValue::Int32(10).into())),
                },
            })
        ));

        assert_eq!(TypeError::MissingTag(id!("Err")), expr.eval_types(&registry, &relations, &ScopeTypes::EMPTY).unwrap_err());

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(
                EnumValue::new(
                    &registry,
                    id_path!("Result").into(),
                    id!("Err"),
                    ScalarValue::String("my_error".into()).into(),
                ).unwrap().into()
            ),
            ret_type: TTypeId::STRING,
            branches: btreemap! {
                id!("Ok") => Branch::new(id!("_"), Expression::Value(ScalarValue::String("Ok".into()).into())),
                id!("Err") => Branch::new(id!("error"), Expression::NestedIdent(id!("error").into())),
            },
        })));

        assert_eq!(TTypeId::Scalar(ScalarType::String), expr.eval_types(&registry, &relations, &ScopeTypes::EMPTY).unwrap());
        assert_eq!(Value::Scalar(ScalarValue::String("my_error".into()).into()), expr.eval(&registry, &relations, &ScopeValues::EMPTY).unwrap());
    }
}
