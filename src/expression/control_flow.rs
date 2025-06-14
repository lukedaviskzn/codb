use std::{borrow::Cow, collections::BTreeMap, fmt::Debug, sync::{Arc, Mutex}};

use codb_core::Ident;

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, typesystem::{scope::{ScopeTypes, ScopeValues}, ttype::{CompositeType, EnumType, ScalarType, StructType, TType}, value::{EnumValue, StructValue, Value}, TypeError}};

use super::{EvalError, Expression};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ControlFlow {
    #[brw(magic = 0u8)]
    If(IfControlFlow),
    #[brw(magic = 1u8)]
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
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ControlFlow::If(cf) => cf.eval_types(pager, registry, relations, scopes),
            ControlFlow::Match(cf) => cf.eval_types(pager, registry, relations, scopes),
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            ControlFlow::If(cf) => cf.eval(pager, registry, relations, scopes),
            ControlFlow::Match(cf) => cf.eval(pager, registry, relations, scopes),
        }
    }
}

#[binrw]
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
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let cond_type_id = self.condition.eval_types(pager.clone(), registry, relations, scopes)?;

        cond_type_id.must_eq(&TTypeId::BOOL)?;
        
        let then_type_id = self.then.eval_types(pager.clone(), registry, relations, scopes)?;
        let otherwise_type_id = self.then.eval_types(pager, registry, relations, scopes)?;

        then_type_id.must_eq(&self.ret_type)?;
        otherwise_type_id.must_eq(&self.ret_type)?;

        Ok(then_type_id)
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let cond_value = self.condition.eval(pager.clone(), registry, relations, scopes)?;

        if cond_value == Value::TRUE {
            self.then.eval(pager, registry, relations, scopes)
        } else {
            self.otherwise.eval(pager, registry, relations, scopes)
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct MatchControlFlow {
    pub param: Expression,
    pub ret_type: TTypeId,
    #[bw(calc = self.branches.len() as u64)]
    len: u64,
    #[br(count = len, map = |branches: Vec<(Ident, Branch)>| BTreeMap::from_iter(branches.into_iter()))]
    #[bw(map = |branches| Vec::<(Ident, Branch)>::from_iter(branches.clone().into_iter()))]
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
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let param_type_id = self.param.eval_types(pager.clone(), registry, relations, scopes)?;

        if param_type_id == TTypeId::NEVER {
            return Ok(TTypeId::NEVER);
        }
        
        let param_type = registry.ttype(&param_type_id)
            .ok_or_else(|| TypeError::TypeNotFound(param_type_id.clone()))?;

        let TType::Composite(CompositeType::Enum(param_type)) = param_type else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(indexmap! {}).into(),
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
        
        let new_scope = StructType::new(indexmap! {
            first_branch.ident.clone() => param_type.tags()[first_pattern_tag].clone(),
        });

        let mut new_scopes = scopes.clone();
        new_scopes.push(Cow::Owned(new_scope));

        // all branches return same type
        for (tag, branch) in &self.branches {
            let new_scope = StructType::new(indexmap! {
                branch.ident.clone() => param_type.tags()[tag].clone(),
            });

            let mut new_scopes = scopes.clone();
            new_scopes.push(Cow::Owned(new_scope));

            let branch_type_id = branch.expression.eval_types(pager.clone(), registry, relations, &new_scopes)?;

            branch_type_id.must_eq(&self.ret_type)?;
        }
        
        Ok(self.ret_type.clone())
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let param_value: EnumValue = self.param.eval(pager.clone(), registry, relations, scopes)?.try_into().expect("match paramter value is not an enum value");

        let branch = self.branches.get(param_value.tag()).expect("branch not found");

        let new_scope = unsafe { StructValue::new_unchecked(indexmap! {
            branch.ident.clone() => param_value.into_inner_value(),
        }) };

        let mut new_scopes = scopes.clone();
        new_scopes.push(Cow::Owned(new_scope));
        
        branch.expression.eval(pager, registry, relations, &new_scopes)
    }
}

#[binrw]
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
    use crate::{db::DbRelationSet, expression::{CompositeLiteral, EnumLiteral, Literal}, typesystem::value::ScalarValue};

    use super::*;

    #[test]
    fn match_expr() {
        let pager = Arc::new(Mutex::new(Pager::new_memory()));
        let relations = DbRelationSet::new();
        let registry = Registry::new(pager.clone(), &relations);

        let result_ttype_id = TTypeId::from(id_path!("Result"));

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Literal(Literal::Composite(
                CompositeLiteral {
                    ttype_id: result_ttype_id.clone(),
                    inner: EnumLiteral::new(
                        id!("Ok"),
                        Expression::Literal(Literal::UNIT)
                    ).into(),
                }
            )),
            ret_type: TTypeId::INT32,
            branches: btreemap! {
                id!("Ok") => Branch::new(id!("_"), Expression::Literal(ScalarValue::Int32(10).into())),
                id!("Err") => Branch::new(id!("_"), Expression::Literal(ScalarValue::Int32(8).into())),
            },
        })));

        expr.eval_types(pager.clone(), &registry, &relations, &ScopeTypes::EMPTY).unwrap();
        assert_eq!(Value::Scalar(ScalarValue::Int32(10).into()), expr.eval(pager.clone(), &registry, &relations, &ScopeValues::EMPTY).unwrap());

        let expr = Expression::ControlFlow(Box::new(
            ControlFlow::Match(MatchControlFlow {
                param: Expression::Literal(Literal::Composite(
                    CompositeLiteral {
                        ttype_id: result_ttype_id,
                        inner: EnumLiteral::new(
                            id!("Ok"),
                            Expression::Literal(Literal::UNIT),
                        ).into(),
                    }
                )),
                ret_type: TTypeId::INT32,
                branches: btreemap! {
                    id!("Ok") => Branch::new(id!("_"), Expression::Literal(ScalarValue::Int32(10).into())),
                },
            })
        ));

        assert_eq!(TypeError::MissingTag(id!("Err")), expr.eval_types(pager.clone(), &registry, &relations, &ScopeTypes::EMPTY).unwrap_err());

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Literal(Literal::Composite(
                CompositeLiteral {
                    ttype_id: id_path!("Result").into(),
                    inner: EnumLiteral::new(
                        id!("Err"),
                        Expression::Literal(ScalarValue::String("my_error".into()).into()),
                    ).into(),
                }
            )),
            ret_type: TTypeId::STRING,
            branches: btreemap! {
                id!("Ok") => Branch::new(id!("_"), Expression::Literal(ScalarValue::String("Ok".into()).into())),
                id!("Err") => Branch::new(id!("error"), Expression::NestedIdent(id!("error").into())),
            },
        })));

        assert_eq!(TTypeId::Scalar(ScalarType::String), expr.eval_types(pager.clone(), &registry, &relations, &ScopeTypes::EMPTY).unwrap());
        assert_eq!(Value::Scalar(ScalarValue::String("my_error".into()).into()), expr.eval(pager.clone(), &registry, &relations, &ScopeValues::EMPTY).unwrap());
    }
}
