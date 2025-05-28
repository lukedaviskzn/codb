use std::{fmt::Debug, ops::Bound};

use codb_core::{Ident, IdentForest};
use itertools::Itertools;

use crate::{db::{registry::{Registry, TTypeId}, relation::Relation, DbRelations}, typesystem::{function::Function, scope::{ScopeTypes, ScopeValues}, ttype::{ArrayType, EnumType}, value::{ArrayValue, EnumValue, ScalarValue, StructValue, Value}, TypeError}};

use super::{EvalError, Expression};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum InterpreterAction {
    Panic {
        message: Box<Expression>,
    },
    Range {
        relation: Ident,
        ident_forest: IdentForest,
        start: Box<Bound<Expression>>,
        end: Box<Bound<Expression>>,
    },
    Insert {
        relation: Ident,
        new_row: Box<Expression>,
    },
    Extend {
        relation: Ident,
        new_rows: Box<Expression>,
    },
    Remove {
        relation: Ident,
        pkey: Box<Expression>,
    },
    Retain {
        relation: Ident,
        condition: Box<Function>,
    },
}

impl InterpreterAction {
    pub fn eval_types<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            InterpreterAction::Panic {
                message
            } => {
                let ttype = message.eval_types(registry, relations, scopes)?;
                ttype.must_eq(&TTypeId::STRING)?;
                Ok(TTypeId::NEVER)
            },
            InterpreterAction::Range {
                relation,
                ident_forest,
                start,
                end,
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().expect("failed to acquire read lock");

                // ensure valid ident forest
                let selection_type = relation.schema().ttype().select(registry, ident_forest)?;
                let selection_type_id = TTypeId::new_anonymous(selection_type.into());

                // check start type
                if let Bound::Included(start) | Bound::Excluded(start) = &**start {
                    let start_type = start.eval_types(registry, relations, scopes)?;
                    start_type.must_eq(&selection_type_id)?;
                }

                // check end type
                if let Bound::Included(end) | Bound::Excluded(end) = &**end {
                    let end_type = end.eval_types(registry, relations, scopes)?;
                    end_type.must_eq(&selection_type_id)?;
                }

                // returns variable length array of relation type
                Ok(ArrayType::new(relation.schema().ttype_id(), None).into())
            },
            InterpreterAction::Insert {
                relation,
                new_row,
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().expect("failed to acquire read lock");

                new_row.eval_types(registry, relations, scopes)?.must_eq(&relation.schema().ttype_id())?;
                
                Ok(TTypeId::BOOL)
            },
            InterpreterAction::Extend {
                relation,
                new_rows,
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().expect("failed to acquire read lock");
                
                let ttype = new_rows.eval_types(registry, relations, scopes)?;

                ttype.must_eq(&TTypeId::Array(Box::new(ArrayType::new(relation.schema().ttype_id(), None))))?;

                Ok(TTypeId::INT64)
            },
            InterpreterAction::Remove {
                relation,
                pkey,
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().expect("failed to acquire read lock");
                
                let ttype = pkey.eval_types(registry, relations, scopes)?;

                ttype.must_eq(&TTypeId::new_anonymous(relation.schema().pkey_ttype(registry)?.into()))?;

                Ok(TTypeId::new_anonymous(EnumType::new_option(relation.schema().ttype_id()).into()))
            },
            InterpreterAction::Retain {
                relation,
                condition, 
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().expect("failed to acquire read lock");

                let row_ttype_id = relation.schema().ttype_id();

                if condition.args().len() != 1 {
                    return Err(TypeError::FunctionArgLen {
                        expected: 1,
                        got: condition.args().len(),
                    });
                }

                condition.args()[0].ttype_id().must_eq(&row_ttype_id)?;
                condition.result_type().must_eq(&TTypeId::BOOL)?;

                Ok(TTypeId::INT64)
            },
        }
    }

    pub fn eval<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            InterpreterAction::Panic {
                message,
            } => {
                let message = message.eval(registry, relations, scopes)?;
                let Value::Scalar(ScalarValue::String(message)) = message else {
                    panic!("panicked while (user) panicking, argument is not a string");
                };

                Err(EvalError::UserPanic(message))
            },
            InterpreterAction::Range {
                relation,
                ident_forest,
                start,
                end,
            } => {
                let relation = relations.get(&relation).expect("unknown relation");
                let relation = relation.read().expect("failed to acquire read lock");

                let start: Bound<StructValue> = match &**start {
                    Bound::Included(start) => Bound::Included(start.eval(registry, relations, scopes)?.try_into().expect("bound is not a struct")),
                    Bound::Excluded(start) => Bound::Excluded(start.eval(registry, relations, scopes)?.try_into().expect("bound is not a struct")),
                    Bound::Unbounded => Bound::Unbounded,
                };

                let end: Bound<StructValue> = match &**end {
                    Bound::Included(end) => Bound::Included(end.eval(registry, relations, scopes)?.try_into().expect("bound is not a struct")),
                    Bound::Excluded(end) => Bound::Excluded(end.eval(registry, relations, scopes)?.try_into().expect("bound is not a struct")),
                    Bound::Unbounded => Bound::Unbounded,
                };

                let rows = relation.range(registry, ident_forest, (start.as_ref(), end.as_ref()));
                let rows = rows.map(|row| row.into()).collect_vec();

                let array_ttype_id = ArrayType::new(relation.schema().ttype_id(), None).into();

                // SAFETY: eval_types should have already checked that this is valid
                Ok(Value::Array(unsafe { ArrayValue::new_unchecked(array_ttype_id, rows) }))
            },
            InterpreterAction::Insert {
                relation,
                new_row,
            } => {
                let relation = relations.get(&relation).expect("unknown relation");
                let mut relation = relation.write().expect("failed to acquire write lock");

                let out = relation.insert(registry, new_row.eval(registry, relations, scopes)?.try_into().expect("new row is not a struct"));

                let out = if out {
                    Value::TRUE
                } else {
                    Value::FALSE
                };

                Ok(out)
            },
            InterpreterAction::Extend {
                relation,
                new_rows,
            } => {
                let relation = relations.get(&relation).expect("unknown relation");
                let mut relation = relation.write().expect("failed to acquire write lock");

                let new_rows: ArrayValue = new_rows.eval(registry, relations, scopes)?.try_into().expect("new rows are is not an array");

                let mut new_row_values = vec![];

                for row in new_rows.entries() {
                    new_row_values.push(row.clone().try_into().expect("new row is not a struct"));
                }

                let out = relation.extend(registry, new_row_values);

                Ok(Value::Scalar(ScalarValue::Int64(out as i64)))
            },
            InterpreterAction::Remove {
                relation,
                pkey,
            } => {
                let relation = relations.get(&relation).expect("unknown relation");
                let mut relation = relation.write().expect("failed to acquire write lock");

                let option = TTypeId::new_anonymous(EnumType::new_option(relation.schema().ttype_id()).into());
                let out = relation.remove(registry, &pkey.eval(registry, relations, scopes)?.try_into().expect("primary key is not a struct"));

                match out {
                    Some(out) => Ok(EnumValue::new_option_some(option, out.into()).into()),
                    None => Ok(EnumValue::new_option_none(option).into()),
                }
            },
            InterpreterAction::Retain {
                relation: _,
                condition: _,
            } => {
                // let relation = relations.get(&relation).expect("unknown relation");
                // let mut relation = relation.write().expect("failed to acquire write lock");

                // let out = relation.retain(|row| condition.invoke(registry, relations, [row.clone().into()])?);

                // Ok(Value::Scalar(ScalarValue::Int64(out as i64)))
                todo!()
            },
        }
    }
}

impl Debug for InterpreterAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Panic { message } => f.debug_tuple("panic").field(message).finish(),
            Self::Range {
                relation,
                ident_forest,
                start,
                end,
            } => write!(f, "#{relation:?}.range<{ident_forest:?}>({start:?}, {end:?})"),
            Self::Insert {
                relation,
                new_row,
            } => write!(f, "#{relation:?}.insert({new_row:?})"),
            Self::Extend {
                relation,
                new_rows,
            } => write!(f, "#{relation:?}.extend({new_rows:?})"),
            Self::Remove {
                relation,
                pkey,
            } => write!(f, "#{relation:?}.remove({pkey:?})"),
            Self::Retain {
                relation,
                condition,
            } => write!(f, "#{relation:?}.retain({condition:?})"),
        }
    }
}
