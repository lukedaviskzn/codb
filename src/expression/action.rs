use std::{fmt::Debug, ops::Bound};

use codb_core::{Ident, IdentForest};
use itertools::Itertools;

use crate::{db::{registry::{Registry, TTypeId}, relation::{Key, Relation, Row}, DbRelations}, typesystem::{function::Function, scope::{ScopeTypes, ScopeValues}, ttype::{ArrayType, EnumType, ScalarType, TType}, value::{ArrayValue, CompositeValue, EnumValue, ScalarValue, StructValue, Value}, TypeError}};

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
            // todo: proper type checking
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
                let relation = relation.read().unwrap(); // todo: dont unwrap

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
                let relation = relation.read().unwrap(); // todo: dont unwrap

                new_row.eval_types(registry, relations, scopes)?.must_eq(&relation.schema().ttype_id())?;
                
                Ok(TTypeId::BOOL)
            },
            InterpreterAction::Extend {
                relation,
                new_rows,
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().unwrap(); // todo: dont unwrap
                
                let ttype = new_rows.eval_types(registry, relations, scopes)?;

                ttype.must_eq(&TTypeId::Array(Box::new(ArrayType::new(relation.schema().ttype_id(), None))))?;

                Ok(TTypeId::INT64)
            },
            InterpreterAction::Remove {
                relation,
                pkey,
            } => {
                let relation = relations.get(relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().unwrap(); // todo: dont unwrap
                
                let ttype = pkey.eval_types(registry, relations, scopes)?;

                ttype.must_eq(&TTypeId::new_anonymous(relation.schema().pkey_ttype(registry)?.into()))?;

                Ok(TTypeId::new_anonymous(EnumType::new_option(relation.schema().ttype_id()).into()))
            },
            InterpreterAction::Retain {
                relation,
                condition, 
            } => {
                // todo: proper type checking
                todo!()
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
                    return Err(TypeError::ValueTypeInvalid {
                        expected: ScalarType::String.into(),
                        got: message,
                    }.into());
                };

                Err(EvalError::UserPanic(message))
            },
            InterpreterAction::Range {
                relation,
                ident_forest,
                start,
                end,
            } => {
                let relation = relations.get(&relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let relation = relation.read().unwrap(); // todo: not unwrap

                let start: Bound<StructValue> = match &**start {
                    Bound::Included(start) => Bound::Included(start.eval(registry, relations, scopes)?.try_into()?),
                    Bound::Excluded(start) => Bound::Excluded(start.eval(registry, relations, scopes)?.try_into()?),
                    Bound::Unbounded => Bound::Unbounded,
                };

                let end: Bound<StructValue> = match &**end {
                    Bound::Included(end) => Bound::Included(end.eval(registry, relations, scopes)?.try_into()?),
                    Bound::Excluded(end) => Bound::Excluded(end.eval(registry, relations, scopes)?.try_into()?),
                    Bound::Unbounded => Bound::Unbounded,
                };

                let rows = relation.range(registry, ident_forest, (start.as_ref(), end.as_ref())).unwrap(); // todo: not unwrap
                let rows = rows.map(|row| row.into()).collect_vec();

                let array_ttype_id = ArrayType::new(relation.schema().ttype_id(), None).into();

                Ok(Value::Array(ArrayValue::new(registry, array_ttype_id, rows)?))
            },
            InterpreterAction::Insert {
                relation,
                new_row,
            } => {
                let relation = relations.get(&relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let mut relation = relation.write().unwrap(); // todo: not unwrap

                let out = relation.insert(registry, new_row.eval(registry, relations, scopes)?.try_into()?)?;

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
                let relation = relations.get(&relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let mut relation = relation.write().unwrap(); // todo: not unwrap

                let new_rows = new_rows.eval(registry, relations, scopes)?;

                let Value::Array(new_rows) = new_rows else {
                    return Err(TypeError::ValueTypeIdInvalid {
                        expected: TTypeId::new_anonymous(ArrayType::new(relation.schema().ttype_id(), None).into()),
                        got: new_rows,
                    }.into());
                };

                let mut new_row_values = vec![];

                for row in new_rows.entries() {
                    new_row_values.push(row.clone().try_into()?);
                }

                let out = relation.extend(registry, new_row_values)?;

                Ok(Value::Scalar(ScalarValue::Int64(out as i64)))
            },
            InterpreterAction::Remove {
                relation,
                pkey,
            } => {
                let relation = relations.get(&relation).ok_or_else(|| TypeError::UnknownRelation(relation.clone()))?;
                let mut relation = relation.write().unwrap(); // todo: not unwrap

                let option = TTypeId::new_anonymous(EnumType::new_option(relation.schema().ttype_id()).into());
                let out = relation.remove(registry, &pkey.eval(registry, relations, scopes)?.try_into()?)?;

                match out {
                    Some(out) => Ok(EnumValue::new_option_some(option, out.into()).into()),
                    None => Ok(EnumValue::new_option_none(option).into()),
                }
            },
            InterpreterAction::Retain {
                relation,
                condition,
            } => todo!(),
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
            } => write!(f, "#{relation:?}.range({ident_forest:?}, {start:?}, {end:?})"),
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
