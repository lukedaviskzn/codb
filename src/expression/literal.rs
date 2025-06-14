use core::fmt;
use std::{collections::BTreeMap, fmt::Debug, sync::{Arc, Mutex}};

use codb_core::Ident;

use crate::{db::{pager::Pager, registry::{CompositeTTypeId, Registry, TTypeId}, DbRelationSet}, expression::Expression, typesystem::{scope::{ScopeTypes, ScopeValues}, ttype::{ArrayType, CompositeType, EnumType, StructType, TType}, value::{ArrayValue, CompositeValue, CompositeValueInner, EnumValue, ScalarValue, StructValue, Value}, TypeError, TypeSet}};

use super::EvalError;

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Literal {
    #[brw(magic = 0u8)]
    Composite(CompositeLiteral),
    #[brw(magic = 1u8)]
    Scalar(ScalarValue),
}

impl Literal {
    pub const UNIT: Literal = Literal::Scalar(ScalarValue::Unit);
    pub const TRUE: Literal = Literal::Scalar(ScalarValue::Bool(true));
    pub const FALSE: Literal = Literal::Scalar(ScalarValue::Bool(false));

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Literal::Composite(literal) => literal.eval_types(pager, registry, relations, scopes),
            Literal::Scalar(literal) => Ok(literal.ttype_id()),
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Literal::Composite(literal) => Ok(Value::Composite(literal.eval(pager, registry, relations, scopes)?)),
            Literal::Scalar(literal) => Ok(literal.clone().into()),
        }
    }
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(val) => Debug::fmt(val, f),
            Self::Scalar(val) => Debug::fmt(val, f),
        }
    }
}

impl From<CompositeLiteral> for Literal {
    fn from(value: CompositeLiteral) -> Self {
        Self::Composite(value)
    }
}

impl From<ScalarValue> for Literal {
    fn from(value: ScalarValue) -> Self {
        Self::Scalar(value)
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct CompositeLiteral {
    pub ttype_id: TTypeId,
    pub inner: CompositeLiteralInner,
}

impl CompositeLiteral {
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let ttype = registry.ttype(&self.ttype_id).ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        match (
            ttype,
            &self.inner,
            // &self.inner.eval_types(pager.clone(), registry, relations, scopes)?,
        ) {
            (
                TType::Composite(CompositeType::Struct(ttype)),
                CompositeLiteralInner::Struct(literal),
                // CompositeType::Struct(literal_type),
            ) => {
                // check literal fields are expected in type and check types
                for (field, expr) in &literal.fields {
                    let field_ttype_id = ttype.fields().get(field).ok_or_else(|| TypeError::UnknownField(field.clone()))?;
                    expr.eval_types(pager.clone(), registry, relations, scopes)?.must_eq(field_ttype_id);
                }

                // check no fields missing
                for field in ttype.fields().keys() {
                    if !literal.fields.contains_key(field) {
                        return Err(TypeError::MissingField(field.clone()));
                    }
                }

                Ok(self.ttype_id.clone())
            },
            (
                TType::Composite(CompositeType::Enum(ttype)),
                CompositeLiteralInner::Enum(literal),
                // CompositeType::Enum(literal_type),
            ) => {
                let tag_ttype_id = ttype.tags().get(&literal.tag).ok_or_else(|| TypeError::UnknownTag(literal.tag.clone()))?;

                let inner_type = literal.inner_type(pager, registry, relations, scopes)?;
                inner_type.must_eq(tag_ttype_id)?;
                
                Ok(self.ttype_id.clone())
            },
            (
                TType::Composite(CompositeType::Array(_)),
                CompositeLiteralInner::Array(literal),
                // CompositeType::Array(literal_type),
            ) => {
                let inner_type = literal.inner_type(pager, registry, relations, scopes)?;
                let array_type = TTypeId::Composite(CompositeTTypeId::Anonymous(
                    Box::new(CompositeType::Array(ArrayType::new(
                        inner_type,
                        Some(literal.entries.len() as u64),
                    )))
                ));

                self.ttype_id.must_eq(&array_type)?;

                Ok(self.ttype_id.clone())
            },
            (
                ttype,
                inner,
                // literal_type,
            ) => {
                Err(TypeError::TypeSetInvalid {
                    expected: match inner {
                        CompositeLiteralInner::Struct(_) => TypeSet::Struct,
                        CompositeLiteralInner::Enum(_) => TypeSet::Enum,
                        CompositeLiteralInner::Array(_) => TypeSet::Array,
                    },
                    got: ttype,
                })
            }
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<CompositeValue, EvalError> {
        let inner = match &self.inner {
            CompositeLiteralInner::Struct(literal) => CompositeValueInner::Struct(literal.eval(pager, registry, relations, scopes)?),
            CompositeLiteralInner::Enum(literal) => CompositeValueInner::Enum(literal.eval(pager, registry, relations, scopes)?),
            CompositeLiteralInner::Array(literal) => CompositeValueInner::Array(literal.eval(pager, registry, relations, scopes)?),
        };
        
        Ok(CompositeValue {
            ttype_id: self.ttype_id.clone(),
            inner,
        })
    }
}

impl Debug for CompositeLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ttype_id)?;
        Debug::fmt(&self.inner, f)
    }
}

impl TryFrom<Literal> for CompositeLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        match value {
            Literal::Composite(value) => Ok(value),
            _ => Err(()),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CompositeLiteralInner {
    #[brw(magic = 0u8)]
    Struct(StructLiteral),
    #[brw(magic = 1u8)]
    Enum(EnumLiteral),
    #[brw(magic = 2u8)]
    Array(ArrayLiteral),
}

impl CompositeLiteralInner {
    // pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<CompositeType, TypeError> {
    //     match self {
    //         CompositeLiteralInner::Struct(literal) => Ok(
    //             CompositeType::Struct(
    //                 literal.eval_types(pager, registry, relations, scopes)?
    //             )
    //         ),
    //         CompositeLiteralInner::Enum(literal) => Ok(
    //             CompositeType::Enum(
    //                 EnumType::new(indexmap! {
    //                     literal.tag.clone() => literal.inner_type(pager, registry, relations, scopes)?,
    //                 })
    //             )
    //         ),
    //         CompositeLiteralInner::Array(literal) => Ok(
    //             CompositeType::Array(
    //                 ArrayType::new(
    //                     literal.inner_type(pager, registry, relations, scopes)?,
    //                     Some(literal.entries.len() as u64),
    //                 )
    //             )
    //         ),
    //     }
    // }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<CompositeValueInner, EvalError> {
        match self {
            CompositeLiteralInner::Struct(literal) => Ok(CompositeValueInner::Struct(literal.eval(pager, registry, relations, scopes)?)),
            CompositeLiteralInner::Enum(literal) => Ok(CompositeValueInner::Enum(literal.eval(pager, registry, relations, scopes)?)),
            CompositeLiteralInner::Array(literal) => Ok(CompositeValueInner::Array(literal.eval(pager, registry, relations, scopes)?)),
        }
    }
}

impl Debug for CompositeLiteralInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(value) => value.fmt(f),
            Self::Enum(value) => value.fmt(f),
            Self::Array(value) => value.fmt(f),
        }
    }
}

impl From<StructLiteral> for CompositeLiteralInner {
    fn from(value: StructLiteral) -> Self {
        CompositeLiteralInner::Struct(value)
    }
}

impl From<EnumLiteral> for CompositeLiteralInner {
    fn from(value: EnumLiteral) -> Self {
        CompositeLiteralInner::Enum(value)
    }
}

impl From<ArrayLiteral> for CompositeLiteralInner {
    fn from(value: ArrayLiteral) -> Self {
        CompositeLiteralInner::Array(value)
    }
}

impl TryFrom<Literal> for CompositeLiteralInner {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        match value {
            Literal::Composite(value) => Ok(value.inner),
            _ => Err(()),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct StructLiteral {
    #[bw(calc = self.fields.len() as u64)]
    len: u64,
    #[br(count = len, map = |fields: Vec<(Ident, Expression)>| BTreeMap::from_iter(fields.into_iter()))]
    #[bw(map = |fields| Vec::<(Ident, Expression)>::from_iter(fields.clone().into_iter()))]
    pub fields: BTreeMap<Ident, Expression>,
}

impl StructLiteral {
    pub fn new(fields: BTreeMap<Ident, Expression>) -> StructLiteral {
        StructLiteral {
            fields,
        }
    }

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<StructType, TypeError> {
        let mut fields = indexmap! {};

        for (name, expr) in &self.fields {
            let field_type = expr.eval_types(pager.clone(), registry, relations, scopes)?;
            fields.insert(name.clone(), field_type);
        }
        
        Ok(StructType::new(fields))
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<StructValue, EvalError> {
        let mut fields = indexmap! {};
        
        for (field, expr) in &self.fields {
            let field_value = expr.eval(pager.clone(), registry, relations, scopes)?;
            fields.insert(field.clone(), field_value);
        }
        
        Ok(unsafe { StructValue::new_unchecked(fields) })
    }
}

impl Debug for StructLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();

        for (field, expr) in &self.fields {
            map.entry(field, expr);
        }

        map.finish()
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct EnumLiteral {
    pub tag: Ident,
    pub expr: Box<Expression>,
}

impl EnumLiteral {
    pub fn new(tag: Ident, expr: Expression) -> EnumLiteral {
        EnumLiteral {
            tag,
            expr: Box::new(expr),
        }
    }

    pub fn new_option_some(some: Expression) -> EnumLiteral {
        EnumLiteral {
            tag: id!("Some"),
            expr: Box::new(some),
        }
    }

    pub fn new_option_none() -> EnumLiteral {
        EnumLiteral {
            tag: id!("None"),
            expr: Box::new(Expression::Literal(Literal::UNIT)),
        }
    }

    pub fn new_result_ok(ok: Expression) -> EnumLiteral {
        EnumLiteral {
            tag: id!("Ok"),
            expr: Box::new(ok),
        }
    }

    pub fn new_result_err(err: Expression) -> EnumLiteral {
        EnumLiteral {
            tag: id!("Err"),
            expr: Box::new(err),
        }
    }

    pub fn inner_type(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let inner_type = self.expr.eval_types(pager, registry, relations, scopes)?;
        Ok(inner_type)
    }
    
    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<EnumValue, EvalError> {
        let inner_value = self.expr.eval(pager, registry, relations, scopes)?;
        Ok(unsafe { EnumValue::new_unchecked(self.tag.clone(), inner_value) })
    }
}

impl Debug for EnumLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:?}(", self.tag)?;
        Debug::fmt(&self.expr, f)?;
        write!(f, ")")
    }
}

impl TryFrom<CompositeLiteralInner> for EnumLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: CompositeLiteralInner) -> Result<Self, Self::Error> {
        match value {
            CompositeLiteralInner::Enum(value) => Ok(value),
            _ => Err(()),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ArrayLiteral {
    #[bw(calc = entries.len() as u64)]
    len: u64,
    #[br(count = len)]
    entries: Vec<Expression>,
}

impl ArrayLiteral {
    pub fn new(entries: Vec<Expression>) -> ArrayLiteral {
        ArrayLiteral {
            entries,
        }
    }

    pub fn inner_type(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        if let Some(entry) = self.entries.first() {
            let first_type = entry.eval_types(pager.clone(), registry, relations, scopes)?;

            for entry in &self.entries {
                let entry_type = entry.eval_types(pager.clone(), registry, relations, scopes)?;
                entry_type.must_eq(&first_type)?;
            }

            Ok(first_type)
        } else {
            Ok(TTypeId::NEVER)
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<ArrayValue, EvalError> {
        let mut entries = vec![];

        for entry in &self.entries {
            entries.push(entry.eval(pager.clone(), registry, relations, scopes)?);
        }

        // SAFETY: this function should only be used after eval_types
        let array_value = unsafe { ArrayValue::new_unchecked(entries) };
        
        Ok(array_value)
    }
}

impl Debug for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(&self.entries)
            .finish()
    }
}
