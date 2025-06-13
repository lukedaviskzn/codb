use core::fmt;
use std::{collections::BTreeMap, fmt::Debug, sync::{Arc, Mutex}};

use codb_core::Ident;

use crate::{db::{pager::Pager, registry::{Registry, TTypeId}, DbRelationSet}, expression::Expression, typesystem::{scope::{ScopeTypes, ScopeValues}, ttype::{EnumType, StructType, TType}, value::{ArrayValue, EnumValue, ScalarValue, StructValue, Value}, TypeError, TypeSet}};

use super::EvalError;

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Literal {
    #[brw(magic = 0u8)]
    Composite(CompositeLiteral),
    #[brw(magic = 1u8)]
    Scalar(ScalarValue),
    #[brw(magic = 2u8)]
    Array(ArrayLiteral),
}

impl Literal {
    pub const UNIT: Literal = Literal::Scalar(ScalarValue::Unit);
    pub const TRUE: Literal = Literal::Scalar(ScalarValue::Bool(true));
    pub const FALSE: Literal = Literal::Scalar(ScalarValue::Bool(false));

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Literal::Composite(literal) => literal.eval_types(pager, registry, relations, scopes),
            Literal::Scalar(literal) => Ok(literal.ttype_id()),
            Literal::Array(literal) => literal.eval_types(pager, registry, relations, scopes),
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Literal::Composite(literal) => literal.eval(pager, registry, relations, scopes),
            Literal::Scalar(literal) => Ok(literal.clone().into()),
            Literal::Array(literal) => literal.eval(pager, registry, relations, scopes),
        }
    }
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(val) => Debug::fmt(val, f),
            Self::Scalar(val) => Debug::fmt(val, f),
            Self::Array(val) => Debug::fmt(val, f),
        }
    }
}

impl From<CompositeLiteral> for Literal {
    fn from(value: CompositeLiteral) -> Self {
        Self::Composite(value)
    }
}

impl From<StructLiteral> for Literal {
    fn from(value: StructLiteral) -> Self {
        Self::Composite(CompositeLiteral::Struct(value))
    }
}

impl From<EnumLiteral> for Literal {
    fn from(value: EnumLiteral) -> Self {
        Self::Composite(CompositeLiteral::Enum(value))
    }
}

impl From<ScalarValue> for Literal {
    fn from(value: ScalarValue) -> Self {
        Self::Scalar(value)
    }
}

impl From<ArrayLiteral> for Literal {
    fn from(value: ArrayLiteral) -> Self {
        Self::Array(value)
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CompositeLiteral {
    #[brw(magic = 0u8)]
    Struct(StructLiteral),
    #[brw(magic = 1u8)]
    Enum(EnumLiteral),
}

impl CompositeLiteral {
    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            CompositeLiteral::Struct(literal) => literal.eval_types(pager, registry, relations, scopes),
            CompositeLiteral::Enum(literal) => literal.eval_types(pager, registry, relations, scopes),
        }
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            CompositeLiteral::Struct(literal) => literal.eval(pager, registry, relations, scopes),
            CompositeLiteral::Enum(literal) => literal.eval(pager, registry, relations, scopes),
        }
    }
}

impl Debug for CompositeLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(value) => value.fmt(f),
            Self::Enum(value) => value.fmt(f),
        }
    }
}

impl From<StructLiteral> for CompositeLiteral {
    fn from(value: StructLiteral) -> Self {
        CompositeLiteral::Struct(value)
    }
}

impl From<EnumLiteral> for CompositeLiteral {
    fn from(value: EnumLiteral) -> Self {
        CompositeLiteral::Enum(value)
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
pub struct StructLiteral {
    pub ttype_id: TTypeId,
    #[bw(calc = self.fields.len() as u64)]
    len: u64,
    #[br(count = len, map = |fields: Vec<(Ident, Expression)>| BTreeMap::from_iter(fields.into_iter()))]
    #[bw(map = |fields| Vec::<(Ident, Expression)>::from_iter(fields.clone().into_iter()))]
    pub fields: BTreeMap<Ident, Expression>,
}

impl StructLiteral {
    pub fn new(ttype_id: TTypeId, fields: BTreeMap<Ident, Expression>) -> StructLiteral {
        StructLiteral {
            ttype_id,
            fields,
        }
    }

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let ttype = registry.ttype(&self.ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let ttype: StructType = ttype.try_into()?;

        let mut new_fields = btreemap! {};

        // check all fields exist in type
        for (field_name, field_value) in &self.fields {
            // field exists
            if let Some(field_ttype_id) = ttype.fields().get(field_name) {
                let field_value_ttype = field_value.eval_types(pager.clone(), registry, relations, scopes)?;
                if *field_ttype_id != field_value_ttype {
                    return Err(TypeError::TypeIdInvalid {
                        expected: field_ttype_id.clone(),
                        got: field_value_ttype,
                    });
                }

                new_fields.insert(field_name.clone(), field_value);
            } else {
                return Err(TypeError::UnknownField(field_name.clone()));
            }
        }

        // check literal has all fields
        for name in self.fields.keys() {
            if !self.fields.contains_key(name) {
                return Err(TypeError::MissingField(name.clone()));
            }
        }
        
        Ok(self.ttype_id.clone())
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let mut fields = indexmap! {};

        let ttype: StructType = registry
            .ttype(&self.ttype_id).expect("type not found")
            .try_into().expect("not a struct");

        for key in ttype.fields().keys() {
            fields.insert(key.clone(), self.fields[key].eval(pager.clone(), registry, relations, scopes)?);
        }

        // SAFETY: above lines would have crashed otherwise
        let struct_value = unsafe { StructValue::new_unchecked(self.ttype_id.clone(), fields) };
        
        Ok(struct_value.into())
    }
}

pub fn debug_db_struct<'a, D: Debug + 'static>(f: &mut std::fmt::Formatter<'_>, ttype_id: &TTypeId, fields: impl Iterator<Item = (&'a Ident, &'a D)>) -> fmt::Result {
    let mut d = f.debug_struct(&format!("{:?}", ttype_id));
    
    for (name, value) in fields {
        d.field(name, value);
    }
    
    d.finish()
}

impl Debug for StructLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_db_struct(f, &self.ttype_id, self.fields.iter())
    }
}

impl TryFrom<Literal> for StructLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        match value {
            Literal::Composite(CompositeLiteral::Struct(value)) => Ok(value),
            _ => Err(()),
        }
    }
}

impl TryFrom<CompositeLiteral> for StructLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: CompositeLiteral) -> Result<Self, Self::Error> {
        match value {
            CompositeLiteral::Struct(value) => Ok(value),
            _ => Err(()),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct EnumLiteral {
    pub ttype_id: TTypeId,
    pub tag: Ident,
    pub value: Box<Expression>,
}

impl EnumLiteral {
    pub fn new(ttype_id: TTypeId, tag: Ident, value: Expression) -> EnumLiteral {
        EnumLiteral {
            ttype_id,
            tag,
            value: Box::new(value),
        }
    }

    pub fn new_option_some(ttype_id: TTypeId, some: Expression) -> EnumLiteral {
        EnumLiteral {
            ttype_id,
            tag: id!("Some"),
            value: Box::new(some),
        }
    }

    pub fn new_option_none(ttype_id: TTypeId) -> EnumLiteral {
        EnumLiteral {
            ttype_id,
            tag: id!("None"),
            value: Box::new(Expression::Literal(Literal::UNIT)),
        }
    }

    pub fn new_result_ok(ttype_id: TTypeId, ok: Expression) -> EnumLiteral {
        EnumLiteral {
            ttype_id,
            tag: id!("Ok"),
            value: Box::new(ok),
        }
    }

    pub fn new_result_err(ttype_id: TTypeId, err: Expression) -> EnumLiteral {
        EnumLiteral {
            ttype_id,
            tag: id!("Err"),
            value: Box::new(err),
        }
    }

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let ttype = registry.ttype(&self.ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let ttype: EnumType = ttype.try_into()?;

        let tag_name = self.tag.clone();
        
        if let Some(tag_ttype_id) = ttype.tags().get(&tag_name) {
            let value_ttype_id = self.value.eval_types(pager, registry, relations, scopes)?;
            if *tag_ttype_id != value_ttype_id {
                return Err(TypeError::TypeIdInvalid {
                    expected: tag_ttype_id.clone(),
                    got: value_ttype_id,
                });
            }

            Ok(self.ttype_id.clone())
        } else {
            Err(TypeError::UnknownTag(tag_name))
        }
    }
    
    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        // SAFETY: this function should only be used after eval_types
        let enum_value = unsafe { EnumValue::new_unchecked(
            self.ttype_id.clone(),
            self.tag.clone(),
            self.value.eval(pager, registry, relations, scopes)?,
        ) };
        
        Ok(enum_value.into())
    }
}

pub fn debug_db_enum<'a, D: Debug + 'static>(f: &mut std::fmt::Formatter<'_>, ttype_id: &TTypeId, tag: &'a Ident, value: &'a D) -> fmt::Result {
    f.debug_tuple(&format!("{ttype_id:?}#{tag:?}"))
        .field(value)
        .finish()
}

impl Debug for EnumLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_db_enum(f, &self.ttype_id, &self.tag, &self.value)
    }
}

impl TryFrom<Literal> for EnumLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        match value {
            Literal::Composite(CompositeLiteral::Enum(value)) => Ok(value),
            _ => Err(()),
        }
    }
}

impl TryFrom<CompositeLiteral> for EnumLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: CompositeLiteral) -> Result<Self, Self::Error> {
        match value {
            CompositeLiteral::Enum(value) => Ok(value),
            _ => Err(()),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ArrayLiteral {
    array_ttype_id: TTypeId,
    #[bw(calc = entries.len() as u64)]
    len: u64,
    #[br(count = len)]
    entries: Vec<Expression>,
}

impl ArrayLiteral {
    pub fn new(array_ttype_id: TTypeId, entries: Vec<Expression>) -> ArrayLiteral {
        ArrayLiteral {
            array_ttype_id,
            entries,
        }
    }

    pub fn eval_types(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let ttype = registry.ttype(&self.array_ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.array_ttype_id.clone()))?;

        let TType::Array(ttype) = ttype else {
            return Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Array,
                got: ttype,
            });
        };

        // check length
        if let Some(length) = ttype.length() {
            if self.entries.len() as u64 != *length {
                return Err(TypeError::ArrayLen {
                    expected: *length,
                    got: self.entries.len() as u64,
                });
            }
        }

        // check all values are of correct type
        for entry in &self.entries {
            let entry_ttype_id = entry.eval_types(pager.clone(), registry, relations, scopes)?;
            if entry_ttype_id != *ttype.inner_ttype_id() {
                return Err(TypeError::TypeIdInvalid {
                    expected: ttype.inner_ttype_id().clone(),
                    got: entry_ttype_id,
                });
            }
        }

        Ok(self.array_ttype_id.clone())
    }

    pub fn eval(&self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let mut entries = vec![];

        for entry in &self.entries {
            entries.push(entry.eval(pager.clone(), registry, relations, scopes)?);
        }

        // SAFETY: this function should only be used after eval_types
        let array_value = unsafe { ArrayValue::new_unchecked(self.array_ttype_id.clone(), entries) };
        
        Ok(array_value.into())
    }
}

pub fn debug_db_array<'a, D: Debug + 'static>(f: &mut std::fmt::Formatter<'_>, array_ttype_id: &TTypeId, entries: impl Iterator<Item = &'a D>) -> fmt::Result {
    write!(f, "{:?} ", array_ttype_id)?;

    let mut list = f.debug_list();
    
    for value in entries {
        list.entry(value);
    }
    
    list.finish()
}

impl Debug for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_db_array(f, &self.array_ttype_id, self.entries.iter())
    }
}

impl TryFrom<Literal> for ArrayLiteral {
    type Error = (); // todo better errors for literal conversions
    
    fn try_from(value: Literal) -> Result<Self, Self::Error> {
        match value {
            Literal::Array(value) => Ok(value),
            _ => Err(()),
        }
    }
}
