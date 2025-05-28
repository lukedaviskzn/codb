use std::{cmp::Ordering, collections::BTreeMap, fmt::{Debug, Display}};

use codb_core::{Ident, IdentForest};

use crate::{db::{registry::{Registry, TTypeId}, relation::Relation, DbRelations}, expression::{debug_db_array, debug_db_enum, debug_db_struct, StructLiteral}, typesystem::{ttype::{CompositeType, EnumType, StructType, TType}, TypeError}};

use super::{scope::ScopeTypes, ttype::ArrayType, TypeSet};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub enum Value {
    Composite(CompositeValue),
    Scalar(ScalarValue),
    Array(ArrayValue),
}

impl Value {
    pub const UNIT: Value = Value::Scalar(ScalarValue::Unit);
    pub const TRUE: Value = Value::Scalar(ScalarValue::Bool(true));
    pub const FALSE: Value = Value::Scalar(ScalarValue::Bool(false));

    pub fn ttype_id(&self) -> TTypeId {
        match self {
            Value::Composite(value) => value.ttype_id(),
            Value::Scalar(value) => value.ttype_id(),
            Value::Array(value) => value.ttype_id(),
        }
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => Ok(Value::Composite(value.select(registry, ident_forest)?)),
            Value::Scalar(value) => Ok(Value::Scalar(value.select(ident_forest)?)),
            Value::Array(value) => Ok(Value::Array(value.select(registry, ident_forest)?))
        }
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => value.dot(ident),
            Value::Scalar(_) => Err(TypeError::ScalarField(ident.clone())),
            Value::Array(_) => Err(TypeError::ArrayField(ident.clone()))
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(val) => Debug::fmt(val, f),
            Self::Scalar(val) => Debug::fmt(val, f),
            Self::Array(val) => Debug::fmt(val, f),
        }
    }
}

impl From<CompositeValue> for Value {
    fn from(value: CompositeValue) -> Self {
        Self::Composite(value)
    }
}

impl From<StructValue> for Value {
    fn from(value: StructValue) -> Self {
        Self::Composite(CompositeValue::Struct(value))
    }
}

impl From<EnumValue> for Value {
    fn from(value: EnumValue) -> Self {
        Self::Composite(CompositeValue::Enum(value))
    }
}

impl From<ScalarValue> for Value {
    fn from(value: ScalarValue) -> Self {
        Self::Scalar(value)
    }
}

impl From<ArrayValue> for Value {
    fn from(value: ArrayValue) -> Self {
        Self::Array(value)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub enum CompositeValue {
    Struct(StructValue),
    Enum(EnumValue),
}

impl CompositeValue {
    pub fn ttype_id(&self) -> TTypeId {
        match self {
            CompositeValue::Struct(value) => value.ttype_id(),
            CompositeValue::Enum(value) => value.ttype_id(),
        }
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<CompositeValue, TypeError> {
        match self {
            CompositeValue::Struct(value) => Ok(CompositeValue::Struct(value.select(registry, ident_forest)?)),
            CompositeValue::Enum(value) => Ok(CompositeValue::Enum(value.select(registry, ident_forest)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        match self {
            CompositeValue::Struct(value) => value.dot(ident),
            CompositeValue::Enum(_) => Err(TypeError::DotTag(ident.clone())),
        }
    }
}

impl Debug for CompositeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(value) => value.fmt(f),
            Self::Enum(value) => value.fmt(f),
        }
    }
}

impl From<StructValue> for CompositeValue {
    fn from(value: StructValue) -> Self {
        CompositeValue::Struct(value)
    }
}

impl From<EnumValue> for CompositeValue {
    fn from(value: EnumValue) -> Self {
        CompositeValue::Enum(value)
    }
}

impl TryFrom<Value> for CompositeValue {
    type Error = TypeError;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Composite(value) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Composite,
                got: value,
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct StructValue {
    ttype_id: TTypeId,
    fields: BTreeMap<Ident, Value>,
}

impl StructValue {
    pub unsafe fn new_unchecked(ttype_id: TTypeId, fields: BTreeMap<Ident, Value>) -> StructValue {
        StructValue {
            ttype_id,
            fields,
        }
    }

    pub fn fields(&self) -> &BTreeMap<Ident, Value> {
        &self.fields
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<StructValue, TypeError> {
        if ident_forest.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = BTreeMap::new();
        
        for tree in ident_forest {
            let ident = tree.ident().clone();
            if let Some(value) = self.fields.get(&ident) {
                new_fields.insert(ident, value.select(registry, tree.children())?);
            } else {
                return Err(TypeError::UnknownField(ident));
            }
        }

        let ttype = registry.ttype(&self.ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let struct_type: StructType = ttype.try_into()?;
        
        let selection_type = struct_type.select(registry, ident_forest)?;
        
        Ok(StructValue {
            ttype_id: TTypeId::new_anonymous(selection_type.into()),
            fields: new_fields,
        })
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        if let Some(value) = self.fields.get(ident) {
            Ok(value.clone())
        } else {
            Err(TypeError::UnknownField(ident.clone()))
        }
    }
}

impl Debug for StructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_db_struct(f, &self.ttype_id, self.fields.iter())
    }
}

impl PartialOrd for StructValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.ttype_id != other.ttype_id || self.fields.len() != other.fields.len() {
            return None;
        }
        for ((left_ident, left_value), (right_ident, right_value)) in self.fields.iter().zip(other.fields.iter()) {
            debug_assert_eq!(left_ident, right_ident);
            
            let order = left_value.cmp(right_value);

            let Ordering::Equal = order else {
                return Some(order)
            };
        }
        Some(Ordering::Equal)
    }
}

impl Ord for StructValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare values, {:?}, {:?}, types not equal", self, other))
    }
}

impl TryFrom<Value> for StructValue {
    type Error = TypeError;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Composite(CompositeValue::Struct(value)) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Struct,
                got: value,
            }),
        }
    }
}

impl TryFrom<CompositeValue> for StructValue {
    type Error = TypeError;
    
    fn try_from(value: CompositeValue) -> Result<Self, Self::Error> {
        match value {
            CompositeValue::Struct(value) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Struct,
                got: value.into(),
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct EnumValue {
    ttype_id: TTypeId,
    tag: Ident,
    value: Box<Value>,
}

impl EnumValue {
    pub unsafe fn new_unchecked(ttype_id: TTypeId, tag: Ident, value: Value) -> EnumValue {
        EnumValue {
            ttype_id,
            tag,
            value: Box::new(value),
        }
    }

    pub fn new_option_some(ttype_id: TTypeId, some: Value) -> EnumValue {
        EnumValue {
            ttype_id,
            tag: id!("Some"),
            value: Box::new(some),
        }
    }

    pub fn new_option_none(ttype_id: TTypeId) -> EnumValue {
        EnumValue {
            ttype_id,
            tag: id!("None"),
            value: Box::new(Value::UNIT),
        }
    }

    pub fn new_result_ok(ttype_id: TTypeId, ok: Value) -> EnumValue {
        EnumValue {
            ttype_id,
            tag: id!("Ok"),
            value: Box::new(ok),
        }
    }

    pub fn new_result_err(ttype_id: TTypeId, err: Value) -> EnumValue {
        EnumValue {
            ttype_id,
            tag: id!("Err"),
            value: Box::new(err),
        }
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn tag(&self) -> &Ident {
        &self.tag
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn into_inner_value(self) -> Value {
        *self.value
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<EnumValue, TypeError> {
        if ident_forest.is_empty() {
            return Ok(self.clone());
        }

        let ttype = registry.ttype(&self.ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let TType::Composite(CompositeType::Enum(ttype)) = ttype else {
            unreachable!();
        };

        for tag in ttype.tags().keys() {
            if ident_forest.iter().all(|tree| tree.ident() != tag) {
                return Err(TypeError::MissingTag(tag.clone()));
            }
        }

        for tree in ident_forest {
            if ttype.tags().keys().all(|tag| tag != tree.ident()) {
                return Err(TypeError::UnknownTag(tree.ident().clone()));
            }
        }

        Ok(self.clone())
    }
}

impl Debug for EnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_db_enum(f, &self.ttype_id, &self.tag, &self.value)
    }
}

impl TryFrom<Value> for EnumValue {
    type Error = TypeError;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Composite(CompositeValue::Enum(value)) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Enum,
                got: value,
            }),
        }
    }
}

impl TryFrom<CompositeValue> for EnumValue {
    type Error = TypeError;
    
    fn try_from(value: CompositeValue) -> Result<Self, Self::Error> {
        match value {
            CompositeValue::Enum(value) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Enum,
                got: value.into(),
            }),
        }
    }
}

impl PartialOrd for EnumValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.ttype_id != other.ttype_id {
            return None;
        }

        let order = self.tag.cmp(&other.tag);
        let Ordering::Equal = order else {
            return Some(order);
        };

        Some(self.value.cmp(&other.value))
    }
}

impl Ord for EnumValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare values, {:?}, {:?}, types not equal", self, other))
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScalarValue {
    Unit,
    Bool(bool),
    Int32(i32),
    Int64(i64),
    String(String),
}

impl ScalarValue {
    pub fn ttype_id(&self) -> TTypeId {
        match self {
            ScalarValue::Unit => TTypeId::UNIT,
            ScalarValue::Bool(_) => TTypeId::BOOL,
            ScalarValue::Int32(_) => TTypeId::INT32,
            ScalarValue::Int64(_) => TTypeId::INT64,
            ScalarValue::String(_) => TTypeId::STRING,
        }
    }

    pub fn select(&self, ident_forest: &IdentForest) -> Result<ScalarValue, TypeError> {
        if let Some(first) = ident_forest.first() {
            Err(TypeError::ScalarField(first.ident().clone()))
        } else {
            Ok(self.clone())
        }
    }
}

impl Debug for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(value) => Debug::fmt(value, f),
            Self::Int32(value) => Debug::fmt(value, f),
            Self::Int64(value) => Debug::fmt(value, f),
            Self::String(value) => Debug::fmt(value, f),
        }
    }
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(value) => Display::fmt(value, f),
            value => Debug::fmt(value, f),
        }
    }
}

impl TryFrom<Value> for ScalarValue {
    type Error = TypeError;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Scalar(value) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Scalar,
                got: value,
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ArrayValue {
    ttype_id: TTypeId,
    entries: Vec<Value>,
}

impl ArrayValue {
    pub unsafe fn new_unchecked(array_ttype_id: TTypeId, entries: Vec<Value>) -> ArrayValue {
        ArrayValue {
            ttype_id: array_ttype_id,
            entries,
        }
    }

    pub fn entries(&self) -> &[Value] {
        &self.entries
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<ArrayValue, TypeError> {
        let array_ttype = registry.ttype(&self.ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let TType::Array(array_ttype) = array_ttype else {
            return Err(TypeError::TypeInvalid {
                expected: ArrayType::new(TTypeId::NEVER, None).into(),
                got: array_ttype,
            })
        };

        let inner_ttype = registry.ttype(array_ttype.inner_ttype_id())
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let selection_type = inner_ttype.select(registry, ident_forest)?;

        let mut new_entries = vec![];

        for entry in &self.entries {
            new_entries.push(entry.select(registry, ident_forest)?);
        }

        Ok(ArrayValue {
            ttype_id: TTypeId::new_anonymous(selection_type),
            entries: new_entries,
        })
    }
}

impl Debug for ArrayValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_db_array(f, &self.ttype_id, self.entries.iter())
    }
}

impl TryFrom<Value> for ArrayValue {
    type Error = TypeError;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Array(value) => Ok(value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Array,
                got: value,
            }),
        }
    }
}

impl PartialOrd for ArrayValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.ttype_id != other.ttype_id {
            return None;
        }
        Some(self.entries.cmp(&other.entries))
    }
}

impl Ord for ArrayValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare values, {:?}, {:?}, types not equal", self, other))
    }
}
