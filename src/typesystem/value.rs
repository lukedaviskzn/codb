use std::{cmp::Ordering, fmt::{Debug, Display}, io};

use codb_core::{Ident, IdentForest};
use indexmap::IndexMap;

use crate::{db::registry::{Registry, TTypeId}, typesystem::{ttype::StructType, TypeError}};

use super::TypeSet;

#[binrw]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub enum Value {
    #[brw(magic = 0u8)]
    Composite(CompositeValue),
    #[brw(magic = 1u8)]
    Scalar(ScalarValue),
}

impl Value {
    pub const UNIT: Value = Value::Scalar(ScalarValue::Unit);
    pub const TRUE: Value = Value::Scalar(ScalarValue::Bool(true));
    pub const FALSE: Value = Value::Scalar(ScalarValue::Bool(false));

    pub fn ttype_id(&self) -> TTypeId {
        match self {
            Value::Composite(value) => value.ttype_id(),
            Value::Scalar(value) => value.ttype_id(),
        }
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => Ok(Value::Composite(value.select(registry, ident_forest)?)),
            Value::Scalar(value) => Ok(Value::Scalar(value.select(ident_forest)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => value.dot(ident),
            Value::Scalar(_) => Err(TypeError::ScalarField(ident.clone())),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(val) => Debug::fmt(val, f),
            Self::Scalar(val) => Debug::fmt(val, f),
        }
    }
}

impl From<CompositeValue> for Value {
    fn from(value: CompositeValue) -> Self {
        Self::Composite(value)
    }
}

impl From<ScalarValue> for Value {
    fn from(value: ScalarValue) -> Self {
        Self::Scalar(value)
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct CompositeValue {
    pub ttype_id: TTypeId,
    pub inner: CompositeValueInner,
}

impl CompositeValue {
    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<CompositeValue, TypeError> {
        let ttype = registry.ttype(&self.ttype_id).ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;
        let selection_type = ttype.select(registry, ident_forest)?;
        
        Ok(CompositeValue {
            ttype_id: TTypeId::new_anonymous(selection_type),
            inner: self.inner.select(registry, ident_forest)?,
        })
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        self.inner.dot(ident)
    }
}

impl Debug for CompositeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ttype_id)?;
        Debug::fmt(&self.inner, f)
    }
}

impl PartialOrd for CompositeValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for CompositeValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("failed to compare composite values")
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

#[binrw]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
pub enum CompositeValueInner {
    #[brw(magic = 0u8)]
    Struct(StructValue),
    #[brw(magic = 1u8)]
    Enum(EnumValue),
    #[brw(magic = 2u8)]
    Array(ArrayValue),
}

impl CompositeValueInner {
    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<CompositeValueInner, TypeError> {
        match self {
            CompositeValueInner::Struct(value) => Ok(CompositeValueInner::Struct(value.select(registry, ident_forest)?)),
            CompositeValueInner::Enum(value) => Ok(CompositeValueInner::Enum(value.select(registry, ident_forest)?)),
            CompositeValueInner::Array(value) => Ok(CompositeValueInner::Array(value.select(registry, ident_forest)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        match self {
            CompositeValueInner::Struct(value) => value.dot(ident),
            CompositeValueInner::Enum(_) => Err(TypeError::DotTag(ident.clone())),
            CompositeValueInner::Array(_) => Err(TypeError::ArrayField(ident.clone())),
        }
    }
}

impl Debug for CompositeValueInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(value) => value.fmt(f),
            Self::Enum(value) => value.fmt(f),
            Self::Array(value) => value.fmt(f),
        }
    }
}

impl From<StructValue> for CompositeValueInner {
    fn from(value: StructValue) -> Self {
        CompositeValueInner::Struct(value)
    }
}

impl From<EnumValue> for CompositeValueInner {
    fn from(value: EnumValue) -> Self {
        CompositeValueInner::Enum(value)
    }
}

impl From<ArrayValue> for CompositeValueInner {
    fn from(value: ArrayValue) -> Self {
        CompositeValueInner::Array(value)
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct StructValue {
    #[bw(calc = self.fields.len() as u64)]
    len: u64,
    #[br(count = len, map = |fields: Vec<(Ident, Value)>| IndexMap::from_iter(fields.into_iter()))]
    #[bw(map = |fields| Vec::<(Ident, Value)>::from_iter(fields.clone().into_iter()))]
    fields: IndexMap<Ident, Value>,
}

impl StructValue {
    pub unsafe fn new_unchecked(fields: IndexMap<Ident, Value>) -> StructValue {
        StructValue {
            fields,
        }
    }

    pub fn ttype(&self) -> StructType {
        let mut fields = indexmap! {};

        for (name, value) in &self.fields {
            fields.insert(name.clone(), value.ttype_id());
        }

        StructType::new(fields)
    }

    pub fn fields(&self) -> &IndexMap<Ident, Value> {
        &self.fields
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<StructValue, TypeError> {
        if ident_forest.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = IndexMap::new();
        
        for tree in ident_forest {
            let ident = tree.ident().clone();
            if let Some(value) = self.fields.get(&ident) {
                new_fields.insert(ident, value.select(registry, tree.children())?);
            } else {
                return Err(TypeError::UnknownField(ident));
            }
        }

        Ok(StructValue {
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
        f.debug_map()
            .entries(&self.fields)
            .finish()
    }
}

impl PartialOrd for StructValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.fields.len() != other.fields.len() {
            return None;
        }
        
        for ((left_ident, left_value), (right_ident, right_value)) in self.fields.iter().zip(other.fields.iter()) {
            if left_ident != right_ident {
                return None;
            }
            
            let order = left_value.partial_cmp(right_value)?;

            let Ordering::Equal = order else {
                return Some(order)
            };
        }
        
        Some(Ordering::Equal)
    }
}

impl Ord for StructValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare struct values {self:?}, {other:?}"))
    }
}

impl TryFrom<Value> for StructValue {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Composite(CompositeValue {
                inner: CompositeValueInner::Struct(value),
                ..
            }) => Ok(value),
            _ => Err(())
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct EnumValue {
    tag: Ident,
    value: Box<Value>,
}

impl EnumValue {
    pub unsafe fn new_unchecked(tag: Ident, value: Value) -> EnumValue {
        EnumValue {
            tag,
            value: Box::new(value),
        }
    }

    pub fn new_option_some(some: Value) -> EnumValue {
        EnumValue {
            tag: id!("Some"),
            value: Box::new(some),
        }
    }

    pub fn new_option_none() -> EnumValue {
        EnumValue {
            tag: id!("None"),
            value: Box::new(Value::UNIT),
        }
    }

    pub fn new_result_ok(ok: Value) -> EnumValue {
        EnumValue {
            tag: id!("Ok"),
            value: Box::new(ok),
        }
    }

    pub fn new_result_err(err: Value) -> EnumValue {
        EnumValue {
            tag: id!("Err"),
            value: Box::new(err),
        }
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
        if let Some(tree) = ident_forest.first() {
            Err(TypeError::DotTag(tree.ident().clone()))
        } else {
            Ok(self.clone())
        }
    }
}

impl Debug for EnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:?}(", self.tag)?;
        Debug::fmt(&self.value, f)?;
        write!(f, ")")
    }
}

impl PartialOrd for EnumValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.tag != other.tag {
            return None;
        }
        self.value.partial_cmp(&other.value)
    }
}

impl Ord for EnumValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare enum values {self:?}, {other:?}"))
    }
}

impl TryFrom<Value> for EnumValue {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Composite(CompositeValue {
                inner: CompositeValueInner::Enum(value),
                ..
            }) => Ok(value),
            _ => Err(())
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScalarValue {
    #[brw(magic = 1u8)]
    Unit,
    #[brw(magic = 2u8)]
    Bool(
        #[br(try_map = |x: u8| if x == 1 { Ok(true) } else if x == 0 { Ok(false) } else { Err(io::ErrorKind::InvalidData) })]
        #[bw(map = |x| *x as u8)]
        bool
    ),
    #[brw(magic = 3u8)]
    Int32(i32),
    #[brw(magic = 4u8)]
    Int64(i64),
    #[brw(magic = 5u8)]
    String(
        #[bw(calc = self_1.len() as u64)]
        u64,
        #[br(count = self_0, try_map = |bytes: Vec<u8>| String::from_utf8(bytes))]
        #[bw(map = |string| string.as_bytes())]
        String,
    ),
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
            Self::Unit => write!(f, "unit"),
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

impl PartialOrd for ScalarValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (ScalarValue::Unit, ScalarValue::Unit) => Some(Ordering::Equal),
            (ScalarValue::Bool(left), ScalarValue::Bool(right)) => left.partial_cmp(right),
            (ScalarValue::Int32(left), ScalarValue::Int32(right)) => left.partial_cmp(right),
            (ScalarValue::Int64(left), ScalarValue::Int64(right)) => left.partial_cmp(right),
            (ScalarValue::String(left), ScalarValue::String(right)) => left.partial_cmp(right),
            _ => None,
        }
    }
}

impl Ord for ScalarValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare scalar values {self:?}, {other:?}"))
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

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ArrayValue {
    #[bw(calc = entries.len() as u64)]
    len: u64,
    #[br(count = len)]
    entries: Vec<Value>,
}

impl ArrayValue {
    pub unsafe fn new_unchecked(entries: Vec<Value>) -> ArrayValue {
        ArrayValue {
            entries,
        }
    }

    pub fn entries(&self) -> &[Value] {
        &self.entries
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<ArrayValue, TypeError> {
        let mut new_entries = vec![];

        for entry in &self.entries {
            new_entries.push(entry.select(registry, ident_forest)?);
        }

        Ok(ArrayValue {
            entries: new_entries,
        })
    }
}

impl Debug for ArrayValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(&self.entries)
            .finish()
    }
}

impl PartialOrd for ArrayValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.entries.partial_cmp(&other.entries)
    }
}

impl Ord for ArrayValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect(&format!("failed to compare array values {self:?}, {other:?}"))
    }
}

impl TryFrom<Value> for ArrayValue {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Composite(CompositeValue {
                inner: CompositeValueInner::Array(value),
                ..
            }) => Ok(value),
            _ => Err(()),
        }
    }
}
