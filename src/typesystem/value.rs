use std::{collections::BTreeMap, fmt::{Debug, Display}};

use codb_core::{Ident, IdentForest};

use crate::{db::registry::{Registry, TTypeId}, typesystem::{ttype::{CompositeType, EnumType, StructType, TType}, TypeError}};

use super::{ttype::ArrayType, TypeSet};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
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

impl TryInto<CompositeValue> for Value {
    type Error = TypeError;

    fn try_into(self) -> Result<CompositeValue, Self::Error> {
        match self {
            Value::Composite(composite_value) => Ok(composite_value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Composite,
                got: value,
            }),
        }
    }
}

    impl TryInto<StructValue> for Value {
        type Error = TypeError;

        fn try_into(self) -> Result<StructValue, Self::Error> {
            match self {
                Value::Composite(CompositeValue::Struct(struct_value)) => Ok(struct_value),
                value => Err(TypeError::ValueTypeSetInvalid {
                    expected: TypeSet::Struct,
                    got: value,
                }),
            }
        }
}

impl TryInto<EnumValue> for Value {
    type Error = TypeError;

    fn try_into(self) -> Result<EnumValue, Self::Error> {
        match self {
            Value::Composite(CompositeValue::Enum(enum_value)) => Ok(enum_value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Enum,
                got: value,
            }),
        }
    }
}

impl TryInto<ArrayValue> for Value {
    type Error = TypeError;

    fn try_into(self) -> Result<ArrayValue, Self::Error> {
        match self {
            Value::Array(array_value) => Ok(array_value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Array,
                got: value,
            }),
        }
    }
}

impl TryInto<ScalarValue> for Value {
    type Error = TypeError;

    fn try_into(self) -> Result<ScalarValue, Self::Error> {
        match self {
            Value::Scalar(scalar_value) => Ok(scalar_value),
            value => Err(TypeError::ValueTypeSetInvalid {
                expected: TypeSet::Scalar,
                got: value,
            }),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct StructValue {
    ttype_id: TTypeId,
    fields: BTreeMap<Ident, Value>,
}

impl StructValue {
    pub fn new(registry: &Registry, ttype_id: TTypeId, fields: BTreeMap<Ident, Value>) -> Result<StructValue, TypeError> {
        let ttype = registry.ttype(&ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(ttype_id.clone()))?;

        let TType::Composite(CompositeType::Struct(ttype)) = ttype else {
            return Err(TypeError::TypeInvalid {
                expected: StructType::new(btreemap! {}).into(),
                got: ttype
            });
        };

        let mut new_fields = btreemap! {};

        // check all fields exist in type
        for (field_name, field_value) in &fields {
            // field exists
            if let Some(field_ttype_id) = ttype.fields().get(field_name) {
                if *field_ttype_id != field_value.ttype_id() {
                    return Err(TypeError::ValueTypeIdInvalid {
                        expected: field_ttype_id.clone(),
                        got: field_value.clone(),
                    });
                }

                new_fields.insert(field_name.clone(), field_value);
            } else {
                return Err(TypeError::UnknownField(field_name.clone()));
            }
        }

        // check literal has all fields
        for name in fields.keys() {
            if !fields.contains_key(name) {
                return Err(TypeError::MissingField(name.clone()));
            }
        }

        let value = StructValue {
            ttype_id,
            fields,
        };

        Ok(value)
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
        let TType::Composite(CompositeType::Struct(struct_type)) = ttype else {
            return Err(TypeError::TypeInvalid {
                expected: StructType::new(btreemap! {}).into(),
                got: ttype,
            })
        };
        
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
        let mut d = f.debug_struct(&format!("{:?}", self.ttype_id));
        
        for (name, value) in &self.fields {
            d.field(name, value);
        }
        
        d.finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct EnumValue {
    ttype_id: TTypeId,
    tag: Ident,
    value: Box<Value>,
}

impl EnumValue {
    pub fn new(registry: &Registry, ttype_id: TTypeId, tag: Ident, value: Value) -> Result<EnumValue, TypeError> {
        let ttype = registry.ttype(&ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(ttype_id.clone()))?;

        let TType::Composite(CompositeType::Enum(ttype)) = ttype else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: ttype,
            })
        };

        let tag_name = tag.clone();
        
        if let Some(tag_ttype_id) = ttype.tags().get(&tag_name) {
            if *tag_ttype_id != value.ttype_id() {
                return Err(TypeError::ValueTypeIdInvalid {
                    expected: tag_ttype_id.clone(),
                    got: value,
                });
            }

            Ok(EnumValue {
                ttype_id,
                tag: tag_name,
                value: Box::new(value),
            })
        } else {
            Err(TypeError::UnknownTag(tag_name))
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
        f.debug_tuple(&format!("{:?}::{:?}", self.ttype_id, self.tag))
            .field(&self.value)
            .finish()
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct ArrayValue {
    inner_ttype_id: TTypeId,
    entries: Vec<Value>,
}

impl ArrayValue {
    pub fn new(registry: &Registry, array_ttype_id: TTypeId, entries: Vec<Value>) -> Result<ArrayValue, TypeError> {
        let ttype = registry.ttype(&array_ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(array_ttype_id.clone()))?;

        let TType::Array(ttype) = ttype else {
            return Err(TypeError::TypeInvalid {
                // todo: proper error handling
                expected: ArrayType::new(TTypeId::NEVER, None).into(),
                got: ttype,
            });
        };

        // check length
        if let Some(length) = ttype.length() {
            if entries.len() as u64 != *length {
                return Err(TypeError::ArrayLen {
                    expected: *length,
                    got: entries.len() as u64,
                });
            }
        }

        // check all values are of correct type
        for entry in &entries {
            if entry.ttype_id() != *ttype.inner_ttype_id() {
                return Err(TypeError::TypeIdInvalid {
                    expected: ttype.inner_ttype_id().clone(),
                    got: entry.ttype_id(),
                });
            }
        }

        Ok(ArrayValue {
            inner_ttype_id: array_ttype_id,
            entries,
        })
    }

    pub fn entries(&self) -> &[Value] {
        &self.entries
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.inner_ttype_id.clone()
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<ArrayValue, TypeError> {
        let array_ttype = registry.ttype(&self.inner_ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.inner_ttype_id.clone()))?;

        let TType::Array(array_ttype) = array_ttype else {
            return Err(TypeError::TypeInvalid {
                expected: ArrayType::new(TTypeId::NEVER, None).into(),
                got: array_ttype,
            })
        };

        let inner_ttype = registry.ttype(array_ttype.inner_ttype_id())
            .ok_or_else(|| TypeError::TypeNotFound(self.inner_ttype_id.clone()))?;

        let selection_type = inner_ttype.select(registry, ident_forest)?;

        let mut new_entries = vec![];

        for entry in &self.entries {
            new_entries.push(entry.select(registry, ident_forest)?);
        }

        Ok(ArrayValue {
            inner_ttype_id: TTypeId::new_anonymous(selection_type),
            entries: new_entries,
        })
    }
}

impl Debug for ArrayValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?})", self.inner_ttype_id)?;

        let mut list = f.debug_list();
        
        for value in &self.entries {
            list.entry(value);
        }
        
        list.finish()
    }
}
