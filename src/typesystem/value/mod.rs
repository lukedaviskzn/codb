use std::{collections::BTreeMap, fmt::{Debug, Display}};

use codb_core::{Ident, IdentTree};

use crate::{db::registry::{Registry, TTypeId}, typesystem::{ttype::{CompositeType, EnumType, StructType, TType}, TypeError}};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Value {
    Composite(CompositeValue),
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

    pub fn select(&self, registry: &Registry, ident_trees: &[IdentTree]) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => Ok(Value::Composite(value.select(registry, ident_trees)?)),
            Value::Scalar(value) => Ok(Value::Scalar(value.select(ident_trees)?)),
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
            Self::Composite(val) => val.fmt(f),
            Self::Scalar(val) => Debug::fmt(val, f),
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

    pub fn select(&self, registry: &Registry, ident_trees: &[IdentTree]) -> Result<CompositeValue, TypeError> {
        match self {
            CompositeValue::Struct(value) => Ok(CompositeValue::Struct(value.select(registry, ident_trees)?)),
            CompositeValue::Enum(value) => Ok(CompositeValue::Enum(value.select(registry, ident_trees)?)),
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

    pub fn select(&self, registry: &Registry, ident_trees: &[IdentTree]) -> Result<StructValue, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = BTreeMap::new();
        
        for tree in ident_trees {
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
        
        let selection_type = struct_type.select(registry, ident_trees)?;
        
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

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn tag(&self) -> &Ident {
        &self.tag
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn into_value(self) -> Value {
        *self.value
    }

    pub fn select(&self, registry: &Registry, ident_trees: &[IdentTree]) -> Result<EnumValue, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let ttype = registry.ttype(&self.ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.ttype_id.clone()))?;

        let TType::Composite(CompositeType::Enum(ttype)) = ttype else {
            unreachable!();
        };

        for tag in ttype.tags().keys() {
            if ident_trees.iter().all(|tree| tree.ident() != tag) {
                return Err(TypeError::MissingTag(tag.clone()));
            }
        }

        for tree in ident_trees {
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
    String(String),
}

impl ScalarValue {
    pub fn ttype_id(&self) -> TTypeId {
        match self {
            ScalarValue::Unit => TTypeId::UNIT,
            ScalarValue::Bool(_) => TTypeId::BOOL,
            ScalarValue::Int32(_) => TTypeId::INT32,
            ScalarValue::String(_) => TTypeId::STRING,
        }
    }

    pub fn select(&self, ident_trees: &[IdentTree]) -> Result<ScalarValue, TypeError> {
        if let Some(first) = ident_trees.first() {
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
