use std::fmt::{Debug, Display};

use itertools::Itertools;

use crate::{idents::{Ident, IdentTree}, typesystem::{registry::{TTypeId, TypeRegistry}, ttype::ScalarType, DuplicateField, TypeError}};

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

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<Value, TypeError> {
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum CompositeValue {
    Struct(StructValue),
    Enum(EnumValue),
}

impl CompositeValue {
    pub fn new_struct(registry: &TypeRegistry, ttype_id: &TTypeId, fields: impl Into<Vec<FieldValue>>) -> Result<CompositeValue, TypeError> {
        Ok(CompositeValue::Struct(StructValue::new(registry, ttype_id, fields)?))
    }

    pub fn new_enum(registry: &TypeRegistry, ttype_id: &TTypeId, tag: FieldValue) -> Result<CompositeValue, TypeError> {
        Ok(CompositeValue::Enum(EnumValue::new(registry, ttype_id, tag)?))
    }

    pub fn ttype_id(&self) -> TTypeId {
        match self {
            CompositeValue::Struct(value) => value.ttype_id(),
            CompositeValue::Enum(value) => value.ttype_id(),
        }
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<CompositeValue, TypeError> {
        match self {
            CompositeValue::Struct(value) => Ok(CompositeValue::Struct(value.select(registry, ident_trees)?)),
            CompositeValue::Enum(value) => Ok(CompositeValue::Enum(value.select(ident_trees)?)),
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

#[derive(Clone, Eq, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct StructValue {
    ttype_id: TTypeId,
    fields: Vec<FieldValue>,
}

impl PartialEq for StructValue {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(std::cmp::Ordering::Equal)
    }
}

impl PartialOrd for StructValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let self_values = self.fields.iter()
            .sorted_by(|f1, f2| f1.name.cmp(&f2.name))
            .map(|f| &f.value);
        let other_values = other.fields.iter()
            .sorted_by(|f1, f2| f1.name.cmp(&f2.name))
            .map(|f| &f.value);
        self_values.partial_cmp(other_values)
    }
}

impl StructValue {
    pub fn new(registry: &TypeRegistry, ttype_id: &TTypeId, fields: impl Into<Vec<FieldValue>>) -> Result<StructValue, TypeError> {
        let fields: Vec<FieldValue> = fields.into();

        if let Some(_) = fields.iter().duplicates_by(|f| &f.name).next() {
            Err(DuplicateField.into())
        } else {
            let value = Value::Composite(CompositeValue::Struct(StructValue {
                ttype_id: ttype_id.clone(),
                fields,
            }));
            
            registry.get_by_id(&ttype_id)?.check(registry, &value)?;

            let Value::Composite(CompositeValue::Struct(value)) = value else { unreachable!() };

            Ok(value)
        }
    }

    pub fn fields(&self) -> &[FieldValue] {
        &self.fields
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<StructValue, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = Vec::new();
        
        for ident in ident_trees {
            if let Some(field) = self.fields.iter().find(|f| &f.name == ident.ident()) {
                let new_field = FieldValue {
                    name: field.name.clone(),
                    value: field.value.select(registry, ident.children())?,
                };
                new_fields.push(new_field);
            } else {
                return Err(TypeError::UnknownField(ident.ident().clone()));
            }
        }

        let selection_ttype = registry.get_by_id(&self.ttype_id)?
            .select(registry, ident_trees)?;
        
        Ok(StructValue {
            ttype_id: TTypeId::Anonymous(Box::new(selection_ttype)),
            fields: new_fields,
        })
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        if let Some(field) = self.fields.iter().find(|f| &f.name == ident) {
            Ok(field.value.clone())
        } else {
            Err(TypeError::UnknownField(ident.clone()))
        }
    }
}

impl Debug for StructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("(×)");
        
        for field in &self.fields {
            d.field(&field.name, &field.value);
        }
        
        d.finish()
    }
}

#[derive(Clone, PartialEq, Eq, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct EnumValue {
    ttype_id: TTypeId,
    tag: Box<FieldValue>,
}

impl PartialOrd for EnumValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.tag.value.partial_cmp(&other.tag.value)
    }
}

impl EnumValue {
    pub fn new(registry: &TypeRegistry, ttype_id: &TTypeId, tag: FieldValue) -> Result<EnumValue, TypeError> {
        let value = Value::Composite(CompositeValue::Enum(EnumValue {
            ttype_id: ttype_id.clone(),
            tag: Box::new(tag),
        }));

        registry.get_by_id(ttype_id)?.check(registry, &value);

        let Value::Composite(CompositeValue::Enum(value)) = value else {
            unreachable!();
        };

        Ok(value)
    }

    pub fn into_tag(self) -> FieldValue {
        *self.tag
    }

    pub fn tag(&self) -> &FieldValue {
        &self.tag
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn select(&self, ident_trees: &[IdentTree]) -> Result<EnumValue, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        // tag must be listed
        if ident_trees.iter().all(|ident| ident.ident() != &self.tag.name) {
            return Err(TypeError::MissingTag(self.tag.name.clone()));
        }

        // all idents must be valid
        for ident in ident_trees {
            if &self.tag.name != ident.ident() {
                return Err(TypeError::UnknownTag(ident.ident().clone()));
            }
        }

        Ok(self.clone())
    }
}

impl Debug for EnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&self.tag.name)
            .field(&self.tag.value)
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct FieldValue {
    name: Ident,
    value: Value,
}

impl FieldValue {
    pub fn new(name: Ident, value: Value) -> FieldValue {
        FieldValue {
            name: name.into(),
            value,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn into_value(self) -> Value {
        self.value
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
            ScalarValue::Unit => TTypeId::Scalar(ScalarType::Unit),
            ScalarValue::Bool(_) => TTypeId::Scalar(ScalarType::Bool),
            ScalarValue::Int32(_) => TTypeId::Scalar(ScalarType::Int32),
            ScalarValue::String(_) => TTypeId::Scalar(ScalarType::String),
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
