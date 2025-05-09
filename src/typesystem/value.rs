use std::fmt::{Debug, Display};

use itertools::Itertools;

use super::{DuplicateField, NestedIdents, TypeError};


#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Value {
    Composite(CompositeValue),
    Scalar(ScalarValue),
}

impl Value {
    pub const UNIT: Value = Value::Composite(CompositeValue::UNIT);
    pub const TRUE: Value = Value::Scalar(ScalarValue::Bool(true));
    pub const FALSE: Value = Value::Scalar(ScalarValue::Bool(false));

    pub fn select(&self, idents: &[NestedIdents]) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => Ok(Value::Composite(value.select(idents)?)),
            Value::Scalar(value) => Ok(Value::Scalar(value.select(idents)?)),
        }
    }

    pub fn dot(&self, ident: &str) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => Ok(value.dot(ident)?),
            Value::Scalar(value) => Ok(value.dot(ident)?),
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
    pub const UNIT: CompositeValue = CompositeValue::Struct(StructValue::UNIT);
    
    pub fn new_struct(fields: Vec<FieldValue>) -> Result<CompositeValue, DuplicateField> {
        Ok(CompositeValue::Struct(StructValue::new(fields)?))
    }

    pub fn new_enum(tag: FieldValue) -> CompositeValue {
        CompositeValue::Enum(EnumValue::new(tag))
    }

    pub fn select(&self, idents: &[NestedIdents]) -> Result<CompositeValue, TypeError> {
        match self {
            CompositeValue::Struct(value) => Ok(CompositeValue::Struct(value.select(idents)?)),
            CompositeValue::Enum(value) => Ok(CompositeValue::Enum(value.select(idents)?)),
        }
    }

    pub fn dot(&self, ident: &str) -> Result<Value, TypeError> {
        match self {
            CompositeValue::Struct(value) => Ok(value.dot(ident)?),
            CompositeValue::Enum(value) => Ok(value.dot(ident)?),
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
    pub const UNIT: StructValue = StructValue { fields: Vec::new() };
    
    pub fn new(fields: Vec<FieldValue>) -> Result<StructValue, DuplicateField> {
        if let Some(_) = fields.iter().duplicates_by(|f| &f.name).next() {
            Err(DuplicateField)
        } else {
            Ok(StructValue {
                fields,
            })
        }
    }

    pub fn fields(&self) -> &[FieldValue] {
        &self.fields
    }

    pub fn select(&self, idents: &[NestedIdents]) -> Result<StructValue, TypeError> {
        if idents.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = Vec::new();
        
        for ident in idents {
            if let Some(field) = self.fields.iter().find(|f| f.name == ident.ident) {
                let new_field = FieldValue {
                    name: field.name.clone(),
                    value: field.value.select(&ident.children)?,
                };
                new_fields.push(new_field);
            } else {
                return Err(TypeError::UnknownField(ident.ident.clone()));
            }
        }
        
        Ok(StructValue {
            fields: new_fields,
        })
    }

    pub fn dot(&self, ident: &str) -> Result<Value, TypeError> {
        if let Some(field) = self.fields.iter().find(|f| &f.name == ident) {
            Ok(field.value.clone())
        } else {
            Err(TypeError::UnknownField(ident.into()))
        }
    }
}

impl Debug for StructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.fields.is_empty() {
            return write!(f, "()");
        }

        let mut d = f.debug_struct("(×)");
        
        for field in &self.fields {
            d.field(&field.name, &field.value);
        }
        
        d.finish()
    }
}

#[derive(Clone, PartialEq, Eq, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct EnumValue {
    tag: Box<FieldValue>,
}

impl PartialOrd for EnumValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.tag.value.partial_cmp(&other.tag.value)
    }
}

impl EnumValue {
    pub fn new(tag: FieldValue) -> EnumValue {
        EnumValue {
            tag: Box::new(tag),
        }
    }

    pub fn into_tag(self) -> FieldValue {
        *self.tag
    }

    pub fn tag(&self) -> &FieldValue {
        &self.tag
    }

    pub fn select(&self, idents: &[NestedIdents]) -> Result<EnumValue, TypeError> {
        if idents.is_empty() {
            return Ok(self.clone());
        }

        // tag must be listed
        if idents.iter().all(|ident| ident.ident != self.tag.name) {
            return Err(TypeError::MissingTag(self.tag.name.clone()));
        }

        // all idents must be valid
        for ident in idents {
            if self.tag.name != ident.ident {
                return Err(TypeError::UnknownTag(ident.ident.clone()));
            }
        }

        Ok(self.clone())
    }

    pub fn dot(&self, ident: &str) -> Result<Value, TypeError> {
        Err(TypeError::DotTag(ident.into()))
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
    name: String,
    value: Value,
}

impl FieldValue {
    pub fn new(name: impl Into<String>, value: Value) -> FieldValue {
        FieldValue {
            name: name.into(),
            value,
        }
    }

    pub fn name(&self) -> &str {
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
// #[binrw::binrw]
pub enum ScalarValue {
    // #[brw(magic(0u8))]
    Bool(
        // #[bw(map = |x| *x as u8)]
        // #[br(map = |x: u8| x > 0u8)]
        bool
    ),
    // #[brw(magic(1u8))]
    Int32(i32),
    // #[brw(magic(2u8))]
    String(String),
}

impl ScalarValue {
    pub fn select(&self, idents: &[NestedIdents]) -> Result<ScalarValue, TypeError> {
        if let Some(first) = idents.first() {
            Err(TypeError::ScalarField(first.ident.clone()))
        } else {
            Ok(self.clone())
        }
    }

    pub fn dot(&self, ident: &str) -> Result<Value, TypeError> {
        Err(TypeError::ScalarField(ident.into()))
    }
}

impl Debug for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(value) => Debug::fmt(value, f),
            Self::Int32(value) => Debug::fmt(value, f),
            Self::String(value) => Debug::fmt(value, f),
        }
    }
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScalarValue::Bool(value) => Debug::fmt(value, f),
            ScalarValue::Int32(value) => Debug::fmt(value, f),
            ScalarValue::String(value) => Debug::fmt(value, f),
        }
    }
}
