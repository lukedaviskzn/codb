use std::fmt::Debug;

use itertools::Itertools;

use crate::{expr::Expression, typesystem::value::ScalarValue};

use super::{registry::{TTypeId, TypeRegistry}, value::{CompositeValue, EnumValue, FieldValue, StructValue, Value}, DuplicateField, NestedIdents, RefinementFailedError, TypeError};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum TType {
    Refined(RefinedTType),
    Composite(CompositeType),
    Scalar(ScalarType),
}

impl Debug for TType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Refined(ttype) => Debug::fmt(ttype, f),
            Self::Composite(ttype) => Debug::fmt(ttype, f),
            Self::Scalar(ttype) => Debug::fmt(ttype, f),
        }
    }
}

impl TType {
    pub const UNIT: TType = TType::Scalar(ScalarType::Unit);
    pub const BOOL: TType = TType::Scalar(ScalarType::Bool);
    pub const INT32: TType = TType::Scalar(ScalarType::Int32);
    pub const STRING: TType = TType::Scalar(ScalarType::String);

    pub fn select(&self, registry: &TypeRegistry, idents: &[NestedIdents]) -> Result<TType, TypeError> {
        match self {
            TType::Refined(ttype) => ttype.select(registry, idents),
            TType::Composite(ttype) => Ok(TType::Composite(ttype.select(registry, idents)?)),
            TType::Scalar(ttype) => Ok(TType::Scalar(ttype.select(idents)?)),
        }
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &str) -> Result<TType, TypeError> {
        match self {
            TType::Refined(ttype) => ttype.dot(registry, ident),
            TType::Composite(ttype) => ttype.dot(registry, ident),
            TType::Scalar(ttype) => ttype.dot(ident),
        }
    }

    pub fn check(&self, registry: &TypeRegistry, value: &Value) -> Result<(), TypeError> {
        match self {
            TType::Refined(ttype) => ttype.check(registry, value),
            TType::Composite(ttype) => if let Value::Composite(value) = value {
                ttype.check(registry, value)
            } else {
                Err(TypeError::ValueTypeInvalid {
                    expected: TType::Composite(ttype.clone()),
                    got: value.clone(),
                })
            },
            TType::Scalar(ttype) => if let Value::Scalar(value) = value {
                ttype.check(value)
            } else {
                Err(TypeError::ValueTypeInvalid {
                    expected: TType::Scalar(ttype.clone()),
                    got: value.clone(),
                })
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct RefinedTType {
    ttype_id: TTypeId,
    refinement: Box<Expression>,
}

impl RefinedTType {
    pub const RESULT_TYPE_NAME: &str = "RefinementResult";
    pub const RESULT_TYPE_OK: &str = "Ok";
    pub const RESULT_TYPE_ERR: &str = "Err";

    pub fn result_ok(registry: &TypeRegistry) -> Value {
        let ttype_id = registry.get_id_by_name(Self::RESULT_TYPE_NAME).expect("unreachable");

        Value::Composite(CompositeValue::Enum(EnumValue::new(registry, &ttype_id,
            FieldValue::new(Self::RESULT_TYPE_OK, Value::UNIT)
        ).expect("unreachable")))
    }

    pub fn new(registry: &TypeRegistry, ttype_id: TTypeId, refinement: Expression) -> Result<RefinedTType, TypeError> {
        let refinement_ttype = refinement.eval_types(registry, &StructType {
            fields: vec![
                FieldType::new("this", ttype_id.clone()),
            ],
        })?;

        let result_ttype = registry.get_by_name(Self::RESULT_TYPE_NAME)?;

        if result_ttype != refinement_ttype {
            return Err(TypeError::TypeInvalid { expected: result_ttype, got: refinement_ttype });
        }

        Ok(RefinedTType {
            ttype_id: ttype_id,
            refinement: Box::new(refinement),
        })
    }

    pub fn select(&self, registry: &TypeRegistry, idents: &[NestedIdents]) -> Result<TType, TypeError> {
        registry.get_by_id(&self.ttype_id)?.select(registry, idents)
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &str) -> Result<TType, TypeError> {
        registry.get_by_id(&self.ttype_id)?.dot(registry, ident)
    }

    pub fn check(&self, registry: &TypeRegistry, value: &Value) -> Result<(), TypeError> {
        registry.get_by_id(&self.ttype_id)?.check(registry, value)?;

        let variables_ttype = TTypeId::Anonymous(Box::new(TType::Composite(CompositeType::Struct(StructType {
            fields: vec![
                FieldType::new("this", value.ttype_id()),
            ],
        }))));

        let expr = self.refinement.eval(registry, &StructValue::new(registry, &variables_ttype, vec![
            FieldValue::new("this", value.clone()),
        ]).expect("unreachable")).map_err(|err| RefinementFailedError::Expr(Box::new(err)))?;

        let result_ttype = registry.get_by_name(Self::RESULT_TYPE_NAME)?;

        result_ttype.check(registry, &expr)?;

        if expr != Self::result_ok(registry) {
            let Value::Composite(CompositeValue::Enum(expr)) = expr else { unreachable!() };
            let Value::Scalar(ScalarValue::String(string)) = expr.into_tag().into_value() else { unreachable!() };

            return Err(TypeError::RefinementFailed(RefinementFailedError::Refinement(string)))
        }

        Ok(())
    }
}

impl Debug for RefinedTType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.ttype_id, f)?;
        write!(f, " {{ {:?} }}", &self.refinement)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum CompositeType {
    Struct(StructType),
    Enum(EnumType),
}

impl Debug for CompositeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(ttype) => Debug::fmt(ttype, f),
            Self::Enum(ttype) => Debug::fmt(ttype, f),
        }
    }
}

impl CompositeType {
    pub fn new_struct(fields: Vec<FieldType>) -> Result<CompositeType, DuplicateField> {
        Ok(CompositeType::Struct(StructType::new(fields)?))
    }

    pub fn new_enum(tags: Vec<FieldType>) -> Result<CompositeType, DuplicateField> {
        Ok(CompositeType::Enum(EnumType::new(tags)?))
    }

    pub fn select(&self, registry: &TypeRegistry, idents: &[NestedIdents]) -> Result<CompositeType, TypeError> {
        match self {
            CompositeType::Struct(ttype) => Ok(CompositeType::Struct(ttype.select(registry, idents)?)),
            CompositeType::Enum(ttype) => Ok(CompositeType::Enum(ttype.select(idents)?)),
        }
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &str) -> Result<TType, TypeError> {
        match self {
            CompositeType::Struct(ttype) => ttype.dot(registry, ident),
            CompositeType::Enum(ttype) => ttype.dot(ident),
        }
    }

    pub fn check(&self, registry: &TypeRegistry, value: &CompositeValue) -> Result<(), TypeError> {
        match self {
            CompositeType::Struct(ttype) => if let CompositeValue::Struct(value) = value {
                ttype.check(registry, value)
            } else {
                Err(TypeError::ValueTypeInvalid {
                    expected: TType::Composite(CompositeType::Struct(ttype.clone())),
                    got: Value::Composite(value.clone()),
                })
            },
            CompositeType::Enum(ttype) => if let CompositeValue::Enum(value) = value {
                ttype.check(registry, value)
            } else {
                Err(TypeError::ValueTypeInvalid {
                    expected: TType::Composite(CompositeType::Enum(ttype.clone())),
                    got: Value::Composite(value.clone()),
                })
            },
        }
    }
}

#[derive(Clone, Eq, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct StructType {
    fields: Vec<FieldType>,
}

impl Debug for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("(×)");
        for field in &self.fields {
            d.field(&field.name, &field.ttype_id);
        }
        d.finish()
    }
}

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(std::cmp::Ordering::Equal)
    }
}

impl PartialOrd for StructType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.fields.iter().sorted().map(|f| &f.ttype_id).partial_cmp(other.fields.iter().sorted().map(|f| &f.ttype_id))
    }
}

impl StructType {
    pub fn new(fields: Vec<FieldType>) -> Result<StructType, DuplicateField> {
        if let Some(_) = fields.iter().duplicates_by(|f| &f.name).next() {
            Err(DuplicateField)
        } else {
            Ok(StructType {
                fields,
            })
        }
    }

    pub fn fields(&self) -> &[FieldType] {
        &self.fields
    }

    pub fn select(&self, registry: &TypeRegistry, idents: &[NestedIdents]) -> Result<StructType, TypeError> {
        if idents.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = Vec::new();
        
        for ident in idents {
            if let Some(field) = self.fields.iter().find(|f| f.name == ident.ident) {
                let field_type = registry.get_by_id(&field.ttype_id)?;

                let new_field = FieldType {
                    name: field.name.clone(),
                    ttype_id: TTypeId::Anonymous(Box::new(field_type.select(registry, &ident.children)?)),
                };
                new_fields.push(new_field);
            } else {
                return Err(TypeError::UnknownField(ident.ident.clone()));
            }
        }
        
        Ok(StructType {
            fields: new_fields,
        })
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &str) -> Result<TType, TypeError> {
        if let Some(field) = self.fields.iter().find(|f| f.name == ident) {
            let ttype = registry.get_by_id(&field.ttype_id)?;
            Ok(ttype)
        } else {
            Err(TypeError::MissingField(ident.into()))
        }
    }

    pub fn check(&self, registry: &TypeRegistry, value: &StructValue) -> Result<(), TypeError> {
        for v_field in value.fields() {
            if let Some(field) = self.fields.iter().find(|field| field.name == v_field.name()) {
                let ttype = registry.get_by_id(&field.ttype_id)?;
                ttype.check(registry, v_field.value())?;
            } else {
                return Err(TypeError::UnknownField(v_field.name().into()));
            }
        }

        for field in &self.fields {
            if value.fields().iter().all(|v_field| v_field.name() != field.name) {
                return Err(TypeError::MissingField(field.name.clone()));
            }
        }

        if value.fields().len() != self.fields.len() {
            Err(DuplicateField)?;
        }

        Ok(())
    }
}

#[derive(Clone, Eq, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct EnumType {
    tags: Vec<FieldType>,
}

impl Debug for EnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("(+)");
        for tag in &self.tags {
            d.field(&tag.name, &tag.ttype_id);
        }
        d.finish()
    }
}

impl PartialEq for EnumType {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(std::cmp::Ordering::Equal)
    }
}

impl PartialOrd for EnumType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.tags.iter().sorted().map(|f| &f.ttype_id).partial_cmp(other.tags.iter().sorted().map(|f| &f.ttype_id))
    }
}

impl EnumType {
    pub fn new(tags: Vec<FieldType>) -> Result<EnumType, DuplicateField> {
        if let Some(_) = tags.iter().duplicates_by(|f| &f.name).next() {
            Err(DuplicateField)
        } else {
            Ok(EnumType {
                tags,
            })
        }
    }

    pub fn tags(&self) -> &[FieldType] {
        &self.tags
    }

    pub fn select(&self, idents: &[NestedIdents]) -> Result<EnumType, TypeError> {
        if idents.is_empty() {
            return Ok(self.clone());
        }

        // all tags must be listed
        for tag in &self.tags {
            if idents.iter().all(|ident| ident.ident != tag.name) {
                return Err(TypeError::MissingTag(tag.name.clone()));
            }
        }

        // all idents must be valid
        for ident in idents {
            if self.tags.iter().all(|tag| tag.name != ident.ident) {
                return Err(TypeError::UnknownTag(ident.ident.clone()));
            }
        }

        if idents.len() != self.tags.len() {
            Err(DuplicateField)?;
        }

        Ok(self.clone())
    }

    pub fn dot(&self, ident: &str) -> Result<TType, TypeError> {
        return Err(TypeError::DotTag(ident.into()))
    }

    pub fn check(&self, registry: &TypeRegistry, value: &EnumValue) -> Result<(), TypeError> {
        if let Some(tag) = self.tags.iter().find(|tag| tag.name == value.tag().name()) {
            let ttype = registry.get_by_id(&tag.ttype_id)?;
            ttype.check(registry, value.tag().value())
        } else {
            Err(TypeError::UnknownTag(value.tag().name().into()))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct FieldType {
    name: String,
    ttype_id: TTypeId,
}

impl FieldType {
    pub fn new(name: impl Into<String>, ttype_id: TTypeId) -> FieldType {
        FieldType {
            name: name.into(),
            ttype_id,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn ttype_id(&self) -> &TTypeId {
        &self.ttype_id
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScalarType {
    Unit,
    Bool,
    Int32,
    String,
}

impl ScalarType {
    pub const ALL: [ScalarType; 4] = [ScalarType::Unit, ScalarType::Bool, ScalarType::Int32, ScalarType::String];

    pub fn name(&self) -> &str {
        match self {
            ScalarType::Unit => "()",
            ScalarType::Bool => "bool",
            ScalarType::Int32 => "int32",
            ScalarType::String => "string",
        }
    }

    pub fn select(&self, idents: &[NestedIdents]) -> Result<ScalarType, TypeError> {
        if let Some(first) = idents.first() {
            Err(TypeError::ScalarField(first.ident.clone()))
        } else {
            Ok(self.clone())
        }
    }

    pub fn dot(&self, ident: &str) -> Result<TType, TypeError> {
        Err(TypeError::ScalarField(ident.into()))
    }

    pub fn check(&self, value: &ScalarValue) -> Result<(), TypeError> {
        match value {
            ScalarValue::Unit if ScalarType::Unit == *self => Ok(()),
            ScalarValue::Bool(_) if ScalarType::Bool == *self => Ok(()),
            ScalarValue::Int32(_) if ScalarType::Int32 == *self => Ok(()),
            ScalarValue::String(_) if ScalarType::String == *self => Ok(()),
            _ => Err(TypeError::ValueTypeInvalid {
                expected: TType::Scalar(self.clone()),
                got: Value::Scalar(value.clone()),
            }),
        }
    }
}

impl Debug for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
