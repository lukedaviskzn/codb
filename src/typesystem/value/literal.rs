use std::{collections::BTreeMap, fmt::Debug};

use codb_core::Ident;

use crate::{typesystem::ttype::{CompositeType, ScalarType}};

#[derive(Clone, PartialEq, Eq)]
pub enum LiteralType {
    Name(String),
    Anonymous(CompositeType),
}

impl Debug for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(name) => Debug::fmt(name, f),
            Self::Anonymous(ttype) => Debug::fmt(ttype, f),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Literal {
    Composite(CompositeLiteral),
    Scalar(ScalarLiteral),
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(val) => val.fmt(f),
            Self::Scalar(val) => Debug::fmt(val, f),
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

impl From<ScalarLiteral> for Literal {
    fn from(value: ScalarLiteral) -> Self {
        Self::Scalar(value)
    }
}

impl From<ScalarLiteralInner> for Literal {
    fn from(value: ScalarLiteralInner) -> Self {
        Self::Scalar(value.into())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum CompositeLiteral {
    Struct(StructLiteral),
    Enum(EnumLiteral),
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
        Self::Struct(value)
    }
}

impl From<EnumLiteral> for CompositeLiteral {
    fn from(value: EnumLiteral) -> Self {
        Self::Enum(value)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct StructLiteral {
    pub ttype: LiteralType,
    pub fields: BTreeMap<Ident, Literal>,
}

impl Debug for StructLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct(&format!("{:?}", self.ttype));
        
        for (name, value) in &self.fields {
            d.field(name, value);
        }
        
        d.finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct EnumLiteral {
    pub ttype: LiteralType,
    pub tag: Ident,
    pub value: Box<Literal>,
}

impl Debug for EnumLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("{:?}::{:?}", self.ttype, self.tag))
            .field(&self.value)
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct ScalarLiteral {
    pub ttype: LiteralType,
    pub inner: ScalarLiteralInner,
}

impl Debug for ScalarLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?})", &self.ttype)?;
        Debug::fmt(&self.inner, f)
    }
}

impl From<ScalarLiteralInner> for ScalarLiteral {
    fn from(value: ScalarLiteralInner) -> Self {
        Self {
            ttype: match &value {
                ScalarLiteralInner::Unit => LiteralType::Name(ScalarType::Unit.name().into()),
                ScalarLiteralInner::Bool(_) => LiteralType::Name(ScalarType::Bool.name().into()),
                ScalarLiteralInner::Int32(_) => LiteralType::Name(ScalarType::Int32.name().into()),
                ScalarLiteralInner::String(_) => LiteralType::Name(ScalarType::String.name().into()),
            },
            inner: value,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ScalarLiteralInner {
    Unit,
    Bool(bool),
    Int32(i32),
    String(String),
}

impl Debug for ScalarLiteralInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(value) => Debug::fmt(value, f),
            Self::Int32(value) => Debug::fmt(value, f),
            Self::String(value) => Debug::fmt(value, f),
        }
    }
}
