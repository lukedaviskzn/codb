use std::{fmt::Debug, sync::{Arc, Mutex}};

use codb_core::{Ident, IdentPath};
use module::Module;

pub mod module;

use crate::{expression::{Branch, ControlFlow, Expression, InterpreterAction, Literal, MatchControlFlow}, typesystem::{function::Function, ttype::{CompositeType, EnumType, ScalarType, TType}, TypeError}};

use super::{pager::Pager, DbRelationSet};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TTypeId {
    #[brw(magic = 0u8)]
    Scalar(ScalarType),
    #[brw(magic = 1u8)]
    Composite(CompositeTTypeId),
}

impl TTypeId {
    pub const NEVER: TTypeId = TTypeId::Scalar(ScalarType::Never);
    pub const UNIT: TTypeId = TTypeId::Scalar(ScalarType::Unit);
    pub const BOOL: TTypeId = TTypeId::Scalar(ScalarType::Bool);
    pub const INT32: TTypeId = TTypeId::Scalar(ScalarType::Int32);
    pub const INT64: TTypeId = TTypeId::Scalar(ScalarType::Int64);
    pub const STRING: TTypeId = TTypeId::Scalar(ScalarType::String);
    
    pub fn new_anonymous(ttype: TType) -> TTypeId {
        match ttype {
            TType::Composite(composite_type) => TTypeId::Composite(CompositeTTypeId::Anonymous(Box::new(composite_type))),
            TType::Scalar(scalar_type) => TTypeId::Scalar(scalar_type),
        }
    }

    pub fn compatible_with(&self, expected: &TTypeId) -> bool {
        match self {
            this if this == expected => true,
            this if this == &TTypeId::NEVER => true,
            TTypeId::Composite(CompositeTTypeId::Anonymous(ttype)) => {
                match &**ttype {
                    CompositeType::Array(this) => {
                        match expected {
                            TTypeId::Composite(CompositeTTypeId::Anonymous(expected)) => {
                                match &**expected {
                                    CompositeType::Array(expected) => {
                                        this.inner_ttype_id().compatible_with(expected.inner_ttype_id())
                                        &&
                                        if expected.length().is_some() {
                                            this.length() == expected.length()
                                        } else {
                                            true
                                        }
                                    },
                                    _ => false, 
                                }
                            },
                            _ => false
                        }
                    },
                    _ => false,
                }
            },
            _ => false,
        }
    }

    pub fn must_eq(&self, expected: &TTypeId) -> Result<(), TypeError> {
        if self.compatible_with(expected) {
            Ok(())
        } else {
            Err(TypeError::TypeIdInvalid {
                expected: expected.clone(),
                got: self.clone(),
            })
        }
    }
}

impl Debug for TTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(ttype) => Debug::fmt(ttype, f),
            Self::Composite(ttype) => Debug::fmt(ttype, f),
        }
    }
}

impl From<ScalarType> for TTypeId {
    fn from(ttype: ScalarType) -> Self {
        TTypeId::Scalar(ttype)
    }
}

impl From<CompositeTTypeId> for TTypeId {
    fn from(ttype: CompositeTTypeId) -> Self {
        TTypeId::Composite(ttype)
    }
}

impl From<IdentPath> for TTypeId {
    fn from(path: IdentPath) -> Self {
        TTypeId::Composite(path.into())
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CompositeTTypeId {
    #[brw(magic = 0u8)]
    Path(IdentPath),
    #[brw(magic = 1u8)]
    Anonymous(Box<CompositeType>),
}

impl Debug for CompositeTTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Path(path) => Debug::fmt(path, f),
            Self::Anonymous(ttype) => Debug::fmt(ttype, f),
        }
    }
}

impl From<IdentPath> for CompositeTTypeId {
    fn from(path: IdentPath) -> Self {
        CompositeTTypeId::Path(path)
    }
}

#[binrw]
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Registry {
    root: Module,
}

impl Registry {
    pub fn new(pager: Arc<Mutex<Pager>>, relations: &DbRelationSet) -> Registry {
        let mut registry = Registry {
            root: Module::new(),
        };

        registry.root
            .insert(id!("Result"), EnumType::new(indexmap! {
                id!("Ok") => TTypeId::UNIT,
                id!("Err") => TTypeId::STRING,
            }));

        registry.root
            .insert(id!("unwrap"), Function::new(
                pager,
                &registry,
                relations,
                indexmap! {
                    id!("result") => id_path!("Result").into(),
                },
                TTypeId::UNIT,
                Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
                    param: Expression::NestedIdent(id!("result").into()),
                    ret_type: TTypeId::UNIT,
                    branches: btreemap! {
                        id!("Ok") => Branch::new(id!("_"), Expression::Literal(Literal::UNIT)),
                        id!("Err") => Branch::new(id!("error"), Expression::Action(
                            InterpreterAction::Panic { message: Box::new(Expression::NestedIdent(id!("error").into())) }
                        )),
                    },
                }))),
            ).expect("failed to type check `unwrap`"));

        registry
    }

    pub fn module(&self, path: &[Ident]) -> Option<&Module> {
        let mut module = &self.root;

        for ident in path {
            module = module.module(ident)?;
        }

        Some(module)
    }

    pub fn module_mut(&mut self, path: &[Ident]) -> Option<&mut Module> {
        let mut module = &mut self.root;

        for ident in path {
            module = module.module_mut(ident)?;
        }

        Some(module)
    }

    pub fn ttype(&self, id: &TTypeId) -> Option<TType> {
        match id {
            TTypeId::Scalar(scalar_type) => Some(scalar_type.clone().into()),
            TTypeId::Composite(complex_type) => match complex_type {
                CompositeTTypeId::Anonymous(composite_type) => Some((*composite_type.clone()).into()),
                CompositeTTypeId::Path(path) => {
                    let (ttype_name, mod_path) = path.split_last();

                    let module = self.module(mod_path)?;
                    module.ttype(ttype_name).cloned().map(|ttype| ttype.into())
                },
            },
        }
    }

    pub fn function(&self, path: &IdentPath) -> Option<&Function> {
        let (function_name, mod_path) = path.split_last();

        let module = self.module(mod_path)?;
        module.function(function_name)
    }
}
