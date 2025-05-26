use std::fmt::Debug;

use codb_core::{Ident, IdentPath};
use module::Module;

mod module;

use crate::{expression::{Branch, ControlFlow, Expression, InterpreterAction, MatchControlFlow}, typesystem::{function::{Function, FunctionArg}, ttype::{ArrayType, CompositeType, EnumType, ScalarType, TType}, value::Value, TypeError}};

use super::{relation::Relation, DbRelations};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum TTypeId {
    Scalar(ScalarType),
    Composite(CompositeTTypeId),
    Array(Box<ArrayType>),
}

impl TTypeId {
    pub fn new_anonymous(ttype: TType) -> TTypeId {
        match ttype {
            TType::Composite(composite_type) => TTypeId::Composite(CompositeTTypeId::Anonymous(Box::new(composite_type))),
            TType::Scalar(scalar_type) => TTypeId::Scalar(scalar_type),
            TType::Array(array_type) => TTypeId::Array(Box::new(array_type)),
        }
    }

    pub fn compatible_with(&self, expected: &TTypeId) -> bool {
        match self {
            this if this == expected => true,
            this if this == &TTypeId::NEVER => true,
            TTypeId::Array(this) => if let TTypeId::Array(expected) = expected {
                this.inner_ttype_id().compatible_with(expected.inner_ttype_id())
                &&
                if expected.length().is_some() {
                    this.length() == expected.length()
                } else {
                    true
                }
            } else {
                false
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

impl TTypeId {
    pub const NEVER: TTypeId = TTypeId::Scalar(ScalarType::Never);
    pub const UNIT: TTypeId = TTypeId::Scalar(ScalarType::Unit);
    pub const BOOL: TTypeId = TTypeId::Scalar(ScalarType::Bool);
    pub const INT32: TTypeId = TTypeId::Scalar(ScalarType::Int32);
    pub const INT64: TTypeId = TTypeId::Scalar(ScalarType::Int64);
    pub const STRING: TTypeId = TTypeId::Scalar(ScalarType::String);
}

impl Debug for TTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(ttype) => Debug::fmt(ttype, f),
            Self::Composite(ttype) => Debug::fmt(ttype, f),
            Self::Array(ttype) => Debug::fmt(ttype, f),
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

impl From<ArrayType> for TTypeId {
    fn from(ttype: ArrayType) -> Self {
        TTypeId::Array(Box::new(ttype))
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum CompositeTTypeId {
    Path(IdentPath),
    Anonymous(Box<CompositeType>),
}

impl Debug for CompositeTTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Path(path) => Debug::fmt(path, f),
            Self::Anonymous(ttype) => {
                write!(f, "anon[")?;
                Debug::fmt(ttype, f)?;
                write!(f, "]")
            },
        }
    }
}

impl From<IdentPath> for CompositeTTypeId {
    fn from(path: IdentPath) -> Self {
        CompositeTTypeId::Path(path)
    }
}

pub struct Registry {
    root: Module,
}

impl Registry {
    pub fn new<R: Relation>(relations: &DbRelations<R>) -> Registry {
        let mut registry = Registry {
            root: Module::new(),
        };

        // Result<(), String>
        registry.root
            .add(id!("Result"), EnumType::new(btreemap! {
                id!("Ok") => TTypeId::UNIT,
                id!("Err") => TTypeId::STRING,
            }))
            .expect("Result ident is already taken");

        // unwrap(result)
        registry.root
            .add(id!("unwrap"), Function::new(
                &registry,
                relations,
                [FunctionArg::new(id!("result"), id_path!("Result").into())],
                TTypeId::UNIT,
                Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
                    param: Expression::NestedIdent(id!("result").into()),
                    ret_type: TTypeId::UNIT,
                    branches: btreemap! {
                        id!("Ok") => Branch::new(id!("_"), Expression::Value(Value::UNIT)),
                        id!("Err") => Branch::new(id!("error"), Expression::Action(
                            InterpreterAction::Panic { message: Box::new(Expression::NestedIdent(id!("error").into())) }
                        )),
                    },
                }))),
            ).expect("failed to type check `unwrap`"))
            .expect("`unwrap` ident is already taken");

        registry
    }

    pub fn module(&self, path: &[Ident]) -> Option<&Module> {
        let mut module = &self.root;

        for ident in path {
            module = module.module(ident)?;
        }

        Some(module)
    }

    #[allow(unused)]
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
                    module.ttype(ttype_name).cloned()
                },
            },
            TTypeId::Array(array_type) => Some((**array_type).clone().into()),
        }
    }

    pub fn function(&self, path: &IdentPath) -> Option<&Function> {
        let (function_name, mod_path) = path.split_last();

        let module = self.module(mod_path)?;
        module.function(function_name)
    }
}

// let mut registry = Registry {
//     types: TypeRegistry::new(),
//     functions: FunctionRegistry::new(),
// };

// let result_ttype_id = registry.types().get_id_by_name("Result").expect("no result type");

// let function_unwrap = UserFunction::new(
//     &registry,
//     vec![
//         FunctionArg::new(id!("value"), result_ttype_id),
//     ],
//     TTypeId::UNIT,
//     Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
//         param: Expression::NestedIdent(id!("value").into()),
//         ret_type: TTypeId::UNIT,
//         branches: btreemap! {
//             id!("Ok") => Branch::new(
//                 id!("_"),
//                 Expression::Value(Value::UNIT),
//             ),
//             id!("Err") => Branch::new(
//                 id!("_"),
//                 Expression::FunctionInvocation(FunctionInvocation::new(
//                     id_path!("core::panic"),
//                     [
//                         Expression::Value(
//                             ScalarValue::new(registry.types(), TTypeId::STRING, ScalarValueInner::String("".into())).expect("not a string").into(),
//                         )
//                     ],
//                 )),
//             ),
//         },
//     })))
// ).expect("could not create function `unwrap`");

// registry.functions_mut().add(id_path!("core::unwrap"), function_unwrap).expect("failed to add unwrap function");

// registry