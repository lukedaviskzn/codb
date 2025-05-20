use std::{collections::BTreeMap, fmt::Debug};

use crate::{idents::{Ident, IdentPath}, typesystem::function::{FunctionArg, FunctionEntry, InterpreterFunction, InterpreterFunctionAction, UserFunction}};

use super::{Registry, TTypeId, TypeRegistry};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum FunctionRegistryError {
    #[error("function with name {0:?} already exists")]
    NameTaken(IdentPath),
    #[error("function with name {0:?} could not be found")]
    NotFound(IdentPath),
}

pub struct FunctionRegistry {
    core_functions: BTreeMap<IdentPath, InterpreterFunction>,
    functions: BTreeMap<IdentPath, UserFunction>,
}

impl FunctionRegistry {
    pub fn new() -> FunctionRegistry {
        let function_panic = InterpreterFunction::new(
            vec![
                FunctionArg::new("string".parse().expect("not an ident"), TTypeId::STRING),
            ],
            TTypeId::NEVER,
            InterpreterFunctionAction::Panic,
        ).expect("could not create function `panic`");
        
        let function_registry = FunctionRegistry {
            core_functions: btreemap! {
                "core::panic".parse().expect("not an ident") => function_panic,
            },
            functions: btreemap! {},
        };

        function_registry
    }

    pub fn functions(&self) -> &BTreeMap<IdentPath, UserFunction> {
        &self.functions
    }

    pub fn get(&self, ident: &IdentPath) -> Result<FunctionEntry, FunctionRegistryError> {
        self.core_functions.get(ident)
            .map(|function| FunctionEntry::InterpreterFunction(function))
            .or_else(|| {
                self.functions.get(ident)
                    .map(|function| FunctionEntry::UserFunction(function))
            })
            .ok_or_else(|| FunctionRegistryError::NotFound(ident.clone()))
    }

    pub fn add(&mut self, name: IdentPath, function: UserFunction) -> Result<UserFunction, FunctionRegistryError> {
        if self.core_functions.contains_key(&name) || self.functions.contains_key(&name) {
            return Err(FunctionRegistryError::NameTaken(name));
        }

        let None = self.functions.insert(name, function.clone()) else {
            unreachable!();
        };

        Ok(function)
    }
}
