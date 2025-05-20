use std::{collections::BTreeMap, fmt::Debug};

use codb_core::Ident;

use crate::{error::IdentTaken, typesystem::{function::{FunctionEntry, InterpreterFunction, UserFunction}, ttype::TType}};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ModuleItem {
    Module(Module),
    TType(TType),
    UserFunction(UserFunction),
    InterpreterFunction(InterpreterFunction),
}

impl From<Module> for ModuleItem {
    fn from(value: Module) -> Self {
        Self::Module(value)
    }
}

impl From<UserFunction> for ModuleItem {
    fn from(value: UserFunction) -> Self {
        Self::UserFunction(value)
    }
}

impl<T: Into<TType>> From<T> for ModuleItem {
    fn from(value: T) -> Self {
        Self::TType(value.into())
    }
}

impl Debug for ModuleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Module(module) => Debug::fmt(module, f),
            Self::TType(ttype) => Debug::fmt(ttype, f),
            Self::UserFunction(function) => Debug::fmt(function, f),
            Self::InterpreterFunction(function) => Debug::fmt(function, f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Module {
    items: BTreeMap<Ident, ModuleItem>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            items: btreemap! {},
        }
    }

    pub fn item(&self, ident: &Ident) -> Option<&ModuleItem> {
        self.items.get(ident)
    }

    pub fn module(&self, ident: &Ident) -> Option<&Module> {
        if let ModuleItem::Module(module) = self.items.get(ident)? {
            Some(module)
        } else {
            None
        }
    }

    pub fn module_mut(&mut self, ident: &Ident) -> Option<&mut Module> {
        if let ModuleItem::Module(module) = self.items.get_mut(ident)? {
            Some(module)
        } else {
            None
        }
    }

    pub fn ttype(&self, ident: &Ident) -> Option<&TType> {
        if let ModuleItem::TType(ttype) = self.items.get(ident)? {
            Some(ttype)
        } else {
            None
        }
    }

    pub fn function(&self, ident: &Ident) -> Option<FunctionEntry> {
        match self.items.get(ident)? {
            ModuleItem::UserFunction(function) => Some(FunctionEntry::UserFunction(function)),
            ModuleItem::InterpreterFunction(function) => Some(FunctionEntry::InterpreterFunction(function)),
            _ => None,
        }
    }

    /// Add an item to the module, returns true if successful, false if already exists.
    pub fn add(&mut self, ident: Ident, item: impl Into<ModuleItem>) -> Result<(), IdentTaken> {
        if self.items.contains_key(&ident) {
            Err(IdentTaken(ident))
        } else {
            self.items.insert(ident, item.into());
            Ok(())
        }
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();
        for (name, item) in &self.items {
            map.entry(name, item);
        }
        map.finish()
    }
}
