use std::{collections::BTreeMap, fmt::Debug};

use codb_core::Ident;

use crate::{typesystem::{function::Function, ttype::CompositeType}};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ModuleItem {
    #[brw(magic = 0u8)]
    Module(Module),
    #[brw(magic = 1u8)]
    TType(CompositeType),
    #[brw(magic = 2u8)]
    Function(Function),
}

impl From<Module> for ModuleItem {
    fn from(value: Module) -> Self {
        Self::Module(value)
    }
}

impl From<Function> for ModuleItem {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}

impl<T: Into<CompositeType>> From<T> for ModuleItem {
    fn from(value: T) -> Self {
        Self::TType(value.into())
    }
}

impl Debug for ModuleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Module(module) => Debug::fmt(module, f),
            Self::TType(ttype) => Debug::fmt(ttype, f),
            Self::Function(function) => Debug::fmt(function, f),
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Module {
    #[bw(calc = self.items.len() as u64)]
    len: u64,
    #[br(count = len, map = |items: Vec<(Ident, ModuleItem)>| BTreeMap::from_iter(items.into_iter()))]
    #[bw(map = |items| Vec::<(Ident, ModuleItem)>::from_iter(items.clone().into_iter()))]
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

    pub fn ttype(&self, ident: &Ident) -> Option<&CompositeType> {
        if let ModuleItem::TType(ttype) = self.items.get(ident)? {
            Some(ttype)
        } else {
            None
        }
    }

    pub fn function(&self, ident: &Ident) -> Option<&Function> {
        if let ModuleItem::Function(function) = self.items.get(ident)? {
            Some(function)
        } else {
            None
        }
    }

    /// Add an item to the module, returns true if successful, false if already exists.
    pub fn insert(&mut self, ident: Ident, item: impl Into<ModuleItem>) -> bool {
        if self.items.contains_key(&ident) {
            false
        } else {
            self.items.insert(ident, item.into());
            true
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
