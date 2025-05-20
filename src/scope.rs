use std::borrow::Cow;

use codb_core::{Ident, NestedIdent};

use crate::{db::registry::{CompositeTTypeId, Registry, TTypeId}, typesystem::{ttype::{CompositeType, StructType}, value::{StructValue, Value}, TypeError}};

#[derive(Debug, Clone, Default)]
pub struct ScopeTypes<'a> {
    scopes: Vec<Cow<'a, StructType>>,
}

impl<'a> ScopeTypes<'a> {
    pub const EMPTY: ScopeTypes<'static> = ScopeTypes {
        scopes: Vec::new(),
    };

    pub const fn new() -> Self {
        Self::EMPTY
    }

    pub fn one(scope: Cow<'a, StructType>) -> Self {
        Self {
            scopes: vec![scope],
        }
    }

    pub fn push(&mut self, scope: Cow<'a, StructType>) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) -> Option<Cow<'a, StructType>> {
        self.scopes.pop()
    }

    pub fn get(&self, ident: &Ident) -> Option<&TTypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(ttype) = scope.fields().get(ident) {
                return Some(ttype);
            }
        }
        None
    }

    pub fn get_nested(&self, registry: &Registry, nested_ident: &NestedIdent) -> Result<TTypeId, TypeError> {
        let first = nested_ident.first();
        
        let mut ttype_id = None;

        for scope in self.scopes.iter().rev() {
            if let Some(scope_ttype_id) = scope.fields().get(first) {
                ttype_id = Some(scope_ttype_id);
            }
        }

        let ttype_id = ttype_id.unwrap(); // todo: fix unwrap
        let mut ttype_id = ttype_id.clone();

        let mut ident_list = nested_ident.iter();
        ident_list.next();
        
        for ident in ident_list {
            ttype_id = registry.ttype(&ttype_id)
                .ok_or_else(|| TypeError::TypeNotFound(ttype_id.clone()))?
                .dot(ident).unwrap() // todo: fix unwrap
                .clone();
        }

        Ok(ttype_id.clone())
    }
}

#[derive(Debug, Clone, Default)]
pub struct ScopeValues<'a> {
    scopes: Vec<Cow<'a, StructValue>>
}

impl<'a> ScopeValues<'a> {
    pub const EMPTY: ScopeValues<'static> = ScopeValues {
        scopes: Vec::new(),
    };
    
    pub const fn new() -> Self {
        Self::EMPTY
    }

    pub fn one(scope: Cow<'a, StructValue>) -> Self {
        Self {
            scopes: vec![scope],
        }
    }

    pub fn push(&mut self, scope: Cow<'a, StructValue>) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) -> Option<Cow<'a, StructValue>> {
        self.scopes.pop()
    }

    pub fn get(&self, ident: &Ident) -> Result<&Value, TypeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(ttype) = scope.fields().get(ident) {
                return Ok(ttype);
            }
        }
        Err(TypeError::UnknownField(ident.clone()))
    }

    pub fn get_nested(&self, nested_ident: &NestedIdent) -> Result<Value, TypeError> {
        let first = nested_ident.first();
        
        let mut ttype_id = None;

        for scope in self.scopes.iter().rev() {
            if let Some(scope_ttype_id) = scope.fields().get(first) {
                ttype_id = Some(scope_ttype_id);
            }
        }

        let Some(value) = ttype_id else {
            return Err(TypeError::UnknownField(first.clone()));
        };
        let mut value = value.clone();

        let mut ident_list = nested_ident.iter();
        ident_list.next();
        
        for ident in ident_list {
            value = value.dot(ident)?;
        }

        Ok(value)
    }

    pub fn types(&self) -> Result<ScopeTypes, TypeError> {
        let mut scopes = Vec::new();

        for scope in &self.scopes {
            let TTypeId::Composite(CompositeTTypeId::Anonymous(ttype)) = scope.ttype_id() else {
                unreachable!("Scope type is not anonymous!");
            };

            let CompositeType::Struct(ttype) = *ttype else {
                unreachable!("Scope type is not a struct!");
            };
            
            scopes.push(Cow::Owned(ttype));
        }

        Ok(ScopeTypes {
            scopes,
        })
    }
}
