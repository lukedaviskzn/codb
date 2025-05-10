use std::{borrow::Borrow, fmt::{Debug, Display}, ops::{Deref, Index}, str::FromStr};

#[derive(Debug)]
pub struct ParseIdentError;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Ident(String);

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.0
    }
}

impl FromStr for Ident {
    type Err = ParseIdentError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        Ident::try_from(string.to_owned()).map_err(|_| ParseIdentError)
    }
}

impl TryFrom<String> for Ident {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if let Some(char) = value.chars().next() {
            if (char.is_alphabetic() || char == '_') && value.chars().all(|c| c.is_alphanumeric() || c == '_') {
                Ok(Ident(value))
            } else {
                Err(value)
            }
        } else {
            Err(value)
        }
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Ident {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<String> for Ident {
    fn eq(&self, other: &String) -> bool {
        &self.0 == other
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct NestedIdent(Box<[Ident]>);

impl NestedIdent {
    pub fn into_inner(self) -> Box<[Ident]> {
        self.0
    }

    pub fn into_vec(self) -> Vec<Ident> {
        self.0.into()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Ident> {
        self.0.iter()
    }
    
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Ident> {
        self.0.iter_mut()
    }
}

impl Debug for NestedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for NestedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.0.join(".");
        f.write_str(&string)
    }
}

impl FromStr for NestedIdent {
    type Err = ParseIdentError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        if string.is_empty() {
            return Err(ParseIdentError);
        }

        let mut idents = vec![];

        for part in string.split('.') {
            idents.push(part.parse()?);
        }

        Ok(NestedIdent(idents.into()))
    }
}

impl Into<Box<[Ident]>> for NestedIdent {
    fn into(self) -> Box<[Ident]> {
        self.0
    }
}

impl TryFrom<Box<[Ident]>> for NestedIdent {
    type Error = Box<[Ident]>;

    fn try_from(value: Box<[Ident]>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(value)
        } else {
            Ok(NestedIdent(value))
        }
    }
}

impl Into<Vec<Ident>> for NestedIdent {
    fn into(self) -> Vec<Ident> {
        self.0.into()
    }
}

impl TryFrom<Vec<Ident>> for NestedIdent {
    type Error = Vec<Ident>;

    fn try_from(value: Vec<Ident>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(value)
        } else {
            Ok(NestedIdent(value.into()))
        }
    }
}

impl AsRef<[Ident]> for NestedIdent {
    fn as_ref(&self) -> &[Ident] {
        &self.0
    }
}

impl Borrow<[Ident]> for NestedIdent {
    fn borrow(&self) -> &[Ident] {
        &self.0
    }
}

impl Deref for NestedIdent {
    type Target = [Ident];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Index<usize> for NestedIdent {
    type Output = Ident;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}

impl IntoIterator for NestedIdent {
    type Item = Ident;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a NestedIdent {
    type Item = &'a Ident;

    type IntoIter = std::slice::Iter<'a, Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a mut NestedIdent {
    type Item = &'a mut Ident;

    type IntoIter = std::slice::IterMut<'a, Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct IdentTree {
    ident: Ident,
    children: Box<[IdentTree]>,
}

impl IdentTree {
    pub fn from_nested_idents(nested_idents: impl Into<Box<[NestedIdent]>>) -> Box<[IdentTree]> {
        let mut roots: Vec<&Ident> = vec![];

        let nested_idents: Box<[NestedIdent]> = nested_idents.into();

        for nested_ident in &nested_idents {
            let ident = &nested_ident[0];

            if !roots.contains(&ident) {
                roots.push(ident);
            }
        }

        let mut trees = Vec::new();

        for root in roots {
            let mut child_nested_idents = Vec::new();

            for nested_ident in nested_idents.iter().filter(|ni| &ni[0] == root) {
                let rest = nested_ident.clone().into_vec().split_off(1);
                
                if let Ok(child_nested_ident) = NestedIdent::try_from(rest) {
                    child_nested_idents.push(child_nested_ident);
                }
            }

            trees.push(IdentTree {
                ident: root.clone(),
                children: Self::from_nested_idents(child_nested_idents),
            });
        }

        trees.into()
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn children(&self) -> &[IdentTree] {
        &self.children
    }
}

impl Debug for IdentTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = if self.children.is_empty() {
            f.debug_tuple(self.ident.as_ref())
        } else {
            f.debug_tuple(&format!("{}.", self.ident.as_ref()))
        };
        for c in &self.children {
            t.field(c);
        }
        t.finish()
    }
}

impl Display for IdentTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = f.debug_tuple(self.ident.as_ref());
        for c in &self.children {
            t.field(c);
        }
        t.finish()
    }
}
