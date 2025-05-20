use std::{borrow::Borrow, fmt::{Debug, Display}, ops::{Deref, Index}, str::FromStr};

use crate::{Ident, ParseIdentError};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct NestedIdent(Box<[Ident]>);

impl NestedIdent {
    pub fn into_inner(self) -> Box<[Ident]> {
        self.0
    }

    pub fn first(&self) -> &Ident {
        &self.0[0]
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

impl From<Ident> for NestedIdent {
    fn from(value: Ident) -> Self {
        Self(Box::new([value]))
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
