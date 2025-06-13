use std::{borrow::Borrow, fmt::{Debug, Display}, ops::{Deref, Index}, str::FromStr};

use crate::{Ident, ParseIdentError};

#[binrw]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct NestedIdent {
    #[bw(calc = inner.len() as u64)]
    len: u64,
    #[br(count = len)]
    inner: Vec<Ident>,
}

impl NestedIdent {
    pub fn into_inner(self) -> Vec<Ident> {
        self.inner
    }

    pub fn first(&self) -> &Ident {
        self.inner.first().expect("nested ident is never empty")
    }

    pub fn last(&self) -> &Ident {
        self.inner.last().expect("nested ident is never empty")
    }

    pub fn split_first(&self) -> (&Ident, &[Ident]) {
        self.inner.split_first().expect("nested ident is never empty")
    }

    pub fn split_last(&self) -> (&Ident, &[Ident]) {
        self.inner.split_last().expect("nested ident is never empty")
    }
}

impl Debug for NestedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for NestedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.inner.join(".");
        f.write_str(&string)
    }
}

impl FromStr for NestedIdent {
    type Err = ParseIdentError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        if string.is_empty() {
            return Err(ParseIdentError::Empty);
        }

        let mut idents = vec![];

        for part in string.split('.') {
            idents.push(part.parse()?);
        }

        Ok(NestedIdent {
            inner: idents.into(),
        })
    }
}

impl From<Ident> for NestedIdent {
    fn from(value: Ident) -> Self {
        Self {
            inner: vec![value],
        }
    }
}

impl Into<Box<[Ident]>> for NestedIdent {
    fn into(self) -> Box<[Ident]> {
        self.inner.into()
    }
}

impl TryFrom<Box<[Ident]>> for NestedIdent {
    type Error = Box<[Ident]>;

    fn try_from(value: Box<[Ident]>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(value)
        } else {
            Ok(NestedIdent {
                inner: value.into(),
            })
        }
    }
}

impl Into<Vec<Ident>> for NestedIdent {
    fn into(self) -> Vec<Ident> {
        self.inner
    }
}

impl TryFrom<Vec<Ident>> for NestedIdent {
    type Error = Vec<Ident>;

    fn try_from(value: Vec<Ident>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(value)
        } else {
            Ok(NestedIdent {
                inner: value,
            })
        }
    }
}

impl AsRef<[Ident]> for NestedIdent {
    fn as_ref(&self) -> &[Ident] {
        &self.inner
    }
}

impl Borrow<[Ident]> for NestedIdent {
    fn borrow(&self) -> &[Ident] {
        &self.inner
    }
}

impl Deref for NestedIdent {
    type Target = [Ident];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Index<usize> for NestedIdent {
    type Output = Ident;

    fn index(&self, index: usize) -> &Self::Output {
        self.inner.index(index)
    }
}

impl IntoIterator for NestedIdent {
    type Item = Ident;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a NestedIdent {
    type Item = &'a Ident;

    type IntoIter = std::slice::Iter<'a, Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut NestedIdent {
    type Item = &'a mut Ident;

    type IntoIter = std::slice::IterMut<'a, Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}
