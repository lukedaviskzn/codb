use std::{io, sync::LockResult};

// use codb_core::Ident;

// #[derive(Debug, PartialEq, Eq, thiserror::Error)]
// #[error("{0:?} is already taken")]
// pub struct IdentTaken(pub Ident);

pub trait NormaliseToIo<T> {
    fn into_io(self) -> io::Result<T>;
}

impl<T> NormaliseToIo<T> for binrw::BinResult<T> {
    fn into_io(self) -> io::Result<T> {
        self.map_err(|err| match err {
            binrw::Error::Io(error) => error,
            error => io::Error::new(io::ErrorKind::InvalidData, error),
        })
    }
}

impl<T> NormaliseToIo<T> for LockResult<T> {
    fn into_io(self) -> io::Result<T> {
        self.map_err(|_| io::ErrorKind::Deadlock.into())
    }
}
