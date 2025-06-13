use std::{io, marker::PhantomData, ops::{Deref, DerefMut}, sync::{Arc, Mutex}};

use crate::error::NormaliseToIo;

use super::{DbHeader, PagePtr, Pager};

pub struct LinkedPageWriteGuard<'a, T: binrw::BinRead + binrw::BinWrite>
where
    for<'b> <T as binrw::BinRead>::Args<'b>: Default,
    for<'b> <T as binrw::BinWrite>::Args<'b>: Default {
    pager: Arc<Mutex<Pager>>,
    page_ptr: PagePtr,
    inner: T,
    _marker: PhantomData<&'a ()>,
}

impl<'a, T: binrw::BinRead + binrw::BinWrite> LinkedPageWriteGuard<'a, T>
where
    for<'b> <T as binrw::BinRead>::Args<'b>: Default,
    for<'b> <T as binrw::BinWrite>::Args<'b>: Default {
    pub fn new(pager: Arc<Mutex<Pager>>, page_ptr: PagePtr) -> io::Result<LinkedPageWriteGuard<'a, T>> {
        let inner = {
            let mut lock = pager.lock().into_io()?;
            lock.lock_page_exclusive(page_ptr)?;
            lock.read_linked_pages::<T>(page_ptr)?
        };
        Ok(LinkedPageWriteGuard {
            pager,
            page_ptr,
            inner,
            _marker: PhantomData,
        })
    }
}

impl<'a, T: binrw::BinRead + binrw::BinWrite> Deref for LinkedPageWriteGuard<'a, T>
where
    for<'b> <T as binrw::BinRead>::Args<'b>: Default,
    for<'b> <T as binrw::BinWrite>::Args<'b>: Default {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, T: binrw::BinRead + binrw::BinWrite> DerefMut for LinkedPageWriteGuard<'a, T>
where
    for<'b> <T as binrw::BinRead>::Args<'b>: Default,
    for<'b> <T as binrw::BinWrite>::Args<'b>: Default {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a, T: binrw::BinRead + binrw::BinWrite> Drop for LinkedPageWriteGuard<'a, T>
where
    for<'b> <T as binrw::BinRead>::Args<'b>: Default,
    for<'b> <T as binrw::BinWrite>::Args<'b>: Default {
    fn drop(&mut self) {
        let mut lock = self.pager.lock().expect("failed to lock pager");
        lock.write_linked_pages(self.page_ptr, &self.inner).expect("failed to write memory to pager buffer");
        lock.unlock_page(self.page_ptr).expect("failed to unlock page");
    }
}

pub struct LinkedPageReadGuard<'a, T: binrw::BinRead>
where for<'b> <T as binrw::BinRead>::Args<'b>: Default {
    pager: Arc<Mutex<Pager>>,
    page_ptr: PagePtr,
    inner: T,
    _marker: PhantomData<&'a ()>,
}

impl<'a, T: binrw::BinRead> LinkedPageReadGuard<'a, T>
where for<'b> <T as binrw::BinRead>::Args<'b>: Default {
    pub fn new(pager: Arc<Mutex<Pager>>, page_ptr: PagePtr) -> io::Result<LinkedPageReadGuard<'a, T>> {
        let inner = {
            let mut lock = pager.lock().into_io()?;
            lock.lock_page_shared(page_ptr)?;
            lock.read_linked_pages::<T>(page_ptr)?
        };
        Ok(LinkedPageReadGuard {
            pager,
            page_ptr,
            inner,
            _marker: PhantomData,
        })
    }
}

impl<'a, T: binrw::BinRead> Deref for LinkedPageReadGuard<'a, T>
where for<'b> <T as binrw::BinRead>::Args<'b>: Default {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, T: binrw::BinRead> Drop for LinkedPageReadGuard<'a, T>
where for<'b> <T as binrw::BinRead>::Args<'b>: Default {
    fn drop(&mut self) {
        self.pager.lock().expect("failed to lock pager")
            .unlock_page(self.page_ptr).expect("failed to unlock page")
    }
}

pub struct HeaderWriteGuard<'a> {
    pager: Arc<Mutex<Pager>>,
    inner: DbHeader,
    _marker: PhantomData<&'a ()>,
}

impl<'a> HeaderWriteGuard<'a> {
    pub fn new(pager: Arc<Mutex<Pager>>) -> io::Result<HeaderWriteGuard<'a>> {
        let inner = {
            let mut lock = pager.lock().into_io()?;
            lock.lock_page_exclusive(0u64)?;
            lock.read_header()?
        };
        Ok(HeaderWriteGuard {
            pager,
            inner,
            _marker: PhantomData,
        })
    }
}

impl<'a> Deref for HeaderWriteGuard<'a> {
    type Target = DbHeader;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> DerefMut for HeaderWriteGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a> Drop for HeaderWriteGuard<'a> {
    fn drop(&mut self) {
        let mut lock = self.pager.lock().expect("failed to lock pager");
        lock.write_header(&self.inner).expect("failed to write memory to pager buffer");
        lock.unlock_page(0u64).expect("failed to unlock page");
    }
}

pub struct HeaderReadGuard<'a> {
    pager: Arc<Mutex<Pager>>,
    inner: DbHeader,
    _marker: PhantomData<&'a ()>,
}

impl<'a> HeaderReadGuard<'a> {
    pub fn new(pager: Arc<Mutex<Pager>>) -> io::Result<HeaderReadGuard<'a>> {
        let inner = {
            let mut lock = pager.lock().into_io()?;
            lock.lock_page_shared(0u64)?;
            lock.read_header()?
        };
        Ok(HeaderReadGuard {
            pager,
            inner,
            _marker: PhantomData,
        })
    }
}

impl<'a> Deref for HeaderReadGuard<'a> {
    type Target = DbHeader;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> Drop for HeaderReadGuard<'a> {
    fn drop(&mut self) {
        self.pager.lock().expect("failed to lock pager")
            .unlock_page(0u64).expect("failed to unlock page")
    }
}
