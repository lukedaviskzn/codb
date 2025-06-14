use std::{fs::File, io, num::NonZeroU64, panic::UnwindSafe};

use binrw::{BinRead, BinWrite, BinWriterExt};
use fs4::fs_std::FileExt;

use crate::{error::NormaliseToIo, DebugByteSlice};

mod guard;

pub use guard::*;

pub type PagePtr = NonZeroU64;
pub type PagePtrRaw = u64;

pub const PAGE_SIZE_U64: u64 = 4096;
pub const PAGE_SIZE: usize = PAGE_SIZE_U64 as usize;

pub trait Lock {
    fn lock_shared(&self) -> io::Result<()>;
    fn lock_exclusive(&self) -> io::Result<()>;
    fn try_lock_shared(&self) -> io::Result<bool>;
    fn try_lock_exclusive(&self) -> io::Result<bool>;
    fn unlock(&self) -> io::Result<()>;
}

impl Lock for File {
    fn lock_shared(&self) -> io::Result<()> {
        FileExt::lock_shared(self)
    }

    fn lock_exclusive(&self) -> io::Result<()> {
        FileExt::lock_exclusive(self)
    }

    fn try_lock_shared(&self) -> io::Result<bool> {
        FileExt::try_lock_shared(self)
    }

    fn try_lock_exclusive(&self) -> io::Result<bool> {
        FileExt::try_lock_exclusive(self)
    }

    fn unlock(&self) -> io::Result<()> {
        FileExt::unlock(self)
    }
}

impl PagerBuffer for File {}

impl Lock for io::Cursor<Vec<u8>> {
    fn lock_shared(&self) -> io::Result<()> {
        Ok(())
    }

    fn lock_exclusive(&self) -> io::Result<()> {
        Ok(())
    }

    fn try_lock_shared(&self) -> io::Result<bool> {
        Ok(true)
    }

    fn try_lock_exclusive(&self) -> io::Result<bool> {
        Ok(true)
    }

    fn unlock(&self) -> io::Result<()> {
        Ok(())
    }
}

impl PagerBuffer for io::Cursor<Vec<u8>> {}

pub trait PagerBuffer: io::Write + io::Read + io::Seek + Lock + UnwindSafe {}

pub struct Pager {
    buf: Box<dyn PagerBuffer>,
}

impl Pager {
    pub fn open(buf: impl PagerBuffer + 'static) -> Pager {
        Pager {
            buf: Box::new(buf),
        }
    }

    pub fn new_memory() -> Pager {
        Pager {
            buf: Box::new(io::Cursor::new(Vec::<u8>::new())),
        }
    }

    pub fn read_header(&mut self) -> io::Result<DbHeader> {
        let index = self.buf.seek(io::SeekFrom::Start(0))?;
        
        // assert at correct page, invalid seeks are handled differently by implementation
        if index != 0 {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }
        
        let mut page_buf = [0u8; PAGE_SIZE];
        self.buf.read_exact(&mut page_buf)?;
        DbHeader::read_be(&mut io::Cursor::new(page_buf)).into_io()
    }

    pub fn write_header(&mut self, header: &DbHeader) -> io::Result<()> {
        let index = self.buf.seek(io::SeekFrom::Start(0))?;
        
        // assert at correct page, invalid seeks are handled differently by implementation
        if index != 0 {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }
        
        let mut page_buf = io::Cursor::new([0u8; PAGE_SIZE]);
        page_buf.write_be(header).into_io()?;

        self.buf.write(page_buf.get_ref())?;
        Ok(())
    }

    fn read_page(&mut self, page_num: PagePtr) -> io::Result<Page> {
        let target_index = page_num.get() * PAGE_SIZE_U64;
        let index = self.buf.seek(io::SeekFrom::Start(target_index))?;
        
        // assert at correct page, invalid seeks are handled differently by implementation
        if index != target_index {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }
        
        let mut page_buf = [0u8; PAGE_SIZE];
        self.buf.read_exact(&mut page_buf)?;
        Page::read_be(&mut io::Cursor::new(page_buf)).into_io()
    }

    fn write_page(&mut self, page_num: PagePtr, page: &Page) -> io::Result<()> {
        let target_index = page_num.get() * PAGE_SIZE_U64;
        let index = self.buf.seek(io::SeekFrom::Start(target_index))?;
        
        // assert at correct page, invalid seeks are handled differently by implementation
        if index != target_index {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }

        let mut page_buf = io::Cursor::new([0u8; PAGE_SIZE]);
        Page::write_be(&page, &mut page_buf).into_io()?;
        self.buf.write(page_buf.get_ref())?;
        Ok(())
    }

    pub fn write_new_page(&mut self, page: &Page) -> io::Result<PagePtr> {
        let index = self.buf.seek(io::SeekFrom::End(0))?;
        
        // assert at correct page, invalid seeks are handled differently by implementation
        if index % PAGE_SIZE_U64 != 0 || index == 0 {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }

        let mut page_buf = io::Cursor::new([0u8; PAGE_SIZE]);
        Page::write_be(&page, &mut page_buf).into_io()?;
        self.buf.write(page_buf.get_ref())?;
        Ok(NonZeroU64::new(index / PAGE_SIZE_U64).expect("unreachable, page ptr already checked"))
    }

    pub fn new_page(&mut self) -> io::Result<PagePtr> {
        let index = self.buf.seek(io::SeekFrom::End(0))?;
        
        // assert at correct page, invalid seeks are handled differently by implementation
        if index % PAGE_SIZE_U64 != 0 || index == 0 {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }

        self.buf.write(&[0u8; PAGE_SIZE])?;
        Ok(NonZeroU64::new(index / PAGE_SIZE_U64).expect("unreachable, page ptr already checked"))
    }

    fn free_page(&mut self, _page_num: PagePtr) -> io::Result<()> {
        // todo: actually free pages
        Ok(())
    }

    fn read_linked_pages_bytes(&mut self, start: PagePtr) -> io::Result<Vec<u8>> {
        let mut data_buf = vec![];

        let mut next = Some(start);

        while let Some(page_ptr) = next {
            let Page::Link(page) = self.read_page(page_ptr)? else {
                return Err(io::ErrorKind::InvalidData.into());
            };

            data_buf.extend(page.data);

            next = page.next;
        }
        
        Ok(data_buf)
    }

    fn read_linked_pages<T: binrw::BinRead>(&mut self, start: PagePtr) -> io::Result<T> where for<'a> <T as binrw::BinRead>::Args<'a>: Default {
        let bytes = self.read_linked_pages_bytes(start)?;
        T::read_be(&mut io::Cursor::new(bytes)).into_io()
    }

    fn read_linked_pages_args<T: binrw::BinRead>(&mut self, start: PagePtr, args: <T as binrw::BinRead>::Args<'_>) -> io::Result<T> {
        let bytes = self.read_linked_pages_bytes(start)?;
        T::read_be_args(&mut io::Cursor::new(bytes), args).into_io()
    }

    fn free_linked_pages(&mut self, start: PagePtr) -> io::Result<()> {
        let mut next = Some(start);

        while let Some(page_ptr) = next {
            let Page::Link(page) = self.read_page(page_ptr)? else {
                return Err(io::ErrorKind::InvalidData.into());
            };

            self.free_page(page_ptr)?;

            next = page.next;
        }
        
        Ok(())
    }

    pub fn new_linked_pages_bytes(&mut self, bytes: &[u8]) -> io::Result<PagePtr> {
        let mut next_page = None;
        
        for section in bytes.chunks(LinkPage::PAYLOAD_SIZE).rev() {
            let page_ptr = self.write_new_page(&Page::Link(LinkPage {
                next: next_page,
                data: section.into(),
            }))?;
            next_page = Some(page_ptr);
        }

        if let Some(next_page) = next_page {
            Ok(next_page)
        } else {
            self.write_new_page(&Page::Link(LinkPage {
                next: None,
                data: Vec::new(),
            }))
        }
    }

    pub fn new_linked_pages<T: binrw::BinWrite>(&mut self, data: &T) -> io::Result<PagePtr> where for<'a> <T as binrw::BinWrite>::Args<'a>: Default {
        let mut bytes = io::Cursor::new(Vec::new());
        data.write_be(&mut bytes).into_io()?;

        self.new_linked_pages_bytes(&bytes.into_inner())
    }

    pub fn new_linked_pages_args<T: binrw::BinWrite>(&mut self, data: &T, args: <T as binrw::BinWrite>::Args<'_>) -> io::Result<PagePtr> {
        let mut bytes = io::Cursor::new(Vec::new());
        data.write_be_args(&mut bytes, args).into_io()?;

        self.new_linked_pages_bytes(&bytes.into_inner())
    }

    fn write_linked_pages_bytes(&mut self, mut page_num: PagePtr, bytes: &[u8]) -> io::Result<()> {
        let mut chunks = bytes.chunks(LinkPage::PAYLOAD_SIZE).peekable();

        while let Some(chunk) = chunks.next() {
            let Page::Link(page) = self.read_page(page_num)? else {
                return Err(io::ErrorKind::InvalidData.into());
            };

            let has_next = chunks.peek().is_some();

            let next_page = if has_next {
                if let Some(next) = page.next {
                    // next page already allocated
                    Some(next)
                } else {
                    // next page not yet allocated
                    Some(self.new_page()?)
                }
            } else {
                if let Some(next) = page.next {
                    // next page allocated, must delete
                    self.free_linked_pages(next)?
                }
                None
            };

            self.write_page(page_num, &Page::Link(LinkPage {
                next: next_page,
                data: chunk.into(),
            }))?;
            
            if let Some(next_page) = next_page {
                page_num = next_page;
            }
        }

        Ok(())
    }

    fn write_linked_pages<T: binrw::BinWrite>(&mut self, page_num: PagePtr, data: &T) -> io::Result<()> where for<'a> <T as binrw::BinWrite>::Args<'a>: Default {
        let mut bytes = io::Cursor::new(Vec::new());
        data.write_be(&mut bytes).into_io()?;

        self.write_linked_pages_bytes(page_num, &bytes.into_inner())
    }

    fn write_linked_pages_args<T: binrw::BinWrite>(&mut self, page_num: PagePtr, data: &T, args: <T as binrw::BinWrite>::Args<'_>) -> io::Result<()> {
        let mut bytes = io::Cursor::new(Vec::new());
        data.write_be_args(&mut bytes, args).into_io()?;

        self.write_linked_pages_bytes(page_num, &bytes.into_inner())
    }

    fn lock_page_shared(&mut self, _page_num: impl Into<PagePtrRaw>) -> io::Result<()> {
        // todo: locking
        Ok(())
    }

    fn lock_page_exclusive(&mut self, _page_num: impl Into<PagePtrRaw>) -> io::Result<()> {
        // todo: locking
        Ok(())
    }

    fn unlock_page(&mut self, _page_num: impl Into<PagePtrRaw>) -> io::Result<()> {
        // todo: locking
        Ok(())
    }
    
    pub fn debug_fmt(&mut self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.buf.seek(io::SeekFrom::Start(0)).map_err(|_| std::fmt::Error)?;
        
        let mut pager_contents = Vec::new();
        self.buf.read_to_end(&mut pager_contents).map_err(|_| std::fmt::Error)?;

        let pager_contents = DebugByteSlice {
            bytes: &pager_contents,
        };
        
        f.debug_struct("Pager").field("buf", &pager_contents).finish()
    }
}

#[binrw]
pub enum Page {
    #[brw(magic = b"O")]
    Link(LinkPage),
    #[brw(magic = b"F")]
    FreeList(FreeListPage),
}

#[binrw]
#[brw(magic = b"CoDB")]
pub struct DbHeader {
    pub version: DbVersion,
    pub manifest: PagePtr,
    pub freelist: PagePtr,
}

impl DbHeader {
    pub fn new(manifest: PagePtr, freelist: PagePtr) -> Self {
        Self {
            version: Default::default(),
            manifest,
            freelist,
        }
    }
}

#[binrw]
pub struct DbVersion {
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
}

impl Default for DbVersion {
    fn default() -> Self {
        Self {
            major: env!("CARGO_PKG_VERSION_MAJOR").parse().expect("failed to parse db major version"),
            minor: env!("CARGO_PKG_VERSION_MINOR").parse().expect("failed to parse db minor version"),
            patch: env!("CARGO_PKG_VERSION_PATCH").parse().expect("failed to parse db patch version"),
        }
    }
}

#[binrw]
pub struct LinkPage {
    #[br(map = |next: u64| PagePtr::new(next))]
    #[bw(map = |next| next.map(|n| n.get()).unwrap_or_default())]
    pub next: Option<PagePtr>,
    #[bw(try_calc = u16::try_from(data.len()))]
    pub len: u16,
    #[br(count = len)]
    #[bw(assert(data.len() <= LinkPage::PAYLOAD_SIZE))]
    pub data: Vec<u8>,
}

impl LinkPage {
    // page size - page magic - page ptr - payload len u16
    pub const PAYLOAD_SIZE: usize = PAGE_SIZE_U64 as usize - 1 - (PagePtr::BITS / 8) as usize - (u16::BITS / 8) as usize;
}

#[binrw]
pub struct FreeListPage {
    #[br(map = |next: u64| PagePtr::new(next))]
    #[bw(map = |next| next.map(|n| n.get()).unwrap_or_default())]
    pub next: Option<PagePtr>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_version() {
        DbVersion::default();
    }
}
