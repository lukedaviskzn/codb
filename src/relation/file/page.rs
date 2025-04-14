use std::{fs::File, io::{self, Seek}, num::NonZeroU64};

use binrw::{BinRead, BinWrite};

use crate::error;

pub const PAGE_SIZE: usize = 4096;
pub const OVERFLOW_PAYLOAD_MAX: usize = PAGE_SIZE - 12;
pub const BTREE_INTERIOR_MAX_KEYS: usize = 60;
pub const BTREE_KEY_LOCAL_PAYLOAD_MAX: usize = (PAGE_SIZE - 10) / BTREE_INTERIOR_MAX_KEYS - 8;
pub const BTREE_LEAF_MAX_KEYS: usize = (PAGE_SIZE - 4) / (12 + BTREE_KEY_LOCAL_PAYLOAD_MAX);

type PagePtr = NonZeroU64;

#[derive(Debug)]
pub struct Pager {
    file: File,
}

impl Pager {
    pub fn new(file: File) -> Pager {
        Pager {
            file,
        }
    }

    pub fn header(&mut self) -> io::Result<Header> {
        Header::read(&mut self.file).map_err(error::map_binrw_error)
    }

    pub fn read_page(&mut self, page_num: PagePtr) -> io::Result<Page> {
        self.file.seek(io::SeekFrom::Start(page_num.get() * PAGE_SIZE as u64))?;
        Page::read(&mut self.file).map_err(error::map_binrw_error)
    }

    pub fn write_page(&mut self, page_num: PagePtr, page: &Page) -> io::Result<()> {
        self.file.seek(io::SeekFrom::Start(page_num.get() * PAGE_SIZE as u64))?;
        page.write(&mut self.file).map_err(error::map_binrw_error)
    }

    pub fn new_page(&mut self, page: &Page) -> io::Result<PagePtr> {
        let page_num = self.file.seek(io::SeekFrom::End(0))? / PAGE_SIZE as u64;
        let page_num = NonZeroU64::new(page_num).ok_or(io::ErrorKind::InvalidData)?;
        page.write(&mut self.file).map_err(error::map_binrw_error)?;
        Ok(page_num)
    }
}

#[binrw::binrw]
#[brw(big)]
#[brw(magic = b"CoDB")]
#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    pub version: FileVersion,
    pub root_page: NonZeroU64,
}

#[binrw::binrw]
#[derive(Debug, PartialEq, Eq)]
pub enum FileVersion {
    #[brw(magic(0u32))]
    V1,
}

#[binrw::binrw]
#[brw(big)]
#[derive(Debug, PartialEq, Eq)]
pub enum Page {
    #[brw(magic(b"OV"))]
    Overflow(OverflowPage),
    #[brw(magic(b"BC"))]
    BTreeInterior(BTreeInteriorPage),
    #[brw(magic(b"BL"))]
    BTreeLeaf(BTreeLeafPage),
}

#[binrw::binrw]
#[derive(Debug, PartialEq, Eq)]
pub struct OverflowPage {
    // payload len will always be withing u16 range
    #[bw(calc = payload.len().try_into().unwrap_or_default())]
    #[br(temp)]
    pub len: u16,
    #[bw(map = |x| x.map(|x| x.get()).unwrap_or_default())]
    #[br(map = |x: u64| PagePtr::new(x))]
    pub next: Option<PagePtr>,
    #[brw(assert(payload.len() <= OVERFLOW_PAYLOAD_MAX))]
    #[br(count = len)]
    pub payload: Vec<u8>,
}

#[binrw::binrw]
#[derive(Debug, PartialEq, Eq)]
pub struct BTreeInteriorPage {
    // keys will always be withing u16 range
    #[bw(calc = keys.len().try_into().unwrap_or_default())]
    #[br(temp)]
    pub len: u16, // number of keys
    #[brw(assert(keys.len() <= BTREE_INTERIOR_MAX_KEYS))]
    #[br(count = len)]
    pub keys: Vec<BTreeKey>,
    #[brw(assert(ptrs.len() == keys.len() + 1))]
    #[br(count = len + 1)]
    pub ptrs: Vec<PagePtr>,
}

#[binrw::binrw]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BTreeKey {
    #[bw(assert(local_payload.len() == (*len as usize).min(BTREE_KEY_LOCAL_PAYLOAD_MAX)))]
    pub len: u32,
    #[bw(assert(overflow_page.is_some() == (*len as usize >= BTREE_KEY_LOCAL_PAYLOAD_MAX)))]
    #[br(assert(overflow_page.is_some() == (len as usize >= BTREE_KEY_LOCAL_PAYLOAD_MAX)))]
    #[bw(map = |x| x.map(|x| x.get()).unwrap_or_default())]
    #[br(map = |x: u64| PagePtr::new(x))]
    pub overflow_page: Option<PagePtr>,
    #[bw(assert(local_payload.len() <= BTREE_KEY_LOCAL_PAYLOAD_MAX))]
    #[br(count = len.min(BTREE_KEY_LOCAL_PAYLOAD_MAX as u32))]
    pub local_payload: Vec<u8>,
}

#[binrw::binrw]
#[derive(Debug, PartialEq, Eq)]
pub struct BTreeLeafPage {
    // keys will always be withing u16 range
    #[bw(calc = keys.len().try_into().unwrap_or_default())]
    #[br(temp)]
    pub len: u16, // number of keys
    #[brw(assert(keys.len() <= BTREE_LEAF_MAX_KEYS))]
    #[br(count = len)]
    pub keys: Vec<BTreeKey>,
}

#[cfg(test)]
mod tests {
    use std::io::{self, Seek};

    use binrw::{BinRead, BinWrite};

    use super::*;

    #[test]
    fn overflow_page() {
        let page1 = Page::Overflow(OverflowPage {
            next: None,
            payload: vec![1;4084],
        });

        let mut bytes = io::Cursor::new(Vec::<u8>::new());
        page1.write(&mut bytes).unwrap();
        bytes.seek(io::SeekFrom::Start(0)).unwrap();
        let page2 = Page::read(&mut bytes).unwrap();

        assert_eq!(page1, page2);
    }

    #[test]
    fn btree_interior_page() {
        let page1 = Page::BTreeInterior(BTreeInteriorPage {
            keys: vec![
                BTreeKey { len: 5, overflow_page: None, local_payload: vec![1,2,3,4,5] },
                BTreeKey { len: 999, overflow_page: Some(NonZeroU64::MIN), local_payload: vec![1;BTREE_KEY_LOCAL_PAYLOAD_MAX] },
            ],
            ptrs: vec![
                NonZeroU64::MIN,
                NonZeroU64::MIN,
                NonZeroU64::MIN,
            ],
        });

        let mut bytes = io::Cursor::new(Vec::<u8>::new());
        page1.write(&mut bytes).unwrap();
        bytes.seek(io::SeekFrom::Start(0)).unwrap();
        let page2 = Page::read(&mut bytes).unwrap();

        assert_eq!(page1, page2);
    }
}
