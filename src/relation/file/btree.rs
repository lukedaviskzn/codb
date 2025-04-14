use std::{fs::File, io::{self, Seek}, num::NonZeroU64};

use binrw::BinRead;

use crate::{error, relation::Row};

use super::page::{BTreeKey, Page, Pager, PAGE_SIZE};

pub struct BTreePageIter {
    pager: Pager,
    page_stack: Vec<(Page, usize)>,
}

impl BTreePageIter {
    pub fn new(mut pager: Pager, root_page: NonZeroU64) -> io::Result<BTreePageIter> {
        let page = pager.read_page(root_page)?;

        match &page {
            Page::BTreeInterior(_) | Page::BTreeLeaf(_) => {},
            _ => return Err(io::ErrorKind::InvalidData.into()),
        };
        
        Ok(BTreePageIter {
            pager,
            page_stack: vec![(page, 0)],
        })
    }

    fn read_key(&mut self, key: &BTreeKey) -> io::Result<Row> {
        let mut payload = key.local_payload.clone();

        let mut overflow_page_number = key.overflow_page;
        
        while let Some(page_num) = overflow_page_number {
            let overflow_page = self.pager.read_page(page_num)?;
            
            let Page::Overflow(overflow_page) = overflow_page else {
                return Err(io::ErrorKind::InvalidData.into());
            };

            payload.extend(overflow_page.payload);
            
            overflow_page_number = overflow_page.next;
        }
        
        let mut payload = io::Cursor::new(payload);
        let row = Row::read(&mut payload)
            .map_err(error::map_binrw_error)?;

        Ok(row)
    }
}

impl Iterator for BTreePageIter {
    type Item = io::Result<Row>;

    fn next(&mut self) -> Option<Self::Item> {
        let (current_page, current_index) = self.page_stack.last_mut()?;

        match current_page {
            Page::BTreeInterior(interior_page) => {
                // reached end of page
                if *current_index >= interior_page.ptrs.len() {
                    self.page_stack.pop();
                    if let Some((_, index)) = self.page_stack.last_mut() {
                        *index += 1;
                    }
                    return self.next();
                }

                let ptr = interior_page.ptrs[*current_index];

                *current_index += 1;

                let next_page = match self.pager.read_page(ptr) {
                    Ok(page) => page,
                    Err(err) => return Some(Err(err)),
                };

                self.page_stack.push((next_page, 0));
                
                self.next()
            },
            Page::BTreeLeaf(leaf_page) => {
                // reached end of page
                if *current_index >= leaf_page.keys.len() {
                    self.page_stack.pop();
                    return self.next();
                }

                let key = leaf_page.keys[*current_index].clone();

                *current_index += 1;

                match self.read_key(&key) {
                    Ok(row) => Some(Ok(row)),
                    Err(err) => Some(Err(err)),
                }
            },
            _ => return Some(Err(io::ErrorKind::InvalidData.into())),
        }
    }
}


