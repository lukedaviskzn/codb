use std::{io::{self}, num::NonZeroU64, ops::{Bound, RangeBounds}};

use binrw::BinRead;

use crate::{error, relation::{file::page::{BTreeInteriorPage, BTreeLeafPage, BTREE_LEAF_MAX_KEYS}, Row}};

use super::page::{BTreeKey, Page, PagePtr, Pager};

fn read_key(pager: &mut Pager, key: &BTreeKey) -> io::Result<Row> {
    let mut payload = key.local_payload.clone();

    let mut overflow_page_number = key.overflow_page;
    
    while let Some(page_num) = overflow_page_number {
        let overflow_page = pager.read_page(page_num)?;
        
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

pub struct BTreeRangeIter {
    pager: Pager,
    page_stack: Vec<(Page, usize)>,
    start: Bound<Row>,
    end: Bound<Row>,
}

impl BTreeRangeIter {
    pub fn new(mut pager: Pager, root_page: NonZeroU64, range: impl RangeBounds<Row>) -> io::Result<BTreeRangeIter> {
        let Page::BTreeRoot(root_page) = pager.read_page(root_page)? else {
            return Err(io::ErrorKind::InvalidData.into());
        };
        
        let page = pager.read_page(root_page.first_page)?;

        match &page {
            Page::BTreeInterior(_) | Page::BTreeLeaf(_) => {},
            _ => return Err(io::ErrorKind::InvalidData.into()),
        };

        let mut iter = BTreeRangeIter {
            pager,
            page_stack: vec![(page, 0)],
            start: range.start_bound().map(|r| r.clone()),
            end: range.end_bound().map(|r| r.clone()),
        };

        match iter.start.clone() {
            Bound::Included(start) => {
                iter.go_to_row(&start)?;
            },
            Bound::Excluded(start) => {
                iter.go_to_row(&start)?;
                if let Some(Err(err)) = iter.next() {
                    return Err(err);
                }
            },
            _ => {},
        };

        Ok(iter)
    }

    fn go_to_row(&mut self, start: &Row) -> io::Result<()> {
        let (page, _) = self.page_stack.last()
            .ok_or_else(|| io::ErrorKind::InvalidData)?.clone();

        match page {
            Page::BTreeInterior(page) => {
                let mut found_index = 0;
                for (i, key) in page.keys.iter().enumerate() {
                    let row = read_key(&mut self.pager, key)?;
                    if *start >= row {
                        found_index = i+1;
                    }
                }
                let found_ptr = page.ptrs[found_index];

                self.page_stack.last_mut().ok_or(io::ErrorKind::InvalidData)?.1 = found_index + 1;
                
                let page = self.pager.read_page(found_ptr)?;
                
                self.page_stack.push((page, 0));

                self.go_to_row(start)
            },
            Page::BTreeLeaf(page) => {
                let mut found_index = 0;
                for (i, key) in page.keys.iter().enumerate() {
                    let row = read_key(&mut self.pager, key)?;
                    if *start >= row {
                        found_index = i;
                    }
                }

                self.page_stack.last_mut().ok_or(io::ErrorKind::InvalidData)?.1 = found_index;
                Ok(())
            },
            _ => Err(io::ErrorKind::InvalidData.into()),
        }
    }
}

impl Iterator for BTreeRangeIter {
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

                match read_key(&mut self.pager, &key) {
                    Ok(row) => match &self.end {
                        Bound::Included(end) if &row > end => None,
                        Bound::Excluded(end) if &row >= end => None,
                        _ => Some(Ok(row)),
                    },
                    Err(err) => Some(Err(err)),
                }
            },
            _ => return Some(Err(io::ErrorKind::InvalidData.into())),
        }
    }
}

pub fn insert(pager: &mut Pager, root_page_num: PagePtr, row: Row) -> io::Result<bool> {
    struct AlreadyInserted;

    fn _insert(pager: &mut Pager, page_num: PagePtr, row: Row) -> io::Result<Result<Option<(BTreeKey, PagePtr)>, AlreadyInserted>> {
        let page = pager.read_page(page_num)?;
        
        match page {
            Page::BTreeInterior(page) => {
                let mut index = 0;
                for (i, key) in page.keys.iter().enumerate() {
                    let key = read_key(pager, key)?;
                    if row > key {
                        index = i + 1;
                    } else if row == key {
                        return Ok(Err(AlreadyInserted));
                    }
                }
                match _insert(pager, page.ptrs[index], row) {
                    Ok(Ok(Some((key, page_ptr)))) => {
                        todo!()
                    },
                    res => res,
                }
            },
            Page::BTreeLeaf(mut page) => {
                let mut index = 0;
                
                for (i, key) in page.keys.iter().enumerate() {
                    let key = read_key(pager, key)?;
                    if row > key {
                        index = i + 1;
                    } else if row == key {
                        return Ok(Err(AlreadyInserted));
                    }
                }

                let new_key = BTreeKey::allocate(pager, &row)?;
                
                if page.keys.len() < BTREE_LEAF_MAX_KEYS {
                    page.keys.insert(index, new_key);
                    pager.write_page(page_num, &Page::BTreeLeaf(page))?;
                    Ok(Ok(None))
                } else {
                    let mut new_entries = vec![];
                    
                    for key in &page.keys {
                        new_entries.push((key, read_key(pager, key)?));
                    }
                    new_entries.push((&new_key, row));

                    new_entries.sort_by_key(|(_, row)| row.clone());
                    
                    // choose median from existing values & new value
                    let median_idx = new_entries.len()/2;

                    let left_entries = Vec::from_iter(new_entries.drain(..=median_idx));
                    let right_entries = new_entries;
                    let median_entry = left_entries.last().expect("unreachable");

                    pager.write_page(page_num, &Page::BTreeLeaf(BTreeLeafPage {
                        keys: left_entries.iter().map(|(k, _)| (*k).clone()).collect(),
                    }))?;

                    let right_page_ptr = pager.new_page(&Page::BTreeLeaf(BTreeLeafPage {
                        keys: right_entries.iter().map(|(k, _)| (*k).clone()).collect(),
                    }))?;
                    
                    Ok(Ok(Some((median_entry.0.clone(), right_page_ptr))))
                }
            },
            _ => Err(io::ErrorKind::InvalidData.into()),
        }
    }

    let Page::BTreeRoot(mut root_page) = pager.read_page(root_page_num)? else {
        return Err(io::ErrorKind::InvalidData.into());
    };

    let (new_key, new_page_num) = match _insert(pager, root_page.first_page, row)? {
        Ok(Some(new_page)) => new_page,
        Ok(None) => return Ok(true),
        Err(AlreadyInserted) => return Ok(false),
    };

    let new_first_page = pager.new_page(&Page::BTreeInterior(BTreeInteriorPage {
        keys: vec![new_key],
        ptrs: vec![root_page_num, new_page_num],
    }))?;

    root_page.first_page = new_first_page;

    pager.write_page(root_page_num, &Page::BTreeRoot(root_page))?;

    Ok(true)
}
