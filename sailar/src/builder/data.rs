use crate::{builder, format};
use std::rc::Rc;

pub struct Definitions {
    entries: std::cell::RefCell<Vec<Rc<Data>>>,
    data_index: builder::counter::Cell<format::indices::Data>,
}

impl Definitions {
    pub(super) fn new() -> Self {
        Self {
            entries: std::cell::RefCell::new(Vec::new()),
            data_index: builder::counter::Cell::new(),
        }
    }

    pub fn define(&self, data: Box<[u8]>) -> Rc<Data> {
        let data = Rc::new(Data {
            index: self.data_index.next(),
            data,
        });
        self.entries.borrow_mut().push(data.clone());
        data
    }

    pub(super) fn build(&mut self) -> Vec<format::DataArray> {
        self.entries
            .borrow_mut()
            .drain(..)
            .map(|data| {
                format::DataArray(format::LenVec(match Rc::try_unwrap(data) {
                    Ok(Data { data, .. }) => Vec::from(data),
                    Err(data) => Vec::from(&*data.data),
                }))
            })
            .collect()
    }
}

pub struct Data {
    index: format::indices::Data,
    data: Box<[u8]>,
}

impl Data {
    pub fn index(&self) -> format::indices::Data {
        self.index
    }

    pub fn bytes(&self) -> &[u8] {
        &self.data
    }
}
