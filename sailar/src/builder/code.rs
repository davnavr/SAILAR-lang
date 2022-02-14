use crate::builder::{self, block::Block};
use crate::format;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Definitions {
    definitions: RefCell<Vec<Rc<Code>>>,
    code_index: builder::counter::Cell<format::indices::Code>,
    type_signatures: Rc<builder::TypeSignatures>,
}

impl Definitions {
    pub(super) fn new(type_signatures: Rc<builder::TypeSignatures>) -> Self {
        Self {
            definitions: RefCell::new(Vec::new()),
            code_index: builder::counter::Cell::new(),
            type_signatures,
        }
    }

    pub fn define(
        &self,
        input_types: Vec<Rc<builder::Type>>,
        result_types: Vec<Rc<builder::Type>>,
    ) -> Rc<Code> {
        let code = Rc::new(Code::new(
            self.code_index.next(),
            self.type_signatures.clone(),
            input_types.into_boxed_slice(),
            result_types.into_boxed_slice(),
        ));
        self.definitions.borrow_mut().push(code.clone());
        code
    }

    pub fn reserve(&self, count: usize) {
        self.definitions.borrow_mut().reserve(count);
    }

    pub(super) fn build(&mut self) -> Vec<format::Code> {
        self.definitions
            .borrow_mut()
            .iter_mut()
            .map(|code| code.build())
            .collect()
    }
}

#[non_exhaustive]
pub struct Code {
    index: format::indices::Code,
    type_signatures: Rc<builder::TypeSignatures>,
    input_types: Box<[Rc<builder::Type>]>,
    result_types: Box<[Rc<builder::Type>]>,
    entry_block: Block,
    #[allow(clippy::vec_box)] // References to Blocks are required, and Vec may resize.
    blocks: RefCell<Vec<Box<Block>>>,
    block_index: builder::counter::Cell<format::indices::CodeBlock>,
}

impl Code {
    fn new(
        index: format::indices::Code,
        type_signatures: Rc<builder::TypeSignatures>,
        input_types: Box<[Rc<builder::Type>]>,
        result_types: Box<[Rc<builder::Type>]>,
    ) -> Self {
        let block_index = builder::counter::Cell::new();
        let entry_block = Block::new(
            block_index.next(),
            type_signatures.clone(),
            &input_types,
            &result_types,
        );

        Self {
            index,
            type_signatures,
            input_types,
            result_types,
            entry_block,
            blocks: RefCell::new(Vec::new()),
            block_index,
        }
    }

    pub fn index(&self) -> format::indices::Code {
        self.index
    }

    pub fn input_count(&self) -> u32 {
        self.entry_block().input_count()
    }

    pub fn result_count(&self) -> u32 {
        u32::try_from(self.result_types.len()).unwrap()
    }

    pub fn entry_block(&self) -> &Block {
        &self.entry_block
    }

    #[allow(clippy::needless_lifetimes)]
    pub fn define_block<'a>(&'a self, input_types: &[Rc<builder::Type>]) -> &'a Block {
        let block = Box::new(Block::new(
            self.block_index.next(),
            self.type_signatures.clone(),
            input_types,
            &self.result_types,
        ));

        let block_address = std::borrow::Borrow::borrow(&block) as *const Block;

        self.blocks.borrow_mut().push(block);

        // Address points to the memory allocated by the boxed Block.
        unsafe { &*block_address }
    }

    fn build(&self) -> format::Code {
        format::Code {
            entry_block: self.entry_block.build(),
            blocks: format::LenVec(
                self.blocks
                    .take()
                    .iter()
                    .map(|block| block.build())
                    .collect(),
            ),
        }
    }
}

impl std::fmt::Debug for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Code")
            .field("index", &self.index)
            .field("input_types", &self.input_types)
            .field("result_types", &self.result_types)
            .field("entry_block", &self.entry_block)
            .field("blocks", &self.blocks)
            .finish_non_exhaustive()
    }
}
