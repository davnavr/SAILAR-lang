//! The C API for SAILAR.
//!
//! This crate is not intended to be used in Rust. When compiled to a static or dynamic library, it allows other languages
//! (C, C++, C#, Java, etc.) to use the functionality provided by [`sailar`].
//!
//! # Safety
//!
//! Note that as a C API, the provided functions make pervasive use of [`raw pointers`](pointer); and as such,
//! [Rust's pointer safety rules apply](std::ptr#safety).
//!
//! Where possible, the functions provided by this library aim to be thread safe. The main exception to these are disposal
//! functions, which free memory.

#![allow(non_snake_case)]

pub mod builder;
pub mod error;
pub mod identifier;
