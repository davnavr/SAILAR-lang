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
//! Almost all functions provided are also **not thread safe**. This means that it is the duty of callers to do synchronization.

#![allow(non_snake_case)]

pub mod builder;
pub mod error;
pub mod identifier;
pub mod path;
