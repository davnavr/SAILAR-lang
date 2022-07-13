#!/usr/bin/env just --justfile

test:
    cargo test

fmt:
    cargo fmt --all

clippy:
    cargo clippy --all --all-features

check: fmt clippy test
