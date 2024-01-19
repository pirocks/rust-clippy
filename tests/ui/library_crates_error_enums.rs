#![warn(clippy::library_crates_error_enum)]


use std::error::Error;

pub fn foo() -> Result<(),String>{
    todo!()
}

pub fn bar() -> Result<(), Box<dyn Error>>{
    todo!()
}