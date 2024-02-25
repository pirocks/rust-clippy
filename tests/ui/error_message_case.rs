#![warn(clippy::error_message_case)]

use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

fn main() {
    let _ = ThisIsAnErrorWithLowercase {};
    let _ = ThisIsAnErrorWithUppercase {};
    let _ = ThisIsAnEnumError::ThisIsAStringVariant("Uppercase".to_string());
    let _ = ThisIsAnEnumError::ThisIsAStringVariant("lowercase".to_string());
    let _ = ThisIsAnEnumError::ThisIsALowercaseVariant;
    let _ = ThisIsAnEnumError::ThisIsAnUppercaseVariant;
    // test code goes here
}

#[derive(Debug)]
struct ThisIsAnErrorWithLowercase {}

impl Display for ThisIsAnErrorWithLowercase {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "this is a lowercase error message")
    }
}


impl Error for ThisIsAnErrorWithLowercase {}


#[derive(Debug)]
struct ThisIsAnErrorWithUppercase {}

impl Display for ThisIsAnErrorWithUppercase {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "this is a uppercase error message")
    }
}


impl Error for ThisIsAnErrorWithUppercase {}


#[derive(Debug)]
pub enum ThisIsAnEnumError {
    ThisIsAStringVariant(String),
    ThisIsALowercaseVariant,
    ThisIsAnUppercaseVariant,
}

impl Display for ThisIsAnEnumError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ThisIsAnEnumError::ThisIsAStringVariant(this_string) => {
                write!(f, "{this_string}")
            }
            ThisIsAnEnumError::ThisIsALowercaseVariant => {
                write!(f, "this is lowercase")
            }
            ThisIsAnEnumError::ThisIsAnUppercaseVariant => {
                write!(f, "This is uppercase")
            }
        }
    }
}

impl Error for ThisIsAnEnumError {}

//todo need test with thiserror, and more complex string, typed expressions
