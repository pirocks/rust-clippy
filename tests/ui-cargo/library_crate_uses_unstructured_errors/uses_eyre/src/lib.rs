#![warn(clippy::library_crates_structured_errors)]

pub fn uses_eyre_error_directly() -> Result<(), eyre::Report> {
    todo!()
}

pub fn uses_eyre_error_indirectly() -> eyre::Result<()> {
    uses_eyre_error_directly()?;
    Ok(())
}
