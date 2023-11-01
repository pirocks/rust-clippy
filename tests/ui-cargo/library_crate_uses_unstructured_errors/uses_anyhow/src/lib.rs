#![warn(clippy::library_crates_structured_errors)]

pub fn uses_anyhow_error_directly() -> Result<(), anyhow::Error> {
    todo!()
}

pub fn uses_anyhow_error_indirectly() -> anyhow::Result<()> {
    uses_anyhow_error_directly()?;
    Ok(())
}
