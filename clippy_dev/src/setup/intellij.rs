use std::collections::HashSet;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use toml_edit::{value, Array, Document, Item, Table};

// This module takes an absolute path to a rustc repo and alters the dependencies to point towards
// the respective rustc subcrates instead of using extern crate xyz.
// This allows IntelliJ to analyze rustc internals and show proper information inside Clippy
// code. See https://github.com/rust-lang/rust-clippy/issues/5514 for details

const CLIPPY_PROJECTS: &[ClippyProjectInfo] = &[
    ClippyProjectInfo::new("root", "Cargo.toml", "src/driver.rs"),
    ClippyProjectInfo::new("clippy_lints", "clippy_lints/Cargo.toml", "clippy_lints/src/lib.rs"),
    ClippyProjectInfo::new("clippy_utils", "clippy_utils/Cargo.toml", "clippy_utils/src/lib.rs"),
];

/// Used to store clippy project information to later inject the dependency into.
struct ClippyProjectInfo {
    /// Only used to display information to the user
    name: &'static str,
    cargo_file: &'static str,
    lib_rs_file: &'static str,
}

impl ClippyProjectInfo {
    const fn new(name: &'static str, cargo_file: &'static str, lib_rs_file: &'static str) -> Self {
        Self {
            name,
            cargo_file,
            lib_rs_file,
        }
    }
}

pub fn setup_rustc_src(rustc_path: &str) {
    let Ok(rustc_source_dir) = check_and_get_rustc_dir(rustc_path) else {
        return;
    };

    for project in CLIPPY_PROJECTS {
        if inject_deps_into_project(&rustc_source_dir, project).is_err() {
            return;
        }
    }

    println!("info: the source paths can be removed again with `cargo dev remove intellij`");
}

fn check_and_get_rustc_dir(rustc_path: &str) -> Result<PathBuf, ()> {
    let mut path = PathBuf::from(rustc_path);

    if path.is_relative() {
        match path.canonicalize() {
            Ok(absolute_path) => {
                println!("info: the rustc path was resolved to: `{}`", absolute_path.display());
                path = absolute_path;
            },
            Err(err) => {
                eprintln!("error: unable to get the absolute path of rustc ({err})");
                return Err(());
            },
        };
    }

    let path = path.join("compiler");
    println!("info: looking for compiler sources at: {}", path.display());

    if !path.exists() {
        eprintln!("error: the given path does not exist");
        return Err(());
    }

    if !path.is_dir() {
        eprintln!("error: the given path is not a directory");
        return Err(());
    }

    Ok(path)
}

fn inject_deps_into_project(rustc_source_dir: &Path, project: &ClippyProjectInfo) -> Result<(), ()> {
    let cargo_content = read_project_file(project.cargo_file)?;
    let lib_content = read_project_file(project.lib_rs_file)?;

    if inject_deps_into_manifest(rustc_source_dir, project.cargo_file, &cargo_content, &lib_content).is_err() {
        eprintln!(
            "error: unable to inject dependencies into {} with the Cargo file {}",
            project.name, project.cargo_file
        );
        Err(())
    } else {
        Ok(())
    }
}

/// `clippy_dev` expects to be executed in the root directory of Clippy. This function
/// loads the given file or returns an error. Having it in this extra function ensures
/// that the error message looks nice.
fn read_project_file(file_path: &str) -> Result<String, ()> {
    let path = Path::new(file_path);
    if !path.exists() {
        eprintln!("error: unable to find the file `{file_path}`");
        return Err(());
    }

    match fs::read_to_string(path) {
        Ok(content) => Ok(content),
        Err(err) => {
            eprintln!("error: the file `{file_path}` could not be read ({err})");
            Err(())
        },
    }
}

fn extract_rustc_crates(lib_rs: &str) -> HashSet<String> {
    lib_rs
        .lines()
        // only take dependencies starting with `rustc_`
        .filter(|line| line.starts_with("extern crate rustc_"))
        // we have something like "extern crate foo;", we only care about the "foo"
        // extern crate rustc_middle;
        //              ^^^^^^^^^^^^
        .map(|s| s.replace("extern crate", "").replace(';', "").trim().to_string())
        .collect()
}

fn parse_cargo_toml(cargo_toml: &str) -> Result<Document, ()> {
    match cargo_toml.parse::<Document>() {
        Ok(doc) => Ok(doc),
        Err(err) => {
            eprintln!("Cannot parse Cargo.toml: {err}");
            Err(())
        },
    }
}

fn extract_table<'a>(cargo_toml: &'a mut Document, table: &str) -> Result<&'a mut Table, ()> {
    let Some(Item::Table(deps)) = cargo_toml.get_mut(table) else {
        eprintln!("No dependencies entry, or malformed dependencies entry in Cargo.toml?");
        return Err(());
    };
    Ok(deps)
}

fn inject_deps_into_manifest(
    rustc_source_dir: &Path,
    manifest_path: &str,
    cargo_toml: &str,
    lib_rs: &str,
) -> Result<(), ()> {
    let mut needed_crates = extract_rustc_crates(lib_rs);
    let mut toml_document = parse_cargo_toml(cargo_toml)?;
    let deps = extract_table(&mut toml_document, "dependencies")?;

    let local_deps: HashSet<_> = deps
        .iter()
        .map(|(dep_name, _)| dep_name)
        .filter(|dep_name| CLIPPY_PROJECTS.iter().any(|project| project.name == *dep_name))
        .map(ToString::to_string)
        .collect();

    for (dep_name, _dep) in deps.iter() {
        if needed_crates.remove(&dep_name.to_string()) {
            eprintln!("warn: dependency {dep_name} is already setup inside {manifest_path}, skipping dependency");
        }
    }

    // do not inject deps if we have already done so
    if needed_crates.is_empty() {
        eprintln!("warn: dependencies are already setup inside {manifest_path}, skipping file");
        return Ok(());
    }

    for needed_crate in needed_crates.clone() {
        let mut crate_table = Table::new();
        crate_table.insert(
            "path",
            value(rustc_source_dir.join(&needed_crate).to_string_lossy().to_string()),
        );
        crate_table.insert("optional", value(true));
        deps.insert(&needed_crate, Item::Table(crate_table));
    }

    let features = extract_table(&mut toml_document, "features")?;
    let features_array = features
        .entry("intellij")
        .or_insert(value(Array::new()))
        .as_array_mut()
        .unwrap();
    features_array.extend(needed_crates);
    features_array.extend(local_deps.into_iter().map(|feature| feature + "/intellij"));

    match File::create(manifest_path) {
        Ok(mut file) => {
            if let Err(err) = file.write_all(toml_document.to_string().as_bytes()) {
                println!("Error opening {manifest_path}: {err}");
                return Err(());
            }
        },
        Err(err) => {
            println!("Error opening {manifest_path}: {err}");
            return Err(());
        },
    };

    println!("info: successfully setup dependencies inside {manifest_path}");

    Ok(())
}

pub fn remove_rustc_src() {
    for project in CLIPPY_PROJECTS {
        let Ok(cargo_content) = read_project_file(project.cargo_file) else {
            return;
        };
        let Ok(lib_content) = read_project_file(project.lib_rs_file) else {
            return;
        };
        if remove_rustc_src_from_project(project.cargo_file, cargo_content.as_str(), lib_content.as_str()).is_err() {
            return;
        }
    }
}

fn remove_rustc_src_from_project(manifest_path: &str, cargo_toml: &str, lib_rs: &str) -> Result<(), ()> {
    let crates_to_remove = extract_rustc_crates(lib_rs);
    let mut toml_document = parse_cargo_toml(cargo_toml)?;
    let deps = extract_table(&mut toml_document, "dependencies")?;

    let mut found_deps = false;
    for crate_to_remove in crates_to_remove {
        if deps.remove(&crate_to_remove).is_some() {
            found_deps = true;
        }
    }
    let features = extract_table(&mut toml_document, "features")?;
    let removed_feature = features.remove_entry("intellij").is_some();

    if !found_deps && !removed_feature {
        println!("info: dependencies and features could not be found in `{manifest_path}`, skipping file");
        return Ok(());
    };

    match File::create(manifest_path) {
        Ok(mut file) => {
            file.write_all(toml_document.to_string().as_bytes()).unwrap();
            println!("info: successfully removed dependencies inside {manifest_path}");
            Ok(())
        },
        Err(err) => {
            eprintln!("error: unable to open file `{manifest_path}`: ({err})");
            Err(())
        },
    }
}
