use std::{
    fs::{read, OpenOptions},
    process::exit, io::Write,
};

use anyhow::Result;

use crate::bud::Bud;

pub enum EmitMode {
    Files,
    Stdout,
    Default,
}

impl TryFrom<Option<&String>> for EmitMode {
    type Error = String;

    fn try_from(value: Option<&String>) -> Result<Self, Self::Error> {
        let Some(value) = value else {
            return Ok(EmitMode::Default);
        };

        match value.as_str() {
            "files" => Ok(Self::Files),
            "stdout" => Ok(Self::Stdout),
            x => Err(format!("invalid emit mode '{}'", value)),
        }
    }
}

impl EmitMode {
    pub fn is_default(&self) -> bool {
        matches!(self, Self::Default)
    }

    pub fn fallback(self, mode: Self) -> Self {
        assert!(!mode.is_default());

        match self {
            EmitMode::Default => mode,
            x => x,
        }
    }
}

fn emit(mode: &EmitMode, file: String, data: String) {
    todo!()
}

pub fn format_stdin() {}

pub fn format_files<'a>(files: impl Iterator<Item = &'a String>, mode: EmitMode) {
    let mode = mode.fallback(EmitMode::Files);

    for file in files {
        let Ok(data) = read(file) else {
            eprintln!("couldn't open file '{}'", file);
            exit(1);
        };

        let Ok(s) = String::from_utf8(data) else {
            eprintln!("file '{}' contains invalid characters", file);
            exit(1);
        };

        let bud = match Bud::from_str(&s) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{}", e);
                exit(1);
            }
        };

        let formatted = bud.to_string();

        match &mode {
            EmitMode::Stdout => {
                println!("{}\n", file);
                println!("{}", formatted);
            }
            EmitMode::Files => {
                let Ok(mut f) = OpenOptions::new()
                    .truncate(true)
                    .write(true)
                    .create(false)
                    .open(file) else {
                        eprintln!("couldn't open file '{}' for writing", file);
                        exit(1);
                    };

                f.write_all(formatted.as_bytes()).map_err(|_| {
                    eprintln!("couldn't write to file '{}'", file);
                    exit(1);
                });
            }
            EmitMode::Default => unreachable!(),
        }
    }
}
