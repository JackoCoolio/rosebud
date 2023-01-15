use std::{
    fs::{read, OpenOptions},
    process::exit, io::{Write, self, Read},
};

use thiserror::Error;

use rosebud::BudFile;

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
            _ => Err(format!("invalid emit mode '{}'", value)),
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

fn read_stdin() -> io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    Ok(bytes)
}

#[derive(Error, Debug)]
pub enum FormatError {
    #[error("{0} contains non-UTF-8 characters")]
    NotUtf8(String),
    #[error("a file contains non-UTF-8 characters")]
    NotUtf8FileUnspecified,
    #[error("couldn't read from {0}")]
    OpenForRead(String),
    #[error("the 'files' emit mode cannot be used with stdin")]
    FilesModeWithStdin,
    #[error("{0}")]
    ParseError(String),
    #[error("couldn't open file '{0}' for writing")]
    OpenForWrite(String),
    #[error("couldn't write to file '{0}'")]
    WriteError(String),
}

fn format_data(bytes: &[u8]) -> Result<String, FormatError> {
    let Ok(s) = String::from_utf8(bytes.to_vec()) else {
        return Err(FormatError::NotUtf8FileUnspecified);
    };

    let bud_file = BudFile::parse(&s).map_err(|e| FormatError::ParseError(format!("{}", e)))?;

    Ok(bud_file.to_string())
}

pub fn format_stdin(mode: EmitMode) -> Result<(), FormatError> {
    let mode = mode.fallback(EmitMode::Stdout);

    if let EmitMode::Files = mode {
        return Err(FormatError::FilesModeWithStdin);
    }

    let Ok(data) = read_stdin() else {
        return Err(FormatError::OpenForRead("stdin".to_string()));
    };

    let formatted = format_data(&data)?;

    match &mode {
        EmitMode::Stdout => {
            print!("{}", formatted);
        }
        EmitMode::Files | EmitMode::Default => unreachable!()
    }

    Ok(())
}

pub fn format_files<'a>(files: impl Iterator<Item = &'a String>, mode: EmitMode) -> Result<(), FormatError> {
    let mode = mode.fallback(EmitMode::Files);

    for file in files {
        let Ok(data) = read(file) else {
            eprintln!("couldn't open file '{}'", file);
            exit(1);
        };

        let formatted = match format_data(&data) {
            Err(FormatError::NotUtf8FileUnspecified) => Err(FormatError::NotUtf8(file.to_string())),
            x => x,
        }?;

        match &mode {
            EmitMode::Stdout => {
                println!("{}\n", file);
                print!("{}", formatted);
            }
            EmitMode::Files => {
                let mut f = OpenOptions::new()
                    .truncate(true)
                    .write(true)
                    .create(false)
                    .open(file).map_err(|_| FormatError::OpenForWrite(file.to_string()))?;

                f.write_all(formatted.as_bytes()).map_err(|_| FormatError::WriteError(file.to_string()))?;
            }
            EmitMode::Default => unreachable!(),
        }
    }

    Ok(())
}
