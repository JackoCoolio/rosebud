use std::process::exit;

use clap::{Arg, ArgAction, ArgMatches, Command, ValueHint};
use cmds::format::{EmitMode, self};

#[macro_use]
extern crate clap;

mod cmds;

const EMIT_HELP: &str = "\
how the formatted data should be emitted, one of:
stdout:          output to stdout
files: (default) overwrite input files\
";

const CHECK_HELP: &str = "\
Only verify that each input file is formatted correctly, without modifying any files. \
Exits with 0 if all files were formatted correctly. Exits with 1 otherwise.\
";

fn parse_args() -> ArgMatches {
    Command::new("rosebud")
        .term_width(80)
        .about("type translation utility")
        .version(crate_version!())
        .subcommand_required(true)
        .arg_required_else_help(true)
        .author(crate_authors!(",\n"))
        .help_expected(true)
        .subcommand(
            Command::new("format")
                .visible_alias("fmt")
                .about("Formats bud files so they look pretty.")
                .arg(
                    Arg::new("files")
                        .help("the input files")
                        .value_hint(ValueHint::FilePath)
                        .num_args(0..),
                )
                .arg(
                    Arg::new("emit")
                        .help(EMIT_HELP)
                        .action(ArgAction::Set)
                        .value_hint(ValueHint::Other)
                        .value_name("mode")
                        .short('e')
                        .long("emit")
                        .num_args(1),
                )
                .arg(
                    Arg::new("backup")
                        .long("backup")
                        .short('b')
                        .help("Backup any modified files")
                        .conflicts_with("check")
                        .action(ArgAction::SetTrue),
                )
                .arg(
                    Arg::new("check")
                        .long("check")
                        .short('c')
                        .help(CHECK_HELP)
                        .conflicts_with("emit")
                        .action(ArgAction::SetTrue),
                ),
        )
        .get_matches()
}

fn main() {
    let matches = parse_args();

    match matches.subcommand() {
        Some(("format", format_matches)) => {
            let emit_mode: EmitMode = match format_matches.get_one::<String>("emit").try_into() {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("{}", e);
                    exit(1);
                }
            };

            let res = match format_matches.get_many::<String>("files") {
                None => {
                    format::format_stdin(emit_mode)
                }
                Some(values) => {
                    format::format_files(values, emit_mode)
                }
            };

            if let Err(e) = res {
                println!("asdlfkajsdf");
                eprintln!("{}", e);
                exit(1);
            }
        }
        _ => unreachable!(),
    }
}
