use std::{fs::{self, File}, path::PathBuf};

use ariadne::{Label, Source};
use clap::Parser;
use codb::{query::{parser::ParseError, Span}, ConnectionExecutionError};
use rustyline::DefaultEditor;

#[derive(Debug, clap::Parser)]
#[command(version, about)]
struct Args {
    db_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    ctrlc::set_handler(|| {
        std::process::exit(0);
    })?;

    let Args {
        db_path,
    } = Args::parse();
    
    let connection = if let Some(db_path) = db_path {
        if fs::exists(&db_path)? {
            let file = File::options().read(true).write(true).open(db_path)?;
            let conn = codb::Connection::open(file)?;
            conn
        } else {
            let file = File::options().read(true).write(true).create_new(true).open(db_path)?;
            let conn = codb::Connection::create_new(file)?;
            conn
        }
    } else {
        let conn = codb::Connection::create_new_memory();
        conn
    };

    let mut rl = DefaultEditor::new()?;

    loop {
        let mut query = rl.readline("> ")? + "\n";

        loop {
            let line = rl.readline("")?;
            if line.trim().is_empty() {
                break;
            }
            query += &line;
            query.push('\n');
        }

        let query = query.trim();

        // shell command or schema query
        if query.starts_with('.') {
            let whitespace = query.find(char::is_whitespace).unwrap_or(query.len());
            let command = &query[1..whitespace];
            match command {
                "exit" => return Ok(()),
                "schema" => {
                    let schema_query = query[7..].trim();
                    let result = connection.execute_schema(schema_query);
                    match result {
                        Ok(value) => println!("{value:#?}"),
                        Err(err) => print_connection_error(&err, schema_query),
                    }
                },
                "" => println!("expected shell command"),
                command => println!("unknown command `.{command}`"),
            }
        } else { // data query
            match connection.execute(&query) {
                Ok(value) => println!("{value:#?}"),
                Err(err) => print_connection_error(&err, &query),
            }
        }
    }
}

fn print_connection_error(err: &ConnectionExecutionError, query: &str) {
    match err {
        ConnectionExecutionError::LexError(err) => print_error_report(err.span, err.kind.to_string(), query),
        ConnectionExecutionError::ParseError(err) => print_error_report(err.span, err.kind.to_string(), query),
        ConnectionExecutionError::QueryExecutionError(err) => println!("{err}"),
    }
}

fn print_error_report(span: Span, message: String, query: &str) {
    let context_start = span.start.checked_sub(100).unwrap_or_default();
    let context_end = (
        span.end.unwrap_or(span.start) + 100
    ).min(query.len());
    
    let context_span = context_start..context_end;
    
    let span = if let Some(end) = span.end {
        span.start..(end)
    } else {
        span.start..query.chars().count()
    };
    
    let report = ariadne::Report::build(ariadne::ReportKind::Error, ("query", context_span))
        .with_message(message)
        .with_label(
            Label::new(("query", span))
                .with_message("error occurred here")
        )
        .finish();
    
    report.print(("query", Source::from(query))).expect("failed to write error report to stdout");
}
