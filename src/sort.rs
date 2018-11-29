use std::io;
use std::io::BufRead;
use std::io::Write;

use crate::util;

pub struct SortCommand;
impl crate::command::Command for SortCommand {
    fn execute<R: BufRead, W: Write>(
        args: Vec<String>,
        input: &mut R,
        output: &mut W,
    ) -> Result<(), io::Error> {
        sort(args, input, output)
    }
}

// ---- command line arguments -------------------------------------------------

// ---- main procedure ---------------------------------------------------------

fn sort<R: BufRead, W: Write>(
    args: Vec<String>,
    input: &mut R,
    output: &mut W,
) -> Result<(), io::Error> {
    Ok(())
}
