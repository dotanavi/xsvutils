use std::cmp::Ordering;
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process;
use std::str;

use memchr::memchr;

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

const KB: usize = 1024;
const MB: usize = 1024 * KB;

/// この値を超えたらファイルにバッファリングする
const DEFAULT_BUFFER_SIZE: usize = 500 * MB;

/// 何番目の列をどの形式でソートするかのデータ
#[derive(Copy, Clone)]
struct SortInfo {
    column_index: usize,
    sort_type: SortType,
}

type SortConfig = Vec<SortInfo>;

#[derive(Debug, Default)]
struct CmdOpt {
    col: String,
    buffer_size: usize,
}

impl CmdOpt {
    pub fn parse(mut args: Vec<String>) -> CmdOpt {
        let mut opt = CmdOpt::default();
        opt.buffer_size = DEFAULT_BUFFER_SIZE;

        while args.len() > 0 {
            let arg = args.remove(0);
            match arg.as_str() {
                "--col" => {
                    opt.col = util::pop_first_or_else(&mut args, || {
                        die!("option --col needs an argument")
                    })
                }
                "--buffer-size" => {
                    let size = util::pop_first_or_else(&mut args, || {
                        die!("option --buffer-size needs an argument")
                    });
                    opt.buffer_size = size.as_str().parse().unwrap();
                }
                _ => die!("Unknown argument: {}", &arg),
            }
        }
        return opt;
    }

    pub fn sort_info(&self, header: &[&str]) -> Result<SortConfig, ()> {
        let mut vec = vec![];
        for col in self.col.split(",") {
            let mut pair = col.split(":");
            let name = pair.next().unwrap();
            let sort_type_name = pair.next().unwrap_or_default();
            let column_index = match header.iter().position(|&x| x == name) {
                Some(x) => x,
                None => {
                    eprintln!("Unknown column: {}", name);
                    return Err(());
                }
            };
            let sort_type = match sort_type_name {
                "n" => SortType::NumberAsc,
                "nr" => SortType::NumberDesc,
                "" => SortType::String,
                _ => {
                    eprintln!("Unknown sort type: {}", sort_type_name);
                    return Err(());
                }
            };
            vec.push(SortInfo {
                column_index,
                sort_type,
            });
        }
        return Ok(vec);
    }
}

// ---- main procedure ---------------------------------------------------------

fn sort<R: BufRead, W: Write>(
    args: Vec<String>,
    input: &mut R,
    output: &mut W,
) -> Result<(), io::Error> {
    let opt = CmdOpt::parse(args);

    // 表示する列のインデックス
    let sort_config = {
        let mut buff = String::new();
        let len = input.read_line(&mut buff)?;
        if len == 0 {
            die!(); // NoInput
        }
        output.write_all(buff.as_bytes())?;
        output.flush()?;

        let header: Vec<_> = buff.trim_end().split("\t").collect();
        let info = opt.sort_info(&header);
        match info {
            Ok(x) => x,
            Err(_) => {
                io::copy(input, output)?;
                return Ok(());
            }
        }
    };

    let mut block_count = 0;
    let mut total_buff = vec![];
    let mut line_buff = vec![];
    let mut offsets = vec![];
    loop {
        let len = input.read_until(b'\n', &mut line_buff)?;
        if len == 0 {
            break;
        }
        if total_buff.len() + line_buff.len() > opt.buffer_size {
            sort_and_save(&sort_config, block_count, &offsets, &total_buff)?;
            block_count += 1;
            offsets.clear();
            total_buff.clear();
        }
        let range = (total_buff.len(), total_buff.len() + line_buff.len());
        offsets.push(range);
        total_buff.write_all(&line_buff)?;
        line_buff.clear();
    }

    assert!(line_buff.len() == 0);
    if block_count == 0 {
        sort_and_write(&sort_config, &offsets, &total_buff, output)
    } else {
        sort_and_save(&sort_config, block_count, &offsets, &total_buff)?;
        block_count += 1;
        total_buff.clear();
        merge_and_output(&sort_config, block_count, output)
    }
}

/// データをソートしてファイルに保存する
fn sort_and_save(
    sort_config: &SortConfig,
    fileno: usize,
    offsets: &[(usize, usize)],
    bytes: &[u8],
) -> Result<(), io::Error> {
    let path = file_path(fileno);
    let mut file = File::create(path)?;
    return sort_and_write(sort_config, offsets, bytes, &mut file);
}

/// 一時ファイルのパスを生成
fn file_path(fileno: usize) -> PathBuf {
    match env::var_os("WORKING_DIR") {
        Some(dir) => Path::new(&dir).join(format!("{}.{}.tsv", process::id(), fileno)),
        None => die!("WORKING_DIR is not set."),
    }
}

/// データをソートして出力に書き込む
fn sort_and_write<W: Write>(
    sort_config: &SortConfig,
    offsets: &[(usize, usize)],
    bytes: &[u8],
    output: &mut W,
) -> Result<(), io::Error> {
    let mut vec: Vec<SortableRef> = offsets
        .iter()
        .map(|&range| SortableRef::new(range, sort_config, bytes))
        .collect();
    vec.sort_by(|a, b| cmp_sortable(a, b, sort_config));
    for v in &vec {
        let line: &[u8] = &bytes[v.start..v.end];
        output.write_all(line)?;
    }
    Ok(())
}

/// ソート済みのファイルを複数開き、マージして出力に保存
fn merge_and_output<W: Write>(
    sort_config: &SortConfig,
    block_count: usize,
    output: &mut W,
) -> Result<(), io::Error> {
    let mut files = vec![];
    for fileno in 0..block_count {
        files.push(BufReader::new(File::open(file_path(fileno))?));
    }
    let mut vec = vec![];
    for (ix, mut file) in files.iter_mut().enumerate() {
        let mut buff = vec![];
        let len = file.read_until(b'\n', &mut buff)?;
        assert_ne!(len, 0);
        vec.push(SortableOwned::new(buff, ix, sort_config));
    }

    while vec.len() > 0 {
        vec.sort_by(|a, b| cmp_sortable(a, b, sort_config).reverse());
        let item = vec.pop().expect("NOT EMPTY");
        let mut line = item.line;
        let index = item.ord;
        output.write_all(&line)?;
        line.clear();
        let len = files[index].read_until(b'\n', &mut line)?;
        if len > 0 {
            vec.push(SortableOwned::new(line, index, sort_config));
        }
    }
    Ok(())
}

// ---- sort procedure ---------------------------------------------------------

/// ソートするためのデータ構造
trait Sortable {
    fn key(&self, index: usize) -> &[u8];
    fn ord(&self) -> usize;
}

/// 大きいバッファをソートするための構造体
struct SortableRef<'a> {
    keys: Vec<&'a [u8]>,
    start: usize,
    end: usize,
}
impl<'a> SortableRef<'a> {
    pub fn new(
        (start, end): (usize, usize),
        sort_config: &SortConfig,
        bytes: &'a [u8],
    ) -> SortableRef<'a> {
        let row: Vec<&'a [u8]> = util::trim_newline_ref(&bytes[start..end])
            .split(|&b| b == b'\t')
            .collect();
        let mut keys = vec![];
        for info in sort_config {
            keys.push(row[info.column_index]);
        }
        SortableRef { keys, start, end }
    }
}
impl<'a> Sortable for SortableRef<'a> {
    fn key(&self, index: usize) -> &[u8] {
        self.keys[index]
    }
    fn ord(&self) -> usize {
        self.start
    }
}

/// 各行を保持してソートするための構造体
struct SortableOwned {
    line: Vec<u8>,
    positions: Vec<(usize, usize)>,
    ord: usize,
}
impl SortableOwned {
    pub fn new(line: Vec<u8>, ord: usize, sort_config: &SortConfig) -> Self {
        let mut row = vec![];
        let mut start = 0;
        loop {
            if let Some(pos) = memchr(b'\t', &line[start..]) {
                let end = start + pos;
                row.push((start, end));
                start = end + 1;
            } else {
                row.push((start, line.len()));
                break;
            }
        }
        let mut positions = vec![];
        for info in sort_config {
            positions.push(row[info.column_index]);
        }
        SortableOwned {
            line,
            positions,
            ord,
        }
    }
}
impl Sortable for SortableOwned {
    fn key(&self, index: usize) -> &[u8] {
        let (begin, end) = self.positions[index];
        &self.line[begin..end]
    }
    fn ord(&self) -> usize {
        self.ord
    }
}

/// sort関数の引数に渡すための関数
fn cmp_sortable<S: Sortable>(a: &S, b: &S, sort_config: &SortConfig) -> Ordering {
    for (ix, info) in sort_config.iter().enumerate() {
        match info.sort_type.compare(a.key(ix), b.key(ix)) {
            Ordering::Equal => continue,
            x => return x,
        }
    }
    return a.ord().cmp(&b.ord());
}

/// 文字列ソート、数値としてソートなどの種類
#[derive(Copy, Clone)]
enum SortType {
    String,
    NumberAsc,
    NumberDesc,
}

impl SortType {
    /// 文字列、数値などの種類で比較する
    pub fn compare(&self, a: &[u8], b: &[u8]) -> Ordering {
        match *self {
            SortType::String => a.cmp(b),
            SortType::NumberAsc => Self::numcmp(a, b),
            SortType::NumberDesc => Self::numcmp(b, a),
        }
    }

    /// 数値として比較する
    fn numcmp(a: &[u8], b: &[u8]) -> Ordering {
        let a = Self::to_num(a);
        let b = Self::to_num(b);
        a.partial_cmp(&b).unwrap_or(Ordering::Equal)
    }

    /// バイト列を文字列とみなして数値に変換する
    fn to_num(bytes: &[u8]) -> f64 {
        let string: &str = match str::from_utf8(bytes) {
            Ok(x) => x,
            _ => return 0.0,
        };
        match string.parse() {
            Ok(x) => x,
            _ => 0.0,
        }
    }
}
