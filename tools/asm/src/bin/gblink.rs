#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

use core::str;
use std::{
    collections::HashMap,
    error::Error,
    ffi::OsStr,
    fs::{self, File},
    io::{self, BufReader, BufWriter, ErrorKind, Read, Write},
    mem,
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::Parser;
use gbasm::{
    Expr, ExprNode, Label, Op, PathInt, Pos, Reloc, RelocFlags, Section, SliceInt, StrInt, Sym,
    SymFlags, Tok,
};
use indexmap::IndexMap;
use serde::{de, Deserialize, Deserializer};
use serde_derive::{Deserialize, Serialize};
use tracing::Level;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Object files
    objects: Vec<PathBuf>,

    /// Config file
    #[arg(short, long)]
    config: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Output file for `SYM` debug symbol file
    #[arg(short = 'g', long)]
    debug: Option<PathBuf>,

    /// Output file for VIM tags file
    #[arg(long)]
    tags: Option<PathBuf>,

    /// Pre-defined symbols (repeatable)
    #[arg(short = 'D', long, value_name="KEY1=val", value_parser = gbasm::parse_defines::<String, i32>)]
    define: Vec<(String, i32)>,

    /// One of `TRACE`, `DEBUG`, `INFO`, `WARN`, or `ERROR`
    #[arg(short, long, default_value_t = Level::INFO)]
    log_level: Level,
}

fn main() -> ExitCode {
    let args = Args::parse();
    tracing_subscriber::fmt()
        .with_max_level(args.log_level)
        .with_writer(io::stderr)
        .init();

    if let Err(e) = main_real(args) {
        tracing::error!("{e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn main_real(args: Args) -> Result<(), Box<dyn Error>> {
    let mut config = File::open(&args.config)
        .map_err(|e| format!("cant open file: {}: {e}", args.config.display()))?;
    let mut config_text = String::new();
    config.read_to_string(&mut config_text)?;
    let config: Script = toml::from_str(&config_text)?;

    let mut ld = Ld::new();

    let def_section = ld.str_int.intern("__DEFINES__");
    let def_file = ld.path_int.intern("__DEFINES__");
    let def_unit = ld.str_int.intern("__EXPORT__");
    for (name, val) in &args.define {
        let string = ld.str_int.intern(name);
        ld.syms.insert(
            Label::new(None, string),
            Sym {
                value: Expr::Const(*val),
                unit: def_unit,
                section: def_section,
                pos: Pos {
                    file: def_file,
                    line: 1,
                    col: 1,
                },
                flags: SymFlags::EQU,
            },
        );
    }

    for (name, mem) in &config.memories {
        let name = ld.str_int.intern(&name);
        ld.memories.push(Memory::new(name, mem.start, mem.size));
    }

    for (name, section) in &config.sections {
        let name = ld.str_int.intern(&name);
        if section.align == 0 {
            Err(ld.err(&format!("section \"{name}\" has an invalid alignment of 0",)))?;
        }
        if let Some(memory) = config.memories.get(&section.load) {
            ld.sections.push(Section::new(name));
            match memory.ty {
                MemoryType::RO => match &section.ty {
                    SectionType::RO => {}
                    _ => {
                        Err(ld.err(&format!(
                            "memory \"{}\" is not type-compatible with section \"{name}\"",
                            section.load
                        )))?;
                    }
                },
                MemoryType::RW => match &section.ty {
                    SectionType::RW | SectionType::BSS | SectionType::HI => {}
                    _ => {
                        Err(ld.err(&format!(
                            "memory \"{}\" is not type-compatible with section \"{name}\"",
                            section.load
                        )))?;
                    }
                },
            }
        } else {
            Err(ld.err(&format!(
                "memory \"{}\" is not defined in config",
                section.load
            )))?;
        }
    }

    tracing::trace!("loading objects");
    for object in args.objects {
        let path = fs::canonicalize(object)?;
        let path = path.to_str().unwrap();
        let file = BufReader::new(File::open(path)?);
        ld.load(path, file)?;
    }

    tracing::trace!("relocating sections");
    for (name, section) in &config.sections {
        let Ld {
            ref mut sections,
            ref mut memories,
            ref mut syms,
            ..
        } = ld;
        let memory = memories
            .iter_mut()
            .find(|memory| memory.name == section.load)
            .unwrap();
        let define = section.define;
        let aligned = ((memory.pc + section.align - 1) / section.align) * section.align;
        if memory.pc != aligned {
            tracing::trace!(
                "aligning section \"{name}\" from ${:08X} to ${aligned:08X}",
                memory.pc
            );
        }
        let files = &section.files;
        let section = sections
            .iter_mut()
            .find(|section| section.name == name)
            .unwrap();
        // we update the section pc to be its absolute start address in memory
        section.pc = aligned;
        tracing::trace!("placing section \"{name}\" at ${aligned:08X}");
        // add any files to the section
        if let Some(files) = files {
            for path in files {
                let mut file = File::open(path)
                    .map_err(|e| format!("Could not open {}: {e}", path.display()))?;
                file.read_to_end(&mut section.data)?;
            }
        }
        // check if section is too big
        let section_size = section.data.len() as u32;
        memory.pc = aligned + section_size;
        // the "end" is actually 1 past the last address in the memory
        if memory.pc > memory.end {
            Err(io::Error::new(
                ErrorKind::InvalidData,
                format!(
                    "no room left in memory \"{}\" for section \"{name}\"",
                    memory.name
                ),
            ))?;
        }
        // define start and end in symbol table if wanted
        if define {
            let def_section = ld.str_int.intern("__DEFINES__");
            let def_file = ld.path_int.intern("__DEFINES__");
            let def_unit = ld.str_int.intern("__EXPORT__");
            let start = ld.str_int.intern(&format!("__{name}_START__"));
            syms.insert(
                Label::new(None, start),
                Sym {
                    value: Expr::Const(section.pc as i32),
                    unit: def_unit,
                    section: def_section,
                    pos: Pos {
                        file: def_file,
                        line: 1,
                        col: 1,
                    },
                    flags: SymFlags::EQU,
                },
            );
            let size = ld.str_int.intern(&format!("__{name}_SIZE__"));
            syms.insert(
                Label::new(None, size),
                Sym {
                    value: Expr::Const(section_size as i32),
                    unit: def_unit,
                    section: def_section,
                    pos: Pos {
                        file: def_file,
                        line: 1,
                        col: 1,
                    },
                    flags: SymFlags::EQU,
                },
            );
        }
    }

    for pass in 1..=2 {
        tracing::trace!("symbol table pass {pass}");
        // TODO: perf
        let labels: Vec<Label> = ld.syms.keys().cloned().collect();
        for label in labels {
            let sym = ld.syms.get(&label).unwrap();
            let value = match sym.value {
                Expr::Const(value) => Expr::Const(value),
                Expr::List(expr) => {
                    if let Some(value) = ld.expr_eval(sym.unit, expr, &config.sections) {
                        Expr::Const(value)
                    } else {
                        Expr::List(expr)
                    }
                }
            };
            ld.syms.get_mut(&label).unwrap().value = value;
        }
    }

    tracing::trace!("symbol table validation");
    for (label, sym) in &ld.syms {
        if let Expr::Const(_) = sym.value {
            continue;
        }
        Err(ld.err_in(
            sym.unit,
            &format!(
                "undefined symbol \"{}\"\n\tdeclared at {}:{}:{}",
                label,
                sym.pos.file.display(),
                sym.pos.line,
                sym.pos.col
            ),
        ))?;
    }

    tracing::trace!("linking");
    for i in 0..ld.sections.len() {
        for j in 0..ld.sections[i].relocs.len() {
            let reloc = ld.sections[i].relocs[j];
            let value = if let Some(value) = ld.expr_eval(reloc.unit, reloc.value, &config.sections)
            {
                value
            } else {
                Err(ld.err_in(
                    reloc.unit,
                    &format!(
                        "expression cannot be solved\n\tdefined at {}:{}:{}",
                        reloc.pos.file.display(),
                        reloc.pos.line,
                        reloc.pos.col
                    ),
                ))?
            };
            match reloc.width {
                1 => {
                    if (value as u32) > (u8::MAX as u32) {
                        // HI relocations are OK if they reloc inside of a HI section
                        if (reloc.flags & RelocFlags::HI) != 0 {
                            if ld
                                .sections
                                .iter()
                                .find(|sec| {
                                    // find HI section that holds this address
                                    config
                                        .sections
                                        .iter()
                                        .find(|(name, section)| {
                                            matches!(section.ty, SectionType::HI)
                                                && (sec.name == *name)
                                        })
                                        .is_some()
                                        && ((value as u32) >= sec.pc)
                                        && ((value as u32) < (sec.pc + (sec.data.len() as u32)))
                                })
                                .is_none()
                            {
                                Err(ld.err_in(
                                    reloc.unit,
                                    &format!(
                                        "expression >1 byte\n\tdefined at {}:{}:{}",
                                        reloc.pos.file.display(),
                                        reloc.pos.line,
                                        reloc.pos.col
                                    ),
                                ))?;
                            }
                        } else {
                            Err(ld.err_in(
                                reloc.unit,
                                &format!(
                                    "expression >1 byte\n\tdefined at {}:{}:{}",
                                    reloc.pos.file.display(),
                                    reloc.pos.line,
                                    reloc.pos.col
                                ),
                            ))?;
                        }
                    }
                    // validate the rst
                    if (reloc.flags & RelocFlags::RST) != 0 {
                        let value = match value {
                            0x00 => 0xC7,
                            0x08 => 0xCF,
                            0x10 => 0xD7,
                            0x18 => 0xDF,
                            0x20 => 0xE7,
                            0x28 => 0xEF,
                            0x30 => 0xF7,
                            0x38 => 0xFF,
                            _ => Err(ld.err_in(
                                reloc.unit,
                                &format!(
                                    "expression not rst\n\tdefined at {}:{}:{}",
                                    reloc.pos.file.display(),
                                    reloc.pos.line,
                                    reloc.pos.col
                                ),
                            ))?,
                        };
                        ld.sections[i].data[reloc.offset] = value as u8;
                        continue;
                    }
                    ld.sections[i].data[reloc.offset] = value as u8;
                }
                2 => {
                    // TODO check if source and dest bank are different
                    // JP within a bank or to bank0 is always fine

                    if (value as u32) > (u16::MAX as u32) {
                        // TODO handle JP reloc flag?
                        Err(ld.err_in(
                            reloc.unit,
                            &format!(
                                "expression >2 bytes\n\tdefined at {}:{}:{}",
                                reloc.pos.file.display(),
                                reloc.pos.line,
                                reloc.pos.col
                            ),
                        ))?;
                    }
                    ld.sections[i].data[reloc.offset] = ((value as u32) >> 0) as u8;
                    ld.sections[i].data[reloc.offset + 1] = ((value as u32) >> 8) as u8;
                }
                _ => unreachable!(),
            }
        }
    }

    let mut output: Box<dyn Write> = match args.output {
        Some(path) => Box::new(BufWriter::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&path)
                .map_err(|e| format!("cant open file: {}: {e}", path.display()))?,
        )),
        None => Box::new(BufWriter::new(io::stdout())),
    };

    tracing::trace!("writing");
    for (mem_name, memory) in &config.memories {
        for (sec_name, section) in &config.sections {
            if &section.load != mem_name {
                continue;
            }
            if matches!(section.ty, SectionType::BSS | SectionType::HI) {
                continue;
            }
            let section = &ld.sections.iter().find(|sec| sec.name == sec_name).unwrap();
            tracing::trace!(
                "writing {} bytes of section \"{sec_name}\" in memory \"{mem_name}\"",
                section.data.len()
            );
            output.write_all(&section.data)?;
        }
        if let Some(value) = memory.fill {
            let mem = &ld.memories.iter().find(|mem| mem.name == mem_name).unwrap();
            tracing::trace!(
                "filling {} bytes of memory \"{mem_name}\" with ${value:02X}",
                mem.end - mem.pc,
            );
            for _ in mem.pc..mem.end {
                output.write(&[value])?;
            }
        }
    }

    if let Some(path) = args.debug {
        tracing::trace!("writing debug symbols");
        let mut file = BufWriter::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&path)
                .map_err(|e| format!("cant open file: {}: {e}", path.display()))?,
        );
        for (label, sym) in &ld.syms {
            if (sym.flags & SymFlags::EQU) != 0 {
                continue;
            }
            if let Expr::Const(value) = sym.value {
                let tags = &config.sections[sym.section].tags.as_ref();
                if let Some(tags) = tags {
                    if let Some(bank) = tags.get("bank") {
                        writeln!(file, "{bank:02X}:{value:04X} {}", label)?;
                        continue;
                    }
                }
                writeln!(file, "00:{value:04X} {}", label)?;
            }
        }
    }

    if let Some(path) = args.tags {
        tracing::trace!("writing tags file");
        let mut file = BufWriter::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&path)
                .map_err(|e| format!("cant open file: {}: {e}", path.display()))?,
        );
        for (label, sym) in &ld.syms {
            if sym.pos.file == Path::new("__DEFINES__") {
                continue;
            }
            writeln!(
                file,
                "{}\t{}\t{}",
                label,
                sym.pos.file.display(),
                sym.pos.line
            )?;
        }
    }

    Ok(())
}

struct Ld<'a> {
    str_int: StrInt,
    path_int: PathInt,
    expr_int: SliceInt<ExprNode<'a>>,
    sections: Vec<Section<'a>>,
    memories: Vec<Memory<'a>>,
    syms: HashMap<Label<'a>, Sym<'a>>,
}

impl<'a> Ld<'a> {
    fn new() -> Self {
        Self {
            str_int: StrInt::new(),
            path_int: PathInt::new(),
            expr_int: SliceInt::new(),
            sections: Vec::new(),
            memories: Vec::new(),
            syms: HashMap::new(),
        }
    }

    fn err(&self, msg: &str) -> io::Error {
        io::Error::new(ErrorKind::InvalidData, msg)
    }

    fn err_in(&self, file: &str, msg: &str) -> io::Error {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("in file: {file}: {msg}"),
        )
    }

    fn read_int<R: Read, T: FromLeBytes<Buf = [u8; mem::size_of::<T>()]>>(
        &self,
        reader: &mut R,
    ) -> io::Result<T>
    where
        T::Buf: Default,
    {
        let mut buf = T::Buf::default();
        reader.read_exact(&mut buf)?;
        Ok(T::from_le_bytes(buf.into()))
    }

    fn load<R: Read>(&mut self, file: &str, mut reader: R) -> io::Result<()> {
        let mut magic = [0u8; 7];
        reader.read_exact(&mut magic)?;
        if &magic != b"gbasm01" {
            return Err(self.err_in(file, "bad magic"));
        }
        // fill up a temporary string table
        let mut str_int: StrInt = StrInt::new();
        {
            let str_len: u32 = self.read_int(&mut reader)?;
            let mut bytes = Vec::new();
            bytes.extend((0..str_len).map(|_| 0));
            reader.read_exact(&mut bytes)?;
            str_int.intern(unsafe { str::from_utf8_unchecked(&bytes) });
        }
        // fill up a temporary path table
        let mut path_int: PathInt = PathInt::new();
        {
            let str_len: u32 = self.read_int(&mut reader)?;
            let mut bytes = Vec::new();
            bytes.extend((0..str_len).map(|_| 0));
            reader.read_exact(&mut bytes)?;
            path_int.intern(unsafe { OsStr::from_encoded_bytes_unchecked(&bytes) });
        }
        // and a temporary expression table
        let mut expr_int: SliceInt<ExprNode<'_>> = SliceInt::new();
        {
            let expr_len: u32 = self.read_int(&mut reader)?;
            let mut exprs = Vec::new();
            for _ in 0..expr_len {
                let ty: u8 = self.read_int(&mut reader)?;
                match ty {
                    0 => {
                        let value: i32 = self.read_int(&mut reader)?;
                        exprs.push(ExprNode::Const(value));
                    }
                    1 => {
                        let ty: u8 = self.read_int(&mut reader)?;
                        match ty {
                            0 => {
                                let value: u32 = self.read_int(&mut reader)?;
                                let value = char::from_u32(value).unwrap();
                                exprs.push(ExprNode::Op(Op::Binary(Tok(value))));
                            }
                            1 => {
                                let value: u32 = self.read_int(&mut reader)?;
                                let value = char::from_u32(value).unwrap();
                                exprs.push(ExprNode::Op(Op::Unary(Tok(value))));
                            }
                            _ => return Err(self.err_in(file, "malformed expression table")),
                        }
                    }
                    2 => {
                        let ty: u8 = self.read_int(&mut reader)?;
                        match ty {
                            0 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let scope = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let scope = self.str_int.intern(scope);
                                let string = self.str_int.intern(string);
                                exprs.push(ExprNode::Label(Label::new(Some(scope), string)));
                            }
                            1 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let string = self.str_int.intern(string);
                                exprs.push(ExprNode::Label(Label::new(None, string)));
                            }
                            _ => return Err(self.err_in(file, "malformed expression table")),
                        }
                    }
                    3 => {
                        let ty: u8 = self.read_int(&mut reader)?;
                        match ty {
                            0 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let scope = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let tag = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let scope = self.str_int.intern(scope);
                                let string = self.str_int.intern(string);
                                let tag = self.str_int.intern(tag);
                                exprs.push(ExprNode::Tag(Label::new(Some(scope), string), tag));
                            }
                            1 => {
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let string = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let index: u32 = self.read_int(&mut reader)?;
                                let len: u32 = self.read_int(&mut reader)?;
                                let tag = str_int
                                    .slice((index as usize)..((index as usize) + (len as usize)))
                                    .unwrap();
                                let string = self.str_int.intern(string);
                                let tag = self.str_int.intern(tag);
                                exprs.push(ExprNode::Tag(Label::new(None, string), tag));
                            }
                            _ => return Err(self.err_in(file, "malformed expression table")),
                        }
                    }
                    4 => {
                        let index: u32 = self.read_int(&mut reader)?;
                        let len: u32 = self.read_int(&mut reader)?;
                        let expr_section = str_int
                            .slice((index as usize)..((index as usize) + (len as usize)))
                            .unwrap();
                        let pc: u32 = self.read_int(&mut reader)?;
                        let expr_section = self.str_int.intern(expr_section);
                        let pc = if let Some(section) =
                            self.sections.iter().find(|sec| sec.name == expr_section)
                        {
                            pc + section.pc
                        } else {
                            return Err(self.err_in(
                                file,
                                &format!("section \"{expr_section}\" is not defined in config"),
                            ));
                        };
                        exprs.push(ExprNode::Addr(expr_section, pc));
                    }
                    _ => return Err(self.err_in(file, "malformed expression table")),
                }
            }
            expr_int.storages.push(exprs);
        }
        // time to start filling the global symbol table
        let syms_len: u32 = self.read_int(&mut reader)?;
        for _ in 0..syms_len {
            let ty: u8 = self.read_int(&mut reader)?;
            let label = match ty {
                0 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let scope = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let string = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let scope = self.str_int.intern(scope);
                    let string = self.str_int.intern(string);
                    Label::new(Some(scope), string)
                }
                1 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let string = str_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let string = self.str_int.intern(string);
                    Label::new(None, string)
                }
                _ => return Err(self.err_in(file, "malformed symbol table")),
            };
            let ty: u8 = self.read_int(&mut reader)?;
            let value = match ty {
                0 => {
                    let value: i32 = self.read_int(&mut reader)?;
                    Expr::Const(value)
                }
                1 => {
                    let index: u32 = self.read_int(&mut reader)?;
                    let len: u32 = self.read_int(&mut reader)?;
                    let expr = expr_int
                        .slice((index as usize)..((index as usize) + (len as usize)))
                        .unwrap();
                    let expr = self.expr_int.intern(expr);
                    Expr::List(expr)
                }
                _ => return Err(self.err_in(file, "malformed symbol table")),
            };
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let unit = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            // hide static symbols under the object file name
            let unit = if unit == "__STATIC__" {
                self.str_int.intern(file)
            } else {
                self.str_int.intern(unit)
            };
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let sym_section = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            let sym_section = self.str_int.intern(sym_section);
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let sym_file = path_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            let sym_file = self.path_int.intern(sym_file);
            let line: u32 = self.read_int(&mut reader)?;
            let col: u32 = self.read_int(&mut reader)?;
            let pos = Pos {
                file: sym_file,
                line,
                col,
            };
            let flags: u8 = self.read_int(&mut reader)?;
            // duplicate exported symbol?
            if let Some(sym) = self.syms.get(&label) {
                if sym.unit == unit {
                    return Err(self.err_in(file, &format!("duplicate exported symbol \"{label}\" found\n\tdefined at {}:{}:{}\n\tagain at {}:{line}:{col}",sym_file.display(),  sym.pos.file.display(), sym.pos.line, sym.pos.col)));
                }
            }
            self.syms.insert(
                label,
                Sym {
                    value,
                    unit,
                    section: sym_section,
                    pos,
                    flags,
                },
            );
        }
        // add to sections
        let sections_len: u32 = self.read_int(&mut reader)?;
        for _ in 0..sections_len {
            let index: u32 = self.read_int(&mut reader)?;
            let len: u32 = self.read_int(&mut reader)?;
            let name = str_int
                .slice((index as usize)..((index as usize) + (len as usize)))
                .unwrap();
            let name = self.str_int.intern(name);
            tracing::trace!("loading section \"{name}\"");
            let section = if let Some(section) = self.sections.iter().find(|sec| sec.name == name) {
                section
            } else {
                return Err(self.err_in(
                    file,
                    &format!("section \"{name}\" is not defined in config"),
                ));
            };
            let data_len: u32 = self.read_int(&mut reader)?;
            let mut data = Vec::new();
            data.extend((0..data_len).map(|_| 0));
            reader.read_exact(&mut data)?;
            // TODO this seems messy,
            let mut relocs = Vec::new();
            let relocs_len: u32 = self.read_int(&mut reader)?;
            for _ in 0..relocs_len {
                let offset: u32 = self.read_int(&mut reader)?;
                // place the offset relative to the start of the section
                let offset = (offset as usize) + (section.pc as usize);
                let width: u8 = self.read_int(&mut reader)?;
                let index: u32 = self.read_int(&mut reader)?;
                let len: u32 = self.read_int(&mut reader)?;
                let expr = expr_int
                    .slice((index as usize)..((index as usize) + (len as usize)))
                    .unwrap();
                let value = self.expr_int.intern(expr);
                let index: u32 = self.read_int(&mut reader)?;
                let len: u32 = self.read_int(&mut reader)?;
                let unit = str_int
                    .slice((index as usize)..((index as usize) + (len as usize)))
                    .unwrap();
                let index: u32 = self.read_int(&mut reader)?;
                let len: u32 = self.read_int(&mut reader)?;
                let reloc_file = path_int
                    .slice((index as usize)..((index as usize) + (len as usize)))
                    .unwrap();
                let unit = if unit == "__STATIC__" {
                    self.str_int.intern(file)
                } else {
                    self.str_int.intern("__EXPORT__")
                };
                let reloc_file = self.path_int.intern(reloc_file);
                let line: u32 = self.read_int(&mut reader)?;
                let col: u32 = self.read_int(&mut reader)?;
                let pos = Pos {
                    file: reloc_file,
                    line,
                    col,
                };
                let flags: u8 = self.read_int(&mut reader)?;
                relocs.push(Reloc {
                    offset,
                    width,
                    value,
                    unit,
                    pos,
                    flags,
                });
            }
            // extend section
            if let Some(section) = self.sections.iter_mut().find(|sec| sec.name == name) {
                tracing::trace!("extending section \"{name}\" by {} bytes", data.len());
                section.data.extend(&data);
                section.relocs.extend(relocs);
                section.pc += data.len() as u32;
            } else {
                return Err(self.err_in(
                    file,
                    &format!("section \"{name}\" is not defined in config"),
                ));
            };
        }

        Ok(())
    }

    fn expr_eval(
        &self,
        unit: &'_ str,
        expr: &[ExprNode<'_>],
        sections: &IndexMap<String, ConfigSection>,
    ) -> Option<i32> {
        let mut scratch = Vec::new();
        for node in expr.iter() {
            match *node {
                ExprNode::Const(value) => scratch.push(value),
                ExprNode::Label(label) => {
                    let sym = self.syms.get(&label)?;
                    if (sym.unit != unit) && (sym.unit != "__EXPORT__") {
                        return None;
                    }
                    match sym.value {
                        Expr::Const(value) => scratch.push(value),
                        Expr::List(expr) => {
                            // expand the sub-expression recursively
                            scratch.push(self.expr_eval(unit, expr, sections)?);
                        }
                    }
                }
                ExprNode::Tag(label, tag) => {
                    let sym = self.syms.get(&label)?;
                    if (sym.unit != unit) && (sym.unit != "__EXPORT__") {
                        return None;
                    }
                    let tags = sections[sym.section].tags.as_ref();
                    let value = tags?.get(tag)?;
                    scratch.push(*value);
                }
                ExprNode::Op(op) => {
                    let rhs = scratch.pop().unwrap();
                    match op {
                        Op::Unary(Tok::PLUS) => scratch.push(rhs),
                        Op::Unary(Tok::MINUS) => scratch.push(-rhs),
                        Op::Unary(Tok::TILDE) => scratch.push(!rhs),
                        Op::Unary(Tok::BANG) => scratch.push((rhs == 0) as i32),
                        Op::Unary(Tok::LT) => scratch.push(((rhs as u32) & 0xFF) as i32),
                        Op::Unary(Tok::GT) => scratch.push((((rhs as u32) & 0xFF00) >> 8) as i32),
                        Op::Unary(Tok::CARET) => {
                            scratch.push((((rhs as u32) & 0xFF0000) >> 16) as i32)
                        }
                        Op::Binary(tok) => {
                            let lhs = scratch.pop().unwrap();
                            match tok {
                                Tok::PLUS => scratch.push(lhs.wrapping_add(rhs)),
                                Tok::MINUS => scratch.push(lhs.wrapping_sub(rhs)),
                                Tok::STAR => scratch.push(lhs.wrapping_mul(rhs)),
                                Tok::SOLIDUS => scratch.push(lhs.wrapping_div(rhs)),
                                Tok::MODULUS => scratch.push(lhs.wrapping_rem(rhs)),
                                Tok::ASL => scratch.push(lhs.wrapping_shl(rhs as u32)),
                                Tok::ASR => scratch.push(lhs.wrapping_shr(rhs as u32)),
                                Tok::LSR => {
                                    scratch.push((lhs as u32).wrapping_shl(rhs as u32) as i32)
                                }
                                Tok::LT => scratch.push((lhs < rhs) as i32),
                                Tok::LTE => scratch.push((lhs <= rhs) as i32),
                                Tok::GT => scratch.push((lhs > rhs) as i32),
                                Tok::GTE => scratch.push((lhs >= rhs) as i32),
                                Tok::DEQU => scratch.push((lhs == rhs) as i32),
                                Tok::NEQU => scratch.push((lhs != rhs) as i32),
                                Tok::AMP => scratch.push(lhs & rhs),
                                Tok::PIPE => scratch.push(lhs | rhs),
                                Tok::CARET => scratch.push(lhs ^ rhs),
                                Tok::AND => scratch.push(((lhs != 0) && (rhs != 0)) as i32),
                                Tok::OR => scratch.push(((lhs != 0) || (rhs != 0)) as i32),
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                ExprNode::Addr(section, pc) => {
                    let section = self
                        .sections
                        .iter()
                        .find(|sec| sec.name == section)
                        .unwrap();
                    scratch.push((pc + section.pc) as i32);
                }
            }
        }
        scratch.last().copied()
    }
}

struct Memory<'a> {
    name: &'a str,
    pc: u32,
    end: u32,
}

impl<'a> Memory<'a> {
    fn new(name: &'a str, pc: u32, len: u32) -> Self {
        Self {
            name,
            pc,
            end: pc + len,
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum MemoryType {
    RO,
    RW,
}

#[derive(Serialize, Deserialize)]
struct ConfigMemory {
    #[serde(deserialize_with = "deserialize_bases_u32")]
    start: u32,

    #[serde(deserialize_with = "deserialize_bases_u32")]
    size: u32,

    #[serde(rename = "type")]
    ty: MemoryType,

    #[serde(default, deserialize_with = "deserialize_bases_u8")]
    fill: Option<u8>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum SectionType {
    RO,
    RW,
    BSS,
    HI,
}

fn one() -> u32 {
    1
}

#[derive(Serialize, Deserialize)]
struct ConfigSection {
    load: String,

    #[serde(rename = "type")]
    ty: SectionType,

    #[serde(default, deserialize_with = "deserialize_tags")]
    tags: Option<HashMap<String, i32>>,

    #[serde(default)]
    define: bool,

    #[serde(default = "one", deserialize_with = "deserialize_bases_u32")]
    align: u32,

    #[serde(default)]
    files: Option<Vec<PathBuf>>,
}

#[derive(Serialize, Deserialize)]
struct Script {
    #[serde(rename = "MEMORY")]
    memories: IndexMap<String, ConfigMemory>,

    #[serde(rename = "SECTIONS")]
    sections: IndexMap<String, ConfigSection>,
}

fn deserialize_tags<'de, D>(deserializer: D) -> Result<Option<HashMap<String, i32>>, D::Error>
where
    D: Deserializer<'de>,
{
    let string_map = Option::<HashMap<String, String>>::deserialize(deserializer)?;
    if let Some(string_map) = string_map {
        let mut map = HashMap::new();
        for (name, buf) in string_map {
            let value = if buf.starts_with('$') {
                i32::from_str_radix(&buf[1..], 16).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 16 value: {e}"))
                })?
            } else if buf.starts_with('%') {
                i32::from_str_radix(&buf[1..], 2).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 2 value: {e}"))
                })?
            } else {
                i32::from_str_radix(&buf, 10).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 10 value: {e}"))
                })?
            };
            map.insert(name, value);
        }
        return Ok(Some(map));
    }
    Ok(None)
}

fn deserialize_bases_u32<'de, D>(deserializer: D) -> Result<u32, D::Error>
where
    D: Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?;
    if buf.starts_with('$') {
        u32::from_str_radix(&buf[1..], 16)
            .map_err(|e| de::Error::custom(format!("{buf} is not a valid base 16 address: {e}")))
    } else if buf.starts_with('%') {
        u32::from_str_radix(&buf[1..], 2)
            .map_err(|e| de::Error::custom(format!("{buf} is not a valid base 2 address: {e}")))
    } else {
        u32::from_str_radix(&buf, 10)
            .map_err(|e| de::Error::custom(format!("{buf} is not a valid base 10 address: {e}")))
    }
}

fn deserialize_bases_u8<'de, D>(deserializer: D) -> Result<Option<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    Option::<String>::deserialize(deserializer)?
        .map(|buf| {
            if buf.starts_with('$') {
                u8::from_str_radix(&buf[1..], 16).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 16 value: {e}"))
                })
            } else if buf.starts_with('%') {
                u8::from_str_radix(&buf[1..], 2).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 2 value: {e}"))
                })
            } else {
                u8::from_str_radix(&buf, 10).map_err(|e| {
                    de::Error::custom(format!("{buf} is not a valid base 10 value: {e}"))
                })
            }
        })
        .transpose()
}

trait FromLeBytes {
    type Buf;

    fn from_le_bytes(buf: Self::Buf) -> Self;
}

macro_rules! impl_le_bytes (( $($int:ident),* ) => {
    $(
        impl FromLeBytes for $int {
            type Buf = [u8; mem::size_of::<Self>()];

            fn from_le_bytes(buf: Self::Buf) -> Self {
                Self::from_le_bytes(buf)
            }
        }
    )*
});

impl_le_bytes!(u8, u32, i32, usize);
