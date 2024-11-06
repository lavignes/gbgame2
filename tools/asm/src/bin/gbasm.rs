use std::{
    collections::{HashMap, HashSet, VecDeque},
    error::Error,
    fmt::Write as FmtWrite,
    fs::{self, File},
    io::{self, ErrorKind, Read, Seek, Write},
    path::{Path, PathBuf},
    process::ExitCode,
    str,
};

use clap::Parser;
use gbasm::{
    Expr, ExprNode, Label, Op, PathInt, Pos, Reloc, RelocFlags, Section, SliceInt, StrInt, Sym,
    SymFlags, Tok,
};
use tracing::Level;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assembly source file
    source: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Pre-defined symbols (repeatable)
    #[arg(short = 'D', long, value_name="KEY1=val", value_parser = gbasm::parse_defines::<String, i32>)]
    define: Vec<(String, i32)>,

    /// Search directories for included files
    #[arg(short = 'I', long)]
    include: Vec<PathBuf>,

    /// Output makefile dependencies lines instead of object file
    #[arg(short = 'M')]
    make_depend: bool,

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
    let input = fs::canonicalize(args.source.clone())?;
    let file = File::open(&input).map_err(|e| format!("cant open file: {e}"))?;
    let lexer = Lexer::new(file, &input);
    let mut asm = Asm::new(lexer, &args.include);

    asm.path_int.intern(&input); // dont forget to intern the input
    let def_section = asm.str_int.intern("__DEFINES__");
    let def_file = asm.path_int.intern("__DEFINES__");
    let def_unit = asm.str_int.intern("__STATIC__");
    for (name, val) in &args.define {
        let string = asm.str_int.intern(name);
        asm.syms.insert(
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

    tracing::trace!("starting pass 1");
    asm.pass()?;

    tracing::trace!("starting pass 2");
    asm.rewind()?;
    asm.pass()?;

    let mut output: Box<dyn Write> = match args.output.clone() {
        Some(path) => Box::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| format!("cant open file: {e}"))?,
        ),
        None => Box::new(io::stdout()),
    };

    tracing::trace!("writing");

    if args.make_depend {
        let mut obj = args.source.clone();
        obj.set_extension("o");
        for include in asm.included {
            writeln!(output, "{}: {}", obj.display(), include.display())?;
        }
    } else {
        output.write_all("gbasm01".as_bytes())?;
        output.write_all(&(asm.str_int.len() as u32).to_le_bytes())?;
        for s in asm.str_int.iter() {
            output.write_all(s.as_bytes())?;
        }
        output.write_all(&(asm.path_int.len() as u32).to_le_bytes())?;
        for p in asm.path_int.iter() {
            output.write_all(p.as_encoded_bytes())?;
        }
        output.write_all(&(asm.expr_int.len() as u32).to_le_bytes())?;
        for expr in asm.expr_int.iter().flatten() {
            match expr {
                ExprNode::Const(val) => {
                    output.write_all(&[0])?;
                    output.write_all(&val.to_le_bytes())?;
                }
                ExprNode::Op(op) => {
                    output.write_all(&[1])?;
                    match op {
                        Op::Binary(tok) => {
                            output.write_all(&[0])?;
                            output.write_all(&(tok.0 as u32).to_le_bytes())?;
                        }
                        Op::Unary(tok) => {
                            output.write_all(&[1])?;
                            output.write_all(&(tok.0 as u32).to_le_bytes())?;
                        }
                    }
                }
                ExprNode::Label(label) => {
                    output.write_all(&[2])?;
                    if let Some(scope) = label.scope {
                        output.write_all(&[0])?;
                        let index = asm.str_int.offset(scope).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(scope.len() as u32).to_le_bytes())?;
                    } else {
                        output.write_all(&[1])?;
                    }
                    let index = asm.str_int.offset(label.string).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(label.string.len() as u32).to_le_bytes())?;
                }
                ExprNode::Tag(label, tag) => {
                    output.write_all(&[3])?;
                    if let Some(scope) = label.scope {
                        output.write_all(&[0])?;
                        let index = asm.str_int.offset(scope).unwrap();
                        output.write_all(&(index as u32).to_le_bytes())?;
                        output.write_all(&(scope.len() as u32).to_le_bytes())?;
                    } else {
                        output.write_all(&[1])?;
                    }
                    let index = asm.str_int.offset(label.string).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(label.string.len() as u32).to_le_bytes())?;
                    let index = asm.str_int.offset(tag).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(tag.len() as u32).to_le_bytes())?;
                }
                ExprNode::Addr(section, pc) => {
                    output.write_all(&[4])?;
                    let index = asm.str_int.offset(section).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(section.len() as u32).to_le_bytes())?;
                    output.write_all(&pc.to_le_bytes())?;
                }
            }
        }
        output.write_all(&(asm.syms.len() as u32).to_le_bytes())?;
        for (label, sym) in asm.syms {
            if let Some(scope) = label.scope {
                output.write_all(&[0])?;
                let index = asm.str_int.offset(scope).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(&(scope.len() as u32).to_le_bytes())?;
            } else {
                output.write_all(&[1])?;
            }
            let index = asm.str_int.offset(label.string).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(label.string.len() as u32).to_le_bytes())?;
            match sym.value {
                Expr::Const(value) => {
                    output.write_all(&[0])?;
                    output.write_all(&value.to_le_bytes())?;
                }
                Expr::List(expr) => {
                    output.write_all(&[1])?;
                    let index = asm.expr_int.offset(expr).unwrap();
                    output.write_all(&(index as u32).to_le_bytes())?;
                    output.write_all(&(expr.len() as u32).to_le_bytes())?;
                }
            }
            let index = asm.str_int.offset(sym.unit).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(sym.unit.len() as u32).to_le_bytes())?;
            let index = asm.str_int.offset(sym.section).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(sym.section.len() as u32).to_le_bytes())?;
            let index = asm.path_int.offset(sym.pos.file).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(
                &(sym.pos.file.as_os_str().as_encoded_bytes().len() as u32).to_le_bytes(),
            )?;
            output.write_all(&(sym.pos.line as u32).to_le_bytes())?;
            output.write_all(&(sym.pos.col as u32).to_le_bytes())?;
            output.write_all(&[sym.flags])?;
        }
        let count = asm
            .sections
            .iter()
            .filter(|section| !section.data.is_empty())
            .count();
        output.write_all(&(count as u32).to_le_bytes())?;
        for section in asm.sections {
            if section.data.is_empty() {
                continue;
            }
            let index = asm.str_int.offset(section.name).unwrap();
            output.write_all(&(index as u32).to_le_bytes())?;
            output.write_all(&(section.name.len() as u32).to_le_bytes())?;
            output.write_all(&(section.data.len() as u32).to_le_bytes())?;
            output.write_all(&section.data)?;
            output.write_all(&(section.relocs.len() as u32).to_le_bytes())?;
            for reloc in section.relocs {
                output.write_all(&(reloc.offset as u32).to_le_bytes())?;
                output.write_all(&reloc.width.to_le_bytes())?;
                let index = asm.expr_int.offset(reloc.value).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(&(reloc.value.len() as u32).to_le_bytes())?;
                let index = asm.str_int.offset(reloc.unit).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(&(reloc.unit.len() as u32).to_le_bytes())?;
                let index = asm.path_int.offset(reloc.pos.file).unwrap();
                output.write_all(&(index as u32).to_le_bytes())?;
                output.write_all(
                    &(reloc.pos.file.as_os_str().as_encoded_bytes().len() as u32).to_le_bytes(),
                )?;
                output.write_all(&(reloc.pos.line as u32).to_le_bytes())?;
                output.write_all(&(reloc.pos.col as u32).to_le_bytes())?;
                output.write_all(&[reloc.flags])?;
            }
        }
    }

    Ok(())
}

struct Asm<'a> {
    toks: Vec<Box<dyn TokStream<'a> + 'a>>,
    str_int: StrInt,
    path_int: PathInt,
    tok_int: SliceInt<(Pos<'a>, MacroTok<'a>)>,
    loop_int: SliceInt<(Pos<'a>, LoopTok<'a>)>,
    expr_int: SliceInt<ExprNode<'a>>,
    section: usize,
    sections: Vec<Section<'a>>,
    syms: HashMap<Label<'a>, Sym<'a>>,
    scope: Option<&'a str>,
    emit: bool,
    if_level: usize,

    includes: Vec<&'a Path>,     // from args
    included: HashSet<&'a Path>, // for tracking usage with -M flag

    macros: HashMap<&'a str, Macro<'a>>,
    unique: usize, // unique id generator for macro invocations

    // expr parsing
    expr_buffer: Vec<ExprNode<'a>>,
    operator_buffer: Vec<Op>,
}

impl<'a> Asm<'a> {
    fn new<R: Read + Seek + 'static>(lexer: Lexer<'a, R>, includes: &[PathBuf]) -> Self {
        let mut str_int = StrInt::new();
        let code = str_int.intern("__CODE__");
        let mut path_int = PathInt::new();
        let includes = includes.iter().map(|path| path_int.intern(path)).collect();
        Self {
            toks: vec![Box::new(lexer)],
            str_int,
            tok_int: SliceInt::new(),
            path_int: PathInt::new(),
            loop_int: SliceInt::new(),
            expr_int: SliceInt::new(),
            section: 0,
            sections: vec![Section::new(code)],
            syms: HashMap::new(),
            scope: None,
            emit: false,
            if_level: 0,

            includes,
            included: HashSet::new(),

            macros: HashMap::new(),
            unique: 0,

            expr_buffer: Vec::new(),
            operator_buffer: Vec::new(),
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.toks.last_mut().unwrap().rewind()?;
        self.section = 0;
        self.sections = vec![Section::new(self.str_int.intern("__CODE__"))];
        self.scope = None;
        self.emit = true;
        self.if_level = 0;
        self.unique = 0;
        Ok(())
    }

    fn pass(&mut self) -> io::Result<()> {
        while self.peek()? != Tok::EOF {
            // skip newlines
            if self.peek()? == Tok::NEWLINE {
                self.eat();
                continue;
            }
            // special case, setting the relative PC
            if self.peek()? == Tok::STAR {
                self.eat();
                if self.peek()? != Tok::EQU {
                    return Err(self.err("expected ="));
                }
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                self.set_pc(self.range_u16(expr)? as u32);
                self.eol()?;
                continue;
            }
            if self.peek()? == Tok::ID {
                let mne = MNEMONICS.iter().find(|mne| self.str_like(mne.0));
                // is this a label?
                if mne.is_none() {
                    // is this a macro?
                    if let Some((name, mac)) = self.macros.get_key_value(self.str()) {
                        self.macroinvoke(name, *mac)?;
                        continue;
                    }
                    let pos = self.tok().pos();
                    let string = self.str_intern();
                    let label = if let Some(index) = string.find('.') {
                        let (scope, string) = string.split_at(index);
                        if scope.is_empty() {
                            Label::new(self.scope, string)
                        } else {
                            Label::new(Some(scope), string)
                        }
                    } else {
                        Label::new(None, string)
                    };
                    self.eat();

                    if self.syms.contains_key(&label) {
                        // allowed to redef during second pass
                        // TODO should test if label value didnt change
                        // TODO allow variable kinds that are redefinable
                        if !self.emit {
                            return Err(self.err("symbol already defined"));
                        }
                    } else {
                        // save in the symbol table with temporary value
                        let unit = self.str_int.intern("__STATIC__");
                        let section = self.sections[self.section].name;
                        self.syms.insert(
                            label,
                            Sym {
                                value: Expr::Const(0),
                                unit,
                                section,
                                pos,
                                flags: 0,
                            },
                        );
                    }

                    match self.peek()? {
                        Tok::COLON => {
                            self.eat();
                        }
                        // export colons
                        Tok::DCOLON => {
                            self.eat();
                            if self.emit {
                                let unit = self.str_int.intern("__EXPORT__");
                                if let Some(sym) = self.syms.get_mut(&label) {
                                    if sym.unit == unit {
                                        return Err(self.err("symbol is already exported"));
                                    }
                                    sym.unit = unit;
                                }
                            }
                        }
                        Tok::EQU => {
                            self.eat();
                            let expr = self.expr()?;
                            // equ's must always be const, either on the first or second pass
                            if self.emit {
                                let expr = self.const_expr(expr)?;
                                let sym = self.syms.get_mut(&label).unwrap();
                                sym.value = Expr::Const(expr);
                                sym.flags = SymFlags::EQU;
                            } else if let Expr::Const(expr) = expr {
                                let sym = self.syms.get_mut(&label).unwrap();
                                sym.value = Expr::Const(expr);
                                sym.flags = SymFlags::EQU;
                            } else {
                                // we couldn't evaluate this yet, so remove it
                                self.syms.remove(&label);
                            }
                            self.eol()?;
                            continue;
                        }
                        _ => return Err(self.err("expected ':' or '='")),
                    }
                    // set the scope
                    if !string.starts_with(".") {
                        self.scope.replace(string);
                    }

                    // otherwise it is a pointer to the current PC
                    let section = self.sections[self.section].name;
                    self.syms.get_mut(&label).unwrap().value =
                        Expr::List(self.expr_int.intern(&[ExprNode::Addr(section, self.pc())]));
                    continue;
                }
                if mne.is_none() {
                    return Err(self.err("unrecognized instruction"));
                }
                self.eat();
                self.operand(*mne.unwrap())?;
                self.eol()?;
                continue;
            }
            self.directive()?;
        }
        Ok(())
    }

    fn operand(&mut self, mne: Mne) -> io::Result<()> {
        match mne {
            Mne::LD => match self.peek()? {
                Tok::A => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        Tok::LBRACKET => {
                            self.eat();
                            match self.peek()? {
                                tok @ (Tok::BC | Tok::DE | Tok::HL | Tok::C) => {
                                    self.eat();
                                    self.expect(Tok::RBRACKET)?;
                                    if self.emit {
                                        self.write(&[match tok {
                                            Tok::BC => 0x0A,
                                            Tok::DE => 0x1A,
                                            Tok::HL => 0x7E,
                                            Tok::C => 0xF2,
                                            _ => unreachable!(),
                                        }]);
                                    }
                                    return self.add_pc(1);
                                }
                                _ => {
                                    let pos = self.tok().pos();
                                    let expr = self.expr()?;
                                    self.expect(Tok::RBRACKET)?;
                                    if self.emit {
                                        self.write(&[0xFA]);
                                        if let Ok(value) = self.const_expr(expr) {
                                            self.write(&self.range_u16(value)?.to_le_bytes());
                                        } else {
                                            self.write(&[0xFD, 0xFD]);
                                            self.reloc(1, 2, expr, pos, 0);
                                        }
                                    }
                                    return self.add_pc(3);
                                }
                            }
                        }
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x7F,
                                    Tok::B => 0x78,
                                    Tok::C => 0x79,
                                    Tok::D => 0x7A,
                                    Tok::E => 0x7B,
                                    Tok::H => 0x7C,
                                    Tok::L => 0x7D,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            let pos = self.tok().pos();
                            let expr = self.expr()?;
                            if self.emit {
                                self.write(&[0x3E]);
                                if let Ok(value) = self.const_expr(expr) {
                                    self.write(&self.range_u8(value)?.to_le_bytes());
                                } else {
                                    self.write(&[0xFD]);
                                    self.reloc(1, 1, expr, pos, 0);
                                }
                            }
                            return self.add_pc(2);
                        }
                    }
                }

                Tok::B => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x47,
                                    Tok::B => 0x40,
                                    Tok::C => 0x41,
                                    Tok::D => 0x42,
                                    Tok::E => 0x43,
                                    Tok::H => 0x44,
                                    Tok::L => 0x45,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            self.expect(Tok::LBRACKET)?;
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x46]);
                            }
                            return self.add_pc(1);
                        }
                    }
                }

                Tok::C => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x4F,
                                    Tok::B => 0x48,
                                    Tok::C => 0x49,
                                    Tok::D => 0x4A,
                                    Tok::E => 0x4B,
                                    Tok::H => 0x4C,
                                    Tok::L => 0x4D,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            self.expect(Tok::LBRACKET)?;
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x4E]);
                            }
                            return self.add_pc(1);
                        }
                    }
                }

                Tok::D => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x57,
                                    Tok::B => 0x50,
                                    Tok::C => 0x51,
                                    Tok::D => 0x52,
                                    Tok::E => 0x53,
                                    Tok::H => 0x54,
                                    Tok::L => 0x55,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            self.expect(Tok::LBRACKET)?;
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x56]);
                            }
                            return self.add_pc(1);
                        }
                    }
                }

                Tok::E => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x5F,
                                    Tok::B => 0x58,
                                    Tok::C => 0x59,
                                    Tok::D => 0x5A,
                                    Tok::E => 0x5B,
                                    Tok::H => 0x5C,
                                    Tok::L => 0x5D,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            self.expect(Tok::LBRACKET)?;
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x5E]);
                            }
                            return self.add_pc(1);
                        }
                    }
                }

                Tok::H => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x67,
                                    Tok::B => 0x60,
                                    Tok::C => 0x61,
                                    Tok::D => 0x62,
                                    Tok::E => 0x63,
                                    Tok::H => 0x64,
                                    Tok::L => 0x65,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            self.expect(Tok::LBRACKET)?;
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x66]);
                            }
                            return self.add_pc(1);
                        }
                    }
                }

                Tok::L => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x6F,
                                    Tok::B => 0x68,
                                    Tok::C => 0x69,
                                    Tok::D => 0x6A,
                                    Tok::E => 0x6B,
                                    Tok::H => 0x6C,
                                    Tok::L => 0x6D,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            self.expect(Tok::LBRACKET)?;
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x6E]);
                            }
                            return self.add_pc(1);
                        }
                    }
                }

                Tok::LBRACKET => {
                    self.eat();
                    match self.peek()? {
                        tok @ (Tok::BC | Tok::DE | Tok::C) => {
                            self.eat();
                            self.expect(Tok::RBRACKET)?;
                            self.expect(Tok::COMMA)?;
                            self.expect(Tok::A)?;
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::BC => 0x02,
                                    Tok::DE => 0x12,
                                    Tok::C => 0xE2,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        Tok::HL => {
                            self.eat();
                            self.expect(Tok::RBRACKET)?;
                            self.expect(Tok::COMMA)?;
                            match self.peek()? {
                                tok @ (Tok::A
                                | Tok::B
                                | Tok::C
                                | Tok::D
                                | Tok::E
                                | Tok::H
                                | Tok::L) => {
                                    self.eat();
                                    if self.emit {
                                        self.write(&[match tok {
                                            Tok::A => 0x77,
                                            Tok::B => 0x70,
                                            Tok::C => 0x71,
                                            Tok::D => 0x72,
                                            Tok::E => 0x73,
                                            Tok::H => 0x74,
                                            Tok::L => 0x75,
                                            _ => unreachable!(),
                                        }]);
                                    }
                                    return self.add_pc(1);
                                }
                                _ => {
                                    let pos = self.tok().pos();
                                    let expr = self.expr()?;
                                    if self.emit {
                                        self.write(&[0x36]);
                                        if let Ok(value) = self.const_expr(expr) {
                                            self.write(&self.range_u8(value)?.to_le_bytes());
                                        } else {
                                            self.write(&[0xFD]);
                                            self.reloc(1, 1, expr, pos, 0);
                                        }
                                    }
                                    return self.add_pc(2);
                                }
                            }
                        }
                        _ => {
                            let pos = self.tok().pos();
                            let expr = self.expr()?;
                            self.expect(Tok::RBRACKET)?;
                            self.expect(Tok::COMMA)?;
                            if self.peek()? == Tok::SP {
                                self.eat();
                                if self.emit {
                                    self.write(&[0x08]);
                                    if let Ok(value) = self.const_expr(expr) {
                                        self.write(&self.range_u16(value)?.to_le_bytes());
                                    } else {
                                        self.write(&[0xFD, 0xFD]);
                                        self.reloc(1, 2, expr, pos, 0);
                                    }
                                }
                                return self.add_pc(3);
                            }
                            self.expect(Tok::A)?;
                            if self.emit {
                                self.write(&[0xEA]);
                                if let Ok(value) = self.const_expr(expr) {
                                    self.write(&self.range_u16(value)?.to_le_bytes());
                                } else {
                                    self.write(&[0xFD, 0xFD]);
                                    self.reloc(1, 2, expr, pos, 0);
                                }
                            }
                            return self.add_pc(3);
                        }
                    }
                }

                tok @ (Tok::BC | Tok::DE | Tok::HL | Tok::SP) => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        self.write(&[match tok {
                            Tok::BC => 0x01,
                            Tok::DE => 0x11,
                            Tok::HL => 0x21,
                            Tok::SP => 0x31,
                            _ => unreachable!(),
                        }]);
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, 0);
                        }
                    }
                    return self.add_pc(3);
                }

                _ => return Err(self.err("unexpected garbage")),
            },

            Mne::LDD => match self.peek()? {
                Tok::A => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0x3A]);
                    }
                    return self.add_pc(1);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    self.expect(Tok::COMMA)?;
                    self.expect(Tok::A)?;
                    if self.emit {
                        self.write(&[0x32]);
                    }
                    return self.add_pc(1);
                }
            },

            Mne::LDI => match self.peek()? {
                Tok::A => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0x2A]);
                    }
                    return self.add_pc(1);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    self.expect(Tok::COMMA)?;
                    self.expect(Tok::A)?;
                    if self.emit {
                        self.write(&[0x22]);
                    }
                    return self.add_pc(1);
                }
            },

            Mne::LDH => match self.peek()? {
                Tok::A => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    self.expect(Tok::LBRACKET)?;
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xF0]);
                        if let Ok(value) = self.const_expr(expr) {
                            let value = self.range_u16(value)?;
                            if !(0xFF00..=0xFFFF).contains(&value) {
                                return Err(self.err("address not in hi memory"));
                            }
                            self.write(&(value as u8).to_le_bytes());
                        } else {
                            self.write(&[0xFD]);
                            self.reloc(1, 1, expr, pos, RelocFlags::HI);
                        }
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    self.expect(Tok::RBRACKET)?;
                    self.expect(Tok::COMMA)?;
                    self.expect(Tok::A)?;
                    if self.emit {
                        self.write(&[0xE0]);
                        if let Ok(value) = self.const_expr(expr) {
                            let value = self.range_u16(value)?;
                            if !(0xFF00..=0xFFFF).contains(&value) {
                                return Err(self.err("address not in hi memory"));
                            }
                            self.write(&(value as u8).to_le_bytes());
                        } else {
                            self.write(&[0xFD]);
                            self.reloc(1, 1, expr, pos, RelocFlags::HI);
                        }
                    }
                    return self.add_pc(2);
                }
            },

            Mne::PUSH => {
                match self.peek()? {
                    tok @ (Tok::BC | Tok::DE | Tok::HL | Tok::AF) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::BC => 0xC5,
                                Tok::DE => 0xD5,
                                Tok::HL => 0xE5,
                                Tok::AF => 0xF5,
                                _ => unreachable!(),
                            }]);
                        }
                    }
                    _ => return Err(self.err("invalid operand")),
                }
                return self.add_pc(1);
            }

            Mne::POP => {
                match self.peek()? {
                    tok @ (Tok::BC | Tok::DE | Tok::HL | Tok::AF) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::BC => 0xC1,
                                Tok::DE => 0xD1,
                                Tok::HL => 0xE1,
                                Tok::AF => 0xF1,
                                _ => unreachable!(),
                            }]);
                        }
                    }
                    _ => return Err(self.err("invalid operand")),
                }
                return self.add_pc(1);
            }

            Mne::ADD => match self.peek()? {
                Tok::HL => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::BC | Tok::DE | Tok::HL | Tok::SP) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::BC => 0x09,
                                    Tok::DE => 0x19,
                                    Tok::HL => 0x29,
                                    Tok::SP => 0x39,
                                    _ => unreachable!(),
                                }]);
                            }
                        }
                        _ => return Err(self.err("invalid operand")),
                    }
                    return self.add_pc(1);
                }
                Tok::SP => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        self.write(&[0xE8]);
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u8(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD]);
                            self.reloc(1, 1, expr, pos, 0);
                        }
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::A)?;
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {
                        tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                            self.eat();
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::A => 0x87,
                                    Tok::B => 0x80,
                                    Tok::C => 0x81,
                                    Tok::D => 0x82,
                                    Tok::E => 0x83,
                                    Tok::H => 0x84,
                                    Tok::L => 0x85,
                                    _ => unreachable!(),
                                }]);
                            }
                            return self.add_pc(1);
                        }
                        Tok::LBRACKET => {
                            self.eat();
                            self.expect(Tok::HL)?;
                            self.expect(Tok::RBRACKET)?;
                            if self.emit {
                                self.write(&[0x86]);
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            let pos = self.tok().pos();
                            let expr = self.expr()?;
                            if self.emit {
                                self.write(&[0xC6]);
                                if let Ok(value) = self.const_expr(expr) {
                                    self.write(&self.range_u8(value)?.to_le_bytes());
                                } else {
                                    self.write(&[0xFD]);
                                    self.reloc(1, 1, expr, pos, 0);
                                }
                            }
                            return self.add_pc(2);
                        }
                    }
                }
            },

            Mne::ADC => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0x8F,
                                Tok::B => 0x88,
                                Tok::C => 0x89,
                                Tok::D => 0x8A,
                                Tok::E => 0x8B,
                                Tok::H => 0x8C,
                                Tok::L => 0x8D,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0x8E]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xCE]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::SUB => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0x97,
                                Tok::B => 0x90,
                                Tok::C => 0x91,
                                Tok::D => 0x92,
                                Tok::E => 0x93,
                                Tok::H => 0x94,
                                Tok::L => 0x95,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0x96]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xD6]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::SBC => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0x9F,
                                Tok::B => 0x98,
                                Tok::C => 0x99,
                                Tok::D => 0x9A,
                                Tok::E => 0x9B,
                                Tok::H => 0x9C,
                                Tok::L => 0x9D,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0x9E]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xDE]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::AND => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0xA7,
                                Tok::B => 0xA0,
                                Tok::C => 0xA1,
                                Tok::D => 0xA2,
                                Tok::E => 0xA3,
                                Tok::H => 0xA4,
                                Tok::L => 0xA5,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xA6]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xE6]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::OR => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0xB7,
                                Tok::B => 0xB0,
                                Tok::C => 0xB1,
                                Tok::D => 0xB2,
                                Tok::E => 0xB3,
                                Tok::H => 0xB4,
                                Tok::L => 0xB5,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xB6]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xF6]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::XOR => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0xAF,
                                Tok::B => 0xA8,
                                Tok::C => 0xA9,
                                Tok::D => 0xAA,
                                Tok::E => 0xAB,
                                Tok::H => 0xAC,
                                Tok::L => 0xAD,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xAE]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xEE]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::CP => {
                self.expect(Tok::A)?;
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[match tok {
                                Tok::A => 0xBF,
                                Tok::B => 0xB8,
                                Tok::C => 0xB9,
                                Tok::D => 0xBA,
                                Tok::E => 0xBB,
                                Tok::H => 0xBC,
                                Tok::L => 0xBD,
                                _ => unreachable!(),
                            }]);
                        }
                        return self.add_pc(1);
                    }
                    Tok::LBRACKET => {
                        self.eat();
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xBE]);
                        }
                        return self.add_pc(1);
                    }
                    _ => {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            self.write(&[0xFE]);
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0xFD]);
                                self.reloc(1, 1, expr, pos, 0);
                            }
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::INC => match self.peek()? {
                tok @ (Tok::BC
                | Tok::DE
                | Tok::HL
                | Tok::SP
                | Tok::A
                | Tok::B
                | Tok::C
                | Tok::D
                | Tok::E
                | Tok::H
                | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[match tok {
                            Tok::BC => 0x03,
                            Tok::DE => 0x13,
                            Tok::HL => 0x23,
                            Tok::SP => 0x33,
                            Tok::A => 0x3C,
                            Tok::B => 0x04,
                            Tok::C => 0x0C,
                            Tok::D => 0x14,
                            Tok::E => 0x1C,
                            Tok::H => 0x24,
                            Tok::L => 0x2C,
                            _ => unreachable!(),
                        }]);
                    }
                    return self.add_pc(1);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0x34]);
                    }
                    return self.add_pc(1);
                }
            },

            Mne::DEC => match self.peek()? {
                tok @ (Tok::BC
                | Tok::DE
                | Tok::HL
                | Tok::SP
                | Tok::A
                | Tok::B
                | Tok::C
                | Tok::D
                | Tok::E
                | Tok::H
                | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[match tok {
                            Tok::BC => 0x0B,
                            Tok::DE => 0x1B,
                            Tok::HL => 0x2B,
                            Tok::SP => 0x3B,
                            Tok::A => 0x3D,
                            Tok::B => 0x05,
                            Tok::C => 0x0D,
                            Tok::D => 0x15,
                            Tok::E => 0x1D,
                            Tok::H => 0x25,
                            Tok::L => 0x2D,
                            _ => unreachable!(),
                        }]);
                    }
                    return self.add_pc(1);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0x35]);
                    }
                    return self.add_pc(1);
                }
            },

            Mne::SWAP => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x37,
                                Tok::B => 0x30,
                                Tok::C => 0x31,
                                Tok::D => 0x32,
                                Tok::E => 0x33,
                                Tok::H => 0x34,
                                Tok::L => 0x35,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x36]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::DAA => {
                if self.emit {
                    self.write(&[0x27]);
                }
                return self.add_pc(1);
            }

            Mne::CPL => {
                if self.emit {
                    self.write(&[0x2F]);
                }
                return self.add_pc(1);
            }

            Mne::CCF => {
                if self.emit {
                    self.write(&[0x3F]);
                }
                return self.add_pc(1);
            }

            Mne::SCF => {
                if self.emit {
                    self.write(&[0x37]);
                }
                return self.add_pc(1);
            }

            Mne::NOP => {
                if self.emit {
                    self.write(&[0x00]);
                }
                return self.add_pc(1);
            }

            Mne::HALT => {
                if self.emit {
                    self.write(&[0x76, 0x00]);
                }
                return self.add_pc(2);
            }

            Mne::STOP => {
                if self.emit {
                    self.write(&[0x10, 0x00]);
                }
                return self.add_pc(2);
            }

            Mne::DI => {
                if self.emit {
                    self.write(&[0xF3]);
                }
                return self.add_pc(1);
            }

            Mne::EI => {
                if self.emit {
                    self.write(&[0xFB]);
                }
                return self.add_pc(1);
            }

            Mne::RLCA => {
                if self.emit {
                    self.write(&[0x07]);
                }
                return self.add_pc(1);
            }

            Mne::RLA => {
                if self.emit {
                    self.write(&[0x17]);
                }
                return self.add_pc(1);
            }

            Mne::RRCA => {
                if self.emit {
                    self.write(&[0x0F]);
                }
                return self.add_pc(1);
            }

            Mne::RRA => {
                if self.emit {
                    self.write(&[0x1F]);
                }
                return self.add_pc(1);
            }

            Mne::RLC => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x07,
                                Tok::B => 0x00,
                                Tok::C => 0x01,
                                Tok::D => 0x02,
                                Tok::E => 0x03,
                                Tok::H => 0x04,
                                Tok::L => 0x05,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x06]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::RL => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x17,
                                Tok::B => 0x10,
                                Tok::C => 0x11,
                                Tok::D => 0x12,
                                Tok::E => 0x13,
                                Tok::H => 0x14,
                                Tok::L => 0x15,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x16]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::RRC => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x0F,
                                Tok::B => 0x08,
                                Tok::C => 0x09,
                                Tok::D => 0x0A,
                                Tok::E => 0x0B,
                                Tok::H => 0x0C,
                                Tok::L => 0x0D,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x0E]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::RR => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x1F,
                                Tok::B => 0x18,
                                Tok::C => 0x19,
                                Tok::D => 0x1A,
                                Tok::E => 0x1B,
                                Tok::H => 0x1C,
                                Tok::L => 0x1D,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x1E]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::SLA => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x27,
                                Tok::B => 0x20,
                                Tok::C => 0x21,
                                Tok::D => 0x22,
                                Tok::E => 0x23,
                                Tok::H => 0x24,
                                Tok::L => 0x25,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x26]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::SRA => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x2F,
                                Tok::B => 0x28,
                                Tok::C => 0x29,
                                Tok::D => 0x2A,
                                Tok::E => 0x2B,
                                Tok::H => 0x2C,
                                Tok::L => 0x2D,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x2E]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::SRL => match self.peek()? {
                tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                    self.eat();
                    if self.emit {
                        self.write(&[
                            0xCB,
                            match tok {
                                Tok::A => 0x3F,
                                Tok::B => 0x38,
                                Tok::C => 0x39,
                                Tok::D => 0x3A,
                                Tok::E => 0x3B,
                                Tok::H => 0x3C,
                                Tok::L => 0x3D,
                                _ => unreachable!(),
                            },
                        ]);
                    }
                    return self.add_pc(2);
                }
                _ => {
                    self.expect(Tok::LBRACKET)?;
                    self.expect(Tok::HL)?;
                    self.expect(Tok::RBRACKET)?;
                    if self.emit {
                        self.write(&[0xCB, 0x3E]);
                    }
                    return self.add_pc(2);
                }
            },

            Mne::BIT => {
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                if expr > 7 {
                    return Err(self.err("invalid bit index"));
                }
                let base = match expr {
                    0 => 0x40,
                    1 => 0x48,
                    2 => 0x50,
                    3 => 0x58,
                    4 => 0x60,
                    5 => 0x68,
                    6 => 0x70,
                    7 => 0x78,
                    _ => unreachable!(),
                };
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[
                                0xCB,
                                base + match tok {
                                    Tok::A => 0x07,
                                    Tok::B => 0x00,
                                    Tok::C => 0x01,
                                    Tok::D => 0x02,
                                    Tok::E => 0x03,
                                    Tok::H => 0x04,
                                    Tok::L => 0x05,
                                    _ => unreachable!(),
                                },
                            ]);
                        }
                        return self.add_pc(2);
                    }
                    _ => {
                        self.expect(Tok::LBRACKET)?;
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xCB, base + 0x06]);
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::SET => {
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                if expr > 7 {
                    return Err(self.err("invalid bit index"));
                }
                let base = match expr {
                    0 => 0xC0,
                    1 => 0xC8,
                    2 => 0xD0,
                    3 => 0xD8,
                    4 => 0xE0,
                    5 => 0xE8,
                    6 => 0xF0,
                    7 => 0xF8,
                    _ => unreachable!(),
                };
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[
                                0xCB,
                                base + match tok {
                                    Tok::A => 0x07,
                                    Tok::B => 0x00,
                                    Tok::C => 0x01,
                                    Tok::D => 0x02,
                                    Tok::E => 0x03,
                                    Tok::H => 0x04,
                                    Tok::L => 0x05,
                                    _ => unreachable!(),
                                },
                            ]);
                        }
                        return self.add_pc(2);
                    }
                    _ => {
                        self.expect(Tok::LBRACKET)?;
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xCB, base + 0x06]);
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::RES => {
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                if expr > 7 {
                    return Err(self.err("invalid bit index"));
                }
                let base = match expr {
                    0 => 0x80,
                    1 => 0x88,
                    2 => 0x90,
                    3 => 0x98,
                    4 => 0xA0,
                    5 => 0xA8,
                    6 => 0xB0,
                    7 => 0xB8,
                    _ => unreachable!(),
                };
                self.expect(Tok::COMMA)?;
                match self.peek()? {
                    tok @ (Tok::A | Tok::B | Tok::C | Tok::D | Tok::E | Tok::H | Tok::L) => {
                        self.eat();
                        if self.emit {
                            self.write(&[
                                0xCB,
                                base + match tok {
                                    Tok::A => 0x07,
                                    Tok::B => 0x00,
                                    Tok::C => 0x01,
                                    Tok::D => 0x02,
                                    Tok::E => 0x03,
                                    Tok::H => 0x04,
                                    Tok::L => 0x05,
                                    _ => unreachable!(),
                                },
                            ]);
                        }
                        return self.add_pc(2);
                    }
                    _ => {
                        self.expect(Tok::LBRACKET)?;
                        self.expect(Tok::HL)?;
                        self.expect(Tok::RBRACKET)?;
                        if self.emit {
                            self.write(&[0xCB, base + 0x06]);
                        }
                        return self.add_pc(2);
                    }
                }
            }

            Mne::JP => match self.peek()? {
                tok @ (Tok::C | Tok::Z | Tok::NC | Tok::NZ) => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        self.write(&[match tok {
                            Tok::C => 0xDA,
                            Tok::Z => 0xCA,
                            Tok::NC => 0xD2,
                            Tok::NZ => 0xC2,
                            _ => unreachable!(),
                        }]);
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, RelocFlags::JP);
                        }
                    }
                    return self.add_pc(3);
                }
                Tok::HL => {
                    self.eat();
                    if self.emit {
                        self.write(&[0xE9]);
                    }
                    return self.add_pc(1);
                }
                _ => {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        self.write(&[0xC3]);
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, RelocFlags::JP);
                        }
                    }
                    return self.add_pc(3);
                }
            },

            Mne::JR => match self.peek()? {
                tok @ (Tok::C | Tok::Z | Tok::NC | Tok::NZ) => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    let expr = self.expr()?;
                    if self.emit {
                        let branch = self
                            .const_branch_expr(expr)?
                            .wrapping_sub((self.pc() as i32).wrapping_add(2));
                        if (branch < (i8::MIN as i32)) || (branch > (i8::MAX as i32)) {
                            return Err(self.err("branch distance too far"));
                        }
                        self.write(&[match tok {
                            Tok::C => 0x38,
                            Tok::Z => 0x28,
                            Tok::NC => 0x30,
                            Tok::NZ => 0x20,
                            _ => unreachable!(),
                        }]);
                        self.write(&(branch as i8).to_le_bytes());
                    }
                    return self.add_pc(2);
                }
                _ => {
                    let expr = self.expr()?;
                    if self.emit {
                        let branch = self
                            .const_branch_expr(expr)?
                            .wrapping_sub((self.pc() as i32).wrapping_add(2));
                        if (branch < (i8::MIN as i32)) || (branch > (i8::MAX as i32)) {
                            return Err(self.err("branch distance too far"));
                        }
                        self.write(&[0x18]);
                        self.write(&(branch as i8).to_le_bytes());
                    }
                    return self.add_pc(2);
                }
            },

            Mne::CALL => match self.peek()? {
                tok @ (Tok::C | Tok::Z | Tok::NC | Tok::NZ) => {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        self.write(&[match tok {
                            Tok::C => 0xDC,
                            Tok::Z => 0xCC,
                            Tok::NC => 0xD4,
                            Tok::NZ => 0xC4,
                            _ => unreachable!(),
                        }]);
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, RelocFlags::JP);
                        }
                    }
                    return self.add_pc(3);
                }
                _ => {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        self.write(&[0xCD]);
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(1, 2, expr, pos, RelocFlags::JP);
                        }
                    }
                    return self.add_pc(3);
                }
            },

            Mne::RST => {
                let expr = self.expr()?;
                if self.emit {
                    let expr = self.const_expr(expr)?;
                    self.write(&[match expr {
                        0x00 => 0xC7,
                        0x08 => 0xCF,
                        0x10 => 0xD7,
                        0x18 => 0xDF,
                        0x20 => 0xE7,
                        0x28 => 0xEF,
                        0x30 => 0xF7,
                        0x38 => 0xFF,
                        _ => return Err(self.err("invalid rst address")),
                    }]);
                }
                return self.add_pc(1);
            }

            Mne::RET => match self.peek()? {
                tok @ (Tok::C | Tok::Z | Tok::NC | Tok::NZ) => {
                    self.eat();
                    if self.emit {
                        self.write(&[match tok {
                            Tok::C => 0xD8,
                            Tok::Z => 0xC8,
                            Tok::NC => 0xD0,
                            Tok::NZ => 0xC0,
                            _ => unreachable!(),
                        }]);
                    }
                    return self.add_pc(1);
                }
                _ => {
                    if self.emit {
                        self.write(&[0xC9]);
                    }
                    return self.add_pc(1);
                }
            },

            Mne::RETI => {
                if self.emit {
                    self.write(&[0xD9]);
                }
                return self.add_pc(1);
            }

            _ => unreachable!(),
        }
    }

    fn directive(&mut self) -> io::Result<()> {
        match self.peek()? {
            Tok::BYTE => {
                self.eat();
                loop {
                    if self.peek()? == Tok::STR {
                        if self.emit {
                            self.write_str();
                        }
                        self.add_pc(self.str().len() as u32)?;
                        self.eat();
                    } else {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_u8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0x0FD]);
                                self.reloc(0, 1, expr, pos, 0);
                            }
                        }
                        self.add_pc(1)?;
                    }
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
                self.eol()?;
            }
            Tok::WORD => {
                self.eat();
                loop {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_u16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0xFD, 0xFD]);
                            self.reloc(0, 2, expr, pos, 0);
                        }
                    }
                    self.add_pc(2)?;
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
                self.eol()?;
            }
            Tok::SECTION => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected section name"));
                }
                let name = self.str_intern();
                self.eat();
                let index = if let Some(sec) = self
                    .sections
                    .iter()
                    .enumerate()
                    .find(|sec| &sec.1.name == &name)
                {
                    sec.0
                } else {
                    // save in the section table with default values
                    let index = self.sections.len();
                    self.sections.push(Section::new(name));
                    index
                };
                self.section = index;
                self.eol()?;
            }
            Tok::INCLUDE => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected file name"));
                }
                let name = self.str();
                let name = if let Ok(name) = fs::canonicalize(name) {
                    name
                } else {
                    let mut base = self.tok().pos().file.to_path_buf();
                    base.pop();
                    base.push(name);
                    // try current directory
                    if let Ok(name) = fs::canonicalize(name) {
                        name
                    } else if let Some(name) = self.includes.iter().find_map(|path| {
                        // try every include directory
                        let mut base = path.to_path_buf();
                        base.push(name);
                        fs::canonicalize(base).ok()
                    }) {
                        name
                    } else {
                        return Err(self.err("file not found"));
                    }
                };
                let name = self.path_int.intern(&name);
                self.included.insert(name);
                let file = File::open(name)?;
                self.eat();
                let lexer = Lexer::new(file, name);
                self.toks.push(Box::new(lexer));
            }
            tok @ (Tok::IF | Tok::IFDEF | Tok::IFNDEF) => {
                self.ifdirective(tok)?;
            }
            Tok::END => {
                if self.if_level == 0 {
                    return Err(self.err("unexpected end"));
                }
                self.eat();
                self.if_level -= 1;
            }
            Tok::SPACE => {
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                let n = self.range_u16(expr)?;
                // reserve space by allocating literal bytes
                // if we dont do this, our current linker will
                // overlap the same section across objects as pc
                // does not allocate space on its own
                if self.emit {
                    for _ in 0..n {
                        self.write(&[0x00]);
                    }
                }
                self.add_pc(n as u32)?;
                self.eol()?;
            }
            Tok::MACRO => {
                self.macrodef()?;
            }
            Tok::LOOP => {
                self.forloop()?;
            }
            Tok::STRUCT => {
                self.structdef()?;
            }
            Tok::FAIL => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected message"));
                }
                return Err(self.err(self.str()));
            }
            _ => return Err(self.err("expected directive")),
        }
        Ok(())
    }

    fn macroinvoke(&mut self, name: &'a str, mac: Macro<'a>) -> io::Result<()> {
        let pos = self.tok().pos();
        self.eat();
        let mut args = VecDeque::new();
        let mut arg = Vec::new();
        loop {
            // TODO: pos per tok
            let pos = self.tok().pos();
            match self.peek()? {
                tok @ (Tok::BREAK | Tok::NEWLINE | Tok::EOF) => {
                    if !arg.is_empty() {
                        let arg = self.tok_int.intern(&arg);
                        args.push_back(arg);
                    }
                    if tok == Tok::BREAK {
                        self.eat();
                    }
                    break;
                }
                Tok::ID => arg.push((pos, MacroTok::Id(self.str_intern()))),
                Tok::STR => arg.push((pos, MacroTok::Str(self.str_intern()))),
                Tok::NUM => arg.push((pos, MacroTok::Num(self.tok().num()))),
                tok => arg.push((pos, MacroTok::Tok(tok))),
            }
            self.eat();
            if self.peek()? == Tok::COMMA {
                self.eat();
                let iarg = self.tok_int.intern(&arg);
                args.push_back(iarg);
                arg.clear();
            }
        }
        self.unique += 1;
        self.toks.push(Box::new(MacroInvocation {
            inner: mac,
            name,
            unique: self.unique,
            index: 0,
            fmt_buf: String::new(),
            args,
            arg_index: 0,
            pos,
        }));
        Ok(())
    }

    fn ifdirective(&mut self, tok: Tok) -> io::Result<()> {
        self.eat();
        let expr = self.expr()?;
        let skip = match tok {
            Tok::IF => self.const_expr(expr)? == 0,
            Tok::IFDEF => !matches!(expr, Expr::Const(_)),
            Tok::IFNDEF => matches!(expr, Expr::Const(_)),
            _ => unreachable!(),
        };
        if skip {
            let mut if_level = 0;
            loop {
                match self.peek()? {
                    Tok::IF | Tok::IFDEF | Tok::STRUCT | Tok::MACRO | Tok::LOOP => if_level += 1,
                    Tok::END => {
                        if if_level == 0 {
                            self.eat();
                            break;
                        }
                        if_level -= 1;
                    }
                    _ => {}
                }
                self.eat();
            }
        }
        self.if_level += 1;
        Ok(())
    }

    fn structdef(&mut self) -> io::Result<()> {
        self.eat();
        if self.peek()? != Tok::ID {
            return Err(self.err("expected struct name"));
        }
        let string = self.str_intern();
        if string.starts_with(".") {
            return Err(self.err("struct must be global"));
        }
        let pos = self.tok().pos();
        self.eat();
        let label = Label::new(Some(string), self.str_int.intern(".SIZE"));
        // TODO: check if struct already defined (similar to macro)
        let mut size = 0;
        let unit = self.str_int.intern("__STATIC__");
        let section = self.sections[self.section].name;
        loop {
            match self.peek()? {
                Tok::NEWLINE => {
                    self.eat();
                    continue;
                }
                Tok::END => {
                    self.eat();
                    break;
                }
                tok @ (Tok::IF | Tok::IFDEF | Tok::IFNDEF) => {
                    self.ifdirective(tok)?;
                    continue;
                }
                _ => {}
            }
            if self.peek()? != Tok::ID {
                return Err(self.err("expected field name"));
            }
            {
                if self.str_like(".SIZE") {
                    return Err(self.err(".SIZE is a reserved field name"));
                }
                // is this a macro?
                if let Some((name, mac)) = self.macros.get_key_value(self.str()) {
                    self.macroinvoke(name, *mac)?;
                    continue;
                }
                let field = self.str_intern();
                if !field.starts_with(".") {
                    return Err(self.err("field must have a local label"));
                }
                let pos = self.tok().pos();
                self.eat();
                let label = Label::new(Some(string), field);
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                if !self.emit {
                    self.syms.insert(
                        label,
                        Sym {
                            value: Expr::Const(size),
                            unit,
                            section,
                            pos,
                            flags: SymFlags::EQU,
                        },
                    );
                }
                size += expr;
            }
            self.eol()?;
        }
        if !self.emit {
            self.syms.insert(
                label,
                Sym {
                    value: Expr::Const(size),
                    unit,
                    section,
                    pos,
                    flags: SymFlags::EQU,
                },
            );
        }
        Ok(())
    }

    fn macrodef(&mut self) -> io::Result<()> {
        self.eat();
        if self.peek()? != Tok::ID {
            return Err(self.err("expected macro name"));
        }
        let string = self.str_intern();
        if string.starts_with(".") {
            return Err(self.err("macro must be global"));
        }
        self.eat();
        let label = Label::new(None, string);
        // TODO: check if macro is already defined
        // if we are in the emit pass then its safe to skip
        self.eol()?;
        let mut toks = Vec::new();
        let mut if_level = 0;
        loop {
            match self.peek()? {
                Tok::IF | Tok::IFDEF | Tok::STRUCT | Tok::MACRO | Tok::LOOP => if_level += 1,
                Tok::END => {
                    if if_level == 0 {
                        self.eat();
                        break;
                    }
                    if_level -= 1;
                }
                _ => {}
            }
            // TODO: pos per tok
            let pos = self.tok().pos();
            match self.peek()? {
                Tok::EOF => return Err(self.err("unexpected end of file")),
                Tok::ID => toks.push((pos, MacroTok::Id(self.str_intern()))),
                Tok::STR => toks.push((pos, MacroTok::Str(self.str_intern()))),
                Tok::NUM => toks.push((pos, MacroTok::Num(self.tok().num()))),
                Tok::ARG => toks.push((pos, MacroTok::Arg((self.tok().num() as usize) - 1))),
                Tok::NARG => toks.push((pos, MacroTok::Narg)),
                Tok::UNIQ => toks.push((pos, MacroTok::Uniq)),
                Tok::SHIFT => toks.push((pos, MacroTok::Shift)),
                Tok::JOIN => {
                    self.eat();
                    let mut jtoks = Vec::new();
                    loop {
                        let pos = self.tok().pos();
                        match self.peek()? {
                            Tok::ID => jtoks.push((pos, MacroTok::Id(self.str_intern()))),
                            Tok::STR => jtoks.push((pos, MacroTok::Str(self.str_intern()))),
                            Tok::NUM => jtoks.push((pos, MacroTok::Num(self.tok().num()))),
                            Tok::ARG => {
                                jtoks.push((pos, MacroTok::Arg((self.tok().num() as usize) - 1)))
                            }
                            Tok::UNIQ => jtoks.push((pos, MacroTok::Uniq)),
                            _ => return Err(self.err("invalid \\JOIN input")),
                        }
                        self.eat();
                        if self.peek()? == Tok::BREAK {
                            self.eat();
                            break;
                        }
                        if self.peek()? != Tok::COMMA {
                            break;
                        }
                        self.eat();
                    }
                    let jtoks = self.tok_int.intern(&jtoks);
                    if jtoks.is_empty() {
                        return Err(self.err("\\JOIN needs inputs"));
                    }
                    toks.push((pos, MacroTok::Join(jtoks)));
                    continue;
                }
                tok => toks.push((pos, MacroTok::Tok(tok))),
            }
            self.eat();
        }
        let toks = self.tok_int.intern(&toks);
        self.macros.insert(&label.string, Macro { toks });
        Ok(())
    }

    fn forloop(&mut self) -> io::Result<()> {
        self.eat();
        if self.peek()? != Tok::ID {
            return Err(self.err("expected variable name"));
        }
        let string = self.str_intern();
        if string.starts_with(".") {
            return Err(self.err("variable must be global"));
        }
        self.eat();
        self.expect(Tok::COMMA)?;
        let end = self.expr()?;
        let end = self.const_expr(end)?;
        let mut toks = Vec::new();
        let mut if_level = 0;
        loop {
            match self.peek()? {
                Tok::IF | Tok::IFDEF | Tok::STRUCT | Tok::MACRO | Tok::LOOP => if_level += 1,
                Tok::END => {
                    if if_level == 0 {
                        self.eat();
                        break;
                    }
                    if_level -= 1;
                }
                _ => {}
            }
            // TODO: a lot of these position calcs are probs wrong because we need to check pos
            //   *after* skipping whitespace, meaning we need to peek first.
            let pos = self.tok().pos();
            match self.peek()? {
                Tok::EOF => return Err(self.err("unexpected end of file")),
                Tok::ID if self.str_like(&string) => toks.push((pos, LoopTok::Iter)),
                Tok::ID => toks.push((pos, LoopTok::Id(self.str_intern()))),
                Tok::STR => toks.push((pos, LoopTok::Str(self.str_intern()))),
                Tok::NUM => toks.push((pos, LoopTok::Num(self.tok().num()))),
                Tok::JOIN => {
                    self.eat();
                    let mut jtoks = Vec::new();
                    loop {
                        let pos = self.tok().pos();
                        match self.peek()? {
                            Tok::ID if self.str_like(&string) => jtoks.push((pos, LoopTok::Iter)),
                            Tok::ID => jtoks.push((pos, LoopTok::Id(self.str_intern()))),
                            Tok::STR => jtoks.push((pos, LoopTok::Str(self.str_intern()))),
                            Tok::NUM => jtoks.push((pos, LoopTok::Num(self.tok().num()))),
                            _ => return Err(self.err("invalid \\JOIN input")),
                        }
                        self.eat();
                        if self.peek()? == Tok::BREAK {
                            self.eat();
                            break;
                        }
                        if self.peek()? != Tok::COMMA {
                            break;
                        }
                        self.eat();
                    }
                    let jtoks = self.loop_int.intern(&jtoks);
                    if jtoks.is_empty() {
                        return Err(self.err("\\JOIN needs inputs"));
                    }
                    toks.push((pos, LoopTok::Join(jtoks)));
                    continue;
                }
                tok => toks.push((pos, LoopTok::Tok(tok))),
            }
            self.eat();
        }
        let toks = self.loop_int.intern(&toks);
        self.toks.push(Box::new(Loop {
            toks,
            index: 0,
            join_buf: String::new(),
            iter: 0,
            end: end as usize,
        }));
        Ok(())
    }

    fn write(&mut self, buf: &[u8]) {
        self.sections[self.section].data.extend_from_slice(buf);
    }

    fn write_str(&mut self) {
        let Self {
            ref mut sections,
            toks,
            ..
        } = self;
        sections[self.section]
            .data
            .extend_from_slice(toks.last().unwrap().str().as_bytes());
    }

    fn tok(&self) -> &dyn TokStream<'a> {
        self.toks.last().unwrap().as_ref()
    }

    fn tok_mut(&mut self) -> &mut dyn TokStream<'a> {
        self.toks.last_mut().unwrap().as_mut()
    }

    fn str(&self) -> &str {
        self.tok().str()
    }

    fn str_like(&self, string: &str) -> bool {
        self.tok().str().eq_ignore_ascii_case(string)
    }

    fn str_intern(&mut self) -> &'a str {
        let Self {
            ref mut str_int,
            toks,
            ..
        } = self;
        let string = toks.last().unwrap().str();
        str_int.intern(string)
    }

    fn pc(&self) -> u32 {
        self.sections[self.section].pc
    }

    fn set_pc(&mut self, val: u32) {
        self.sections[self.section].pc = val;
    }

    fn add_pc(&mut self, amt: u32) -> io::Result<()> {
        let val = self.pc().wrapping_add(amt);
        if (val < self.pc()) || (val > 0x010000) {
            return Err(self.err("section overflow"));
        }
        self.set_pc(val);
        Ok(())
    }

    fn err(&self, msg: &str) -> io::Error {
        self.tok().err(msg)
    }

    fn peek(&mut self) -> io::Result<Tok> {
        match self.tok_mut().peek() {
            Ok(Tok::EOF) if self.toks.len() > 1 => {
                // TODO: yuck recursion
                self.toks.pop();
                self.eat();
                self.peek()
            }
            tok => tok,
        }
    }

    fn eat(&mut self) {
        self.tok_mut().eat();
    }

    fn expect(&mut self, tok: Tok) -> io::Result<()> {
        let actual = self.peek()?;
        if actual != tok {
            return Err(self.err(&format!("unexpected {actual}, expected {tok}")));
        }
        self.eat();
        Ok(())
    }

    fn eol(&mut self) -> io::Result<()> {
        match self.peek()? {
            Tok::NEWLINE | Tok::EOF => {
                self.eat();
                Ok(())
            }
            actual => Err(self.err(&format!("unexpected {actual}, expected end of line"))),
        }
    }

    fn reloc(&mut self, offset: usize, width: u8, expr: Expr<'a>, pos: Pos<'a>, flags: u8) {
        let pc = self.pc() as usize;
        let offset = pc + offset;
        let unit = self.str_int.intern("__STATIC__");
        let value = match expr {
            Expr::Const(_) => unreachable!(),
            Expr::List(expr) => expr,
        };
        self.sections[self.section].relocs.push(Reloc {
            offset,
            width,
            value,
            unit,
            pos,
            flags,
        });
    }

    fn range_u16(&self, value: i32) -> io::Result<u16> {
        if (value as u32) > (u16::MAX as u32) {
            return Err(self.err("expression >2 bytes"));
        }
        Ok(value as u16)
    }

    fn range_u8(&self, value: i32) -> io::Result<u8> {
        if (value as u32) > (u8::MAX as u32) {
            return Err(self.err("expression >1 byte"));
        }
        Ok(value as u8)
    }

    fn range_i16(&self, value: i32) -> io::Result<u16> {
        if value > (i16::MAX as i32) {
            return Err(self.err("expression >2 bytes"));
        }
        Ok(value as u16)
    }

    fn range_i8(&self, value: i32) -> io::Result<u8> {
        if value > (i8::MAX as i32) {
            return Err(self.err("expression >1 byte"));
        }
        Ok(value as u8)
    }

    fn const_expr(&self, expr: Expr<'_>) -> io::Result<i32> {
        match expr {
            Expr::Const(value) => Ok(value),
            Expr::List(expr) => {
                if let Some(value) = self.expr_eval(expr) {
                    Ok(value)
                } else {
                    Err(self.err("expression must be constant"))
                }
            }
        }
    }

    fn const_branch_expr(&self, expr: Expr<'_>) -> io::Result<i32> {
        match expr {
            Expr::Const(value) => Ok(value),
            Expr::List(expr) => {
                if let Some(value) = self.expr_branch_eval(expr, true) {
                    Ok(value)
                } else {
                    Err(self.err("branch expression must be constant"))
                }
            }
        }
    }

    fn expr_eval(&self, expr: &[ExprNode<'_>]) -> Option<i32> {
        self.expr_branch_eval(expr, false)
    }

    fn expr_branch_eval(&self, expr: &[ExprNode<'_>], branch: bool) -> Option<i32> {
        let mut scratch = Vec::new();
        for node in expr.iter() {
            match *node {
                ExprNode::Const(value) => scratch.push(value),
                ExprNode::Label(label) => {
                    if let Some(sym) = self.syms.get(&label) {
                        match sym.value {
                            Expr::Const(value) => scratch.push(value),
                            Expr::List(expr) => {
                                // expand the sub-expression recursively
                                scratch.push(self.expr_branch_eval(expr, branch)?);
                            }
                        }
                    } else {
                        return None; // needs to be solved later
                    }
                }
                ExprNode::Tag(_, _) => {
                    return None; // tags can only be solved at link-time
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
                    // branches within the section can be resolved constantly
                    if branch && (self.sections[self.section].name == section) {
                        scratch.push(pc as i32)
                    } else {
                        // the linker has to handle this
                        return None;
                    }
                }
            }
        }
        scratch.last().copied()
    }

    fn expr_precedence(&self, op: Op) -> u8 {
        match op {
            Op::Unary(Tok::LPAREN) => 0xFF, // lparen is lowest precedence
            Op::Unary(_) => 0,              // other unary is highest precedence
            Op::Binary(Tok::SOLIDUS | Tok::MODULUS | Tok::STAR) => 1,
            Op::Binary(Tok::PLUS | Tok::MINUS) => 2,
            Op::Binary(Tok::ASL | Tok::ASR | Tok::LSR) => 3,
            Op::Binary(Tok::LT | Tok::LTE | Tok::GT | Tok::GTE) => 4,
            Op::Binary(Tok::DEQU | Tok::NEQU) => 5,
            Op::Binary(Tok::AMP) => 6,
            Op::Binary(Tok::CARET) => 7,
            Op::Binary(Tok::PIPE) => 8,
            Op::Binary(Tok::AND) => 9,
            Op::Binary(Tok::OR) => 10,
            _ => unreachable!(),
        }
    }

    fn expr_push_apply(&mut self, op: Op) {
        while let Some(top) = self.operator_buffer.last().copied() {
            if self.expr_precedence(top) >= self.expr_precedence(op) {
                break;
            }
            self.operator_buffer.pop();
            self.expr_buffer.push(ExprNode::Op(top));
        }
        self.operator_buffer.push(op);
    }

    fn expr(&mut self) -> io::Result<Expr<'a>> {
        self.expr_buffer.clear();
        self.operator_buffer.clear();
        let mut seen_val = false;
        let mut paren_depth = 0;
        // sort of a pratt/shunting-yard algorithm combo
        loop {
            match self.peek()? {
                // * is multiply or the relative PC
                Tok::STAR => {
                    if !seen_val {
                        self.expr_buffer.push(ExprNode::Const(self.pc() as i32));
                        seen_val = true;
                        self.eat();
                        continue;
                    }
                    self.expr_push_apply(Op::Binary(Tok::STAR));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                // ** is the absolute PC (as a section-relative address)
                Tok::DSTAR => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    let section = self.sections[self.section].name;
                    self.expr_buffer.push(ExprNode::Addr(section, self.pc()));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                // these are optionally unary
                tok @ (Tok::PLUS | Tok::MINUS | Tok::CARET | Tok::LT | Tok::GT) => {
                    if seen_val {
                        self.expr_push_apply(Op::Binary(tok));
                    } else {
                        self.expr_push_apply(Op::Unary(tok));
                    }
                    seen_val = false;
                    self.eat();
                    continue;
                }
                // always unary
                tok @ (Tok::BANG | Tok::TILDE) => {
                    self.expr_push_apply(Op::Unary(tok));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                tok @ (Tok::AMP
                | Tok::AND
                | Tok::OR
                | Tok::SOLIDUS
                | Tok::MODULUS
                | Tok::PIPE
                | Tok::ASL
                | Tok::ASR
                | Tok::LSR
                | Tok::LTE
                | Tok::GTE
                | Tok::DEQU
                | Tok::NEQU) => {
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    self.expr_push_apply(Op::Binary(tok));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                Tok::NUM => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    self.expr_buffer.push(ExprNode::Const(self.tok().num()));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::LPAREN => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    paren_depth += 1;
                    self.operator_buffer.push(Op::Unary(Tok::LPAREN));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                Tok::RPAREN => {
                    paren_depth -= 1;
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    loop {
                        if let Some(op) = self.operator_buffer.pop() {
                            // we apply ops until we see the start of this grouping
                            match op {
                                Op::Binary(tok) | Op::Unary(tok) if tok == Tok::LPAREN => {
                                    break;
                                }
                                _ => {}
                            }
                            self.expr_buffer.push(ExprNode::Op(op));
                        } else {
                            return Err(self.err("unbalanced parens"));
                        }
                    }
                    self.eat();
                    continue;
                }
                Tok::ID => {
                    // is this a macro?
                    if let Some((name, mac)) = self.macros.get_key_value(self.str()) {
                        self.macroinvoke(name, *mac)?;
                        continue;
                    }
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    let string = self.str_intern();
                    let label = if let Some(index) = string.find('.') {
                        let (scope, string) = string.split_at(index);
                        if scope.is_empty() {
                            Label::new(self.scope, string)
                        } else {
                            Label::new(Some(scope), string)
                        }
                    } else {
                        Label::new(None, string)
                    };
                    self.expr_buffer.push(ExprNode::Label(label));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::TAG => {
                    self.eat();
                    if self.peek()? != Tok::ID {
                        return Err(self.err("expected label"));
                    }
                    let string = self.str_intern();
                    let label = if let Some(index) = string.find('.') {
                        let (scope, string) = string.split_at(index);
                        if scope.is_empty() {
                            Label::new(self.scope, string)
                        } else {
                            Label::new(Some(scope), string)
                        }
                    } else {
                        Label::new(None, string)
                    };
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    if self.peek()? != Tok::STR {
                        return Err(self.err("expected tag name"));
                    }
                    let tag = self.str_intern();
                    self.expr_buffer.push(ExprNode::Tag(label, tag));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::LEN => {
                    self.eat();
                    if self.peek()? != Tok::STR {
                        return Err(self.err("expected string"));
                    }
                    self.expr_buffer
                        .push(ExprNode::Const(self.str().len() as i32));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                _ => {
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    if paren_depth != 0 {
                        return Err(self.err("unbalanced parens"));
                    }
                    break;
                }
            }
        }
        while let Some(top) = self.operator_buffer.pop() {
            self.expr_buffer.push(ExprNode::Op(top));
        }
        if let Some(value) = self.expr_eval(&self.expr_buffer) {
            Ok(Expr::Const(value))
        } else {
            Ok(Expr::List(self.expr_int.intern(&self.expr_buffer)))
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Mne(&'static str);

impl Mne {
    const LD: Self = Self("LD");
    const LDD: Self = Self("LDD");
    const LDI: Self = Self("LDI");
    const LDH: Self = Self("LDH");
    const PUSH: Self = Self("PUSH");
    const POP: Self = Self("POP");
    const ADD: Self = Self("ADD");
    const ADC: Self = Self("ADC");
    const SUB: Self = Self("SUB");
    const SBC: Self = Self("SBC");
    const AND: Self = Self("AND");
    const OR: Self = Self("OR");
    const XOR: Self = Self("XOR");
    const CP: Self = Self("CP");
    const INC: Self = Self("INC");
    const DEC: Self = Self("DEC");
    const SWAP: Self = Self("SWAP");
    const DAA: Self = Self("DAA");
    const CPL: Self = Self("CPL");
    const CCF: Self = Self("CCF");
    const SCF: Self = Self("SCF");
    const NOP: Self = Self("NOP");
    const HALT: Self = Self("HALT");
    const STOP: Self = Self("STOP");
    const DI: Self = Self("DI");
    const EI: Self = Self("EI");
    const RLCA: Self = Self("RLCA");
    const RLA: Self = Self("RLA");
    const RRCA: Self = Self("RRCA");
    const RRA: Self = Self("RRA");
    const RLC: Self = Self("RLC");
    const RL: Self = Self("RL");
    const RRC: Self = Self("RRC");
    const RR: Self = Self("RR");
    const SLA: Self = Self("SLA");
    const SRA: Self = Self("SRA");
    const SRL: Self = Self("SRL");
    const BIT: Self = Self("BIT");
    const SET: Self = Self("SET");
    const RES: Self = Self("RES");
    const JP: Self = Self("JP");
    const JR: Self = Self("JR");
    const CALL: Self = Self("CALL");
    const RST: Self = Self("RST");
    const RET: Self = Self("RET");
    const RETI: Self = Self("RETI");
}

const MNEMONICS: &[Mne] = &[
    Mne::LD,
    Mne::LDD,
    Mne::LDI,
    Mne::LDH,
    Mne::PUSH,
    Mne::POP,
    Mne::ADD,
    Mne::ADC,
    Mne::SUB,
    Mne::SBC,
    Mne::AND,
    Mne::OR,
    Mne::XOR,
    Mne::CP,
    Mne::INC,
    Mne::DEC,
    Mne::SWAP,
    Mne::DAA,
    Mne::CPL,
    Mne::CCF,
    Mne::SCF,
    Mne::NOP,
    Mne::HALT,
    Mne::STOP,
    Mne::DI,
    Mne::EI,
    Mne::RLCA,
    Mne::RLA,
    Mne::RRCA,
    Mne::RRA,
    Mne::RLC,
    Mne::RL,
    Mne::RRC,
    Mne::RR,
    Mne::SLA,
    Mne::SRA,
    Mne::SRL,
    Mne::BIT,
    Mne::SET,
    Mne::RES,
    Mne::JP,
    Mne::JR,
    Mne::CALL,
    Mne::RST,
    Mne::RET,
    Mne::RETI,
];

const DIRECTIVES: &[(&'static str, Tok)] = &[
    ("BYTE", Tok::BYTE),
    ("WORD", Tok::WORD),
    ("SECTION", Tok::SECTION),
    ("INCLUDE", Tok::INCLUDE),
    ("IF", Tok::IF),
    ("IFDEF", Tok::IFDEF),
    ("IFNDEF", Tok::IFNDEF),
    ("END", Tok::END),
    ("SPACE", Tok::SPACE),
    ("MACRO", Tok::MACRO),
    ("LOOP", Tok::LOOP),
    ("FAIL", Tok::FAIL),
    ("STRUCT", Tok::STRUCT),
    ("TAG", Tok::TAG),
    ("LEN", Tok::LEN),
    ("NARG", Tok::NARG),
    ("SHIFT", Tok::SHIFT),
    ("UNIQ", Tok::UNIQ),
    ("JOIN", Tok::JOIN),
    ("BREAK", Tok::BREAK),
];

const DIGRAPHS: &[(&'static str, Tok)] = &[
    ("<<", Tok::ASL),
    (">>", Tok::ASR),
    ("~>", Tok::LSR),
    ("<=", Tok::LTE),
    (">=", Tok::GTE),
    ("==", Tok::DEQU),
    ("!=", Tok::NEQU),
    ("&&", Tok::AND),
    ("||", Tok::OR),
    ("::", Tok::DCOLON),
    ("**", Tok::DSTAR),
    ("AF", Tok::AF),
    ("BC", Tok::BC),
    ("DE", Tok::DE),
    ("HL", Tok::HL),
    ("SP", Tok::SP),
    ("NC", Tok::NC),
    ("NZ", Tok::NZ),
];

trait TokStream<'a> {
    fn err(&self, msg: &str) -> io::Error;

    fn peek(&mut self) -> io::Result<Tok>;

    fn eat(&mut self);

    fn rewind(&mut self) -> io::Result<()>;

    fn str(&self) -> &str;

    fn num(&self) -> i32;

    fn pos(&self) -> Pos<'a>;
}

struct Lexer<'a, R> {
    reader: CharReader<R>,
    string: String,
    number: i32,
    stash: Option<Tok>,
    pos: Pos<'a>,
}

impl<'a, R: Read + Seek> Lexer<'a, R> {
    fn new(reader: R, file: &'a Path) -> Self {
        Self {
            reader: CharReader::new(reader),
            string: String::new(),
            number: 0,
            stash: None,
            pos: Pos {
                file,
                line: 1,
                col: 1,
            },
        }
    }
}

impl<'a, R: Read + Seek> TokStream<'a> for Lexer<'a, R> {
    fn err(&self, msg: &str) -> io::Error {
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: {msg}",
                self.pos.file.display(),
                self.pos.line,
                self.pos.col
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if let Some(tok) = self.stash {
            return Ok(tok);
        }
        // skip whitespace (except newlines)
        while let Some(c) = self.reader.peek()? {
            if !c.is_whitespace() || (c == '\n') {
                break;
            }
            self.reader.eat();
        }
        // skip comment
        if let Some(';') = self.reader.peek()? {
            while !matches!(self.reader.peek()?, Some('\n')) {
                self.reader.eat();
            }
        }
        // token start
        self.pos.line = self.reader.line;
        self.pos.col = self.reader.col;
        match self.reader.peek()? {
            None => {
                self.reader.eat();
                self.stash = Some(Tok::EOF);
                Ok(Tok::EOF)
            }
            Some('\\') => {
                self.reader.eat();
                if let Some('\n') = self.reader.peek()? {
                    self.reader.eat();
                    return self.peek(); // TODO shouldn't recurse
                }
                // macro argument
                if let Some(c) = self.reader.peek()? {
                    if c.is_ascii_digit() {
                        while let Some(c) = self.reader.peek()? {
                            if !c.is_ascii_digit() {
                                break;
                            }
                            self.string.push(c);
                            self.reader.eat();
                        }
                        self.number = i32::from_str_radix(&self.string, 10)
                            .map_err(|e| self.err(&e.to_string()))?;
                        self.stash = Some(Tok::ARG);
                        return Ok(Tok::ARG);
                    }
                }
                // some other directive
                while let Some(c) = self.reader.peek()? {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    self.reader.eat();
                    self.string.push(c);
                }
                if let Some(tok) = DIRECTIVES
                    .iter()
                    .find_map(|(s, tok)| s.eq_ignore_ascii_case(&self.string).then_some(tok))
                    .copied()
                {
                    self.stash = Some(tok);
                    return Ok(tok);
                }
                Err(self.err("unrecognized directive"))
            }
            // number
            Some(c) if c.is_ascii_digit() || (c == '$') || (c == '%') => {
                let radix = match c {
                    '$' => {
                        self.reader.eat();
                        16
                    }
                    '%' => {
                        self.reader.eat();
                        2
                    }
                    _ => 10,
                };
                // edge case: modulus
                if (c == '%') && self.reader.peek()?.is_some_and(|nc| !"01".contains(nc)) {
                    self.stash = Some(Tok::MODULUS);
                    return Ok(Tok::MODULUS);
                }
                // parse number
                while let Some(c) = self.reader.peek()? {
                    if c == '_' {
                        self.reader.eat();
                        continue; // allow '_' separators in numbers
                    }
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    self.string.push(c);
                    self.reader.eat();
                }
                self.number = i32::from_str_radix(&self.string, radix)
                    .map_err(|e| self.err(&e.to_string()))?;
                self.stash = Some(Tok::NUM);
                Ok(Tok::NUM)
            }
            // string
            Some('"') => {
                self.reader.eat();
                while let Some(c) = self.reader.peek()? {
                    if c == '"' {
                        self.reader.eat();
                        break;
                    }
                    if c == '\\' {
                        self.reader.eat();
                        self.string.push(match self.reader.peek()? {
                            Some('n') => '\n',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            Some('\\') => '\\',
                            Some('"') => '"',
                            Some('0') => '\0',
                            _ => return Err(self.err("invalid escape")),
                        });
                        self.reader.eat();
                        continue;
                    }
                    self.string.push(c);
                    self.reader.eat();
                }
                self.stash = Some(Tok::STR);
                Ok(Tok::STR)
            }
            // char
            Some('\'') => {
                self.reader.eat();
                if let Some(c) = self.reader.peek()? {
                    if c == '\\' {
                        self.reader.eat();
                        self.number = match self.reader.peek()? {
                            Some('n') => '\n',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            Some('\\') => '\\',
                            Some('\'') => '\'',
                            Some('0') => '\0',
                            _ => return Err(self.err("invalid escape")),
                        } as i32;
                    } else {
                        self.number = c as i32;
                    }
                    self.reader.eat();
                }
                if self.reader.peek()? != Some('\'') {
                    return Err(self.err("invalid character"));
                }
                self.reader.eat();
                self.stash = Some(Tok::NUM);
                return Ok(Tok::NUM);
            }
            // idents, and single chars
            Some(c) => {
                while let Some(c) = self.reader.peek()? {
                    if !c.is_ascii_alphanumeric() && !"_.".contains(c) {
                        break;
                    }
                    self.reader.eat();
                    self.string.push(c);
                }
                // c wasn't an ident, so wasnt eaten. digraph?
                if self.string.len() == 0 {
                    self.reader.eat();
                    if let Some(nc) = self.reader.peek()? {
                        if c.is_ascii() && nc.is_ascii() {
                            let s = &[c as u8, nc as u8];
                            if let Some(tok) = DIGRAPHS
                                .iter()
                                .find_map(|(bs, tok)| {
                                    bs.as_bytes().eq_ignore_ascii_case(s).then_some(tok)
                                })
                                .copied()
                            {
                                self.reader.eat();
                                self.stash = Some(tok);
                                return Ok(tok);
                            }
                        }
                    }
                }
                // we already ate both chars. digraph?
                if self.string.len() == 2 {
                    if let Some(tok) = DIGRAPHS
                        .iter()
                        .find_map(|(bs, tok)| bs.eq_ignore_ascii_case(&self.string).then_some(tok))
                        .copied()
                    {
                        self.stash = Some(tok);
                        return Ok(tok);
                    }
                }
                // maybe 1 char identifier
                if self.string.len() == 1 {
                    let c = self.string.chars().next().expect("must exist");
                    let tok = match c.to_ascii_uppercase() {
                        'A' => Tok::A,
                        'B' => Tok::B,
                        'C' => Tok::C,
                        'D' => Tok::D,
                        'E' => Tok::E,
                        'H' => Tok::H,
                        'L' => Tok::L,
                        'Z' => Tok::Z,
                        _ => Tok::ID,
                    };
                    self.stash = Some(tok);
                    return Ok(tok);
                }
                // ident?
                if !self.string.is_empty() {
                    self.stash = Some(Tok::ID);
                    return Ok(Tok::ID);
                }
                // else return an uppercase of whatever this char is
                self.stash = Some(Tok(c.to_ascii_uppercase()));
                Ok(Tok(c.to_ascii_uppercase()))
            }
        }
    }

    fn eat(&mut self) {
        self.string.clear();
        self.stash.take();
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.string.clear();
        self.stash = None;
        self.pos.line = 1;
        self.pos.col = 1;
        self.reader.rewind()
    }

    fn str(&self) -> &str {
        &self.string
    }

    fn num(&self) -> i32 {
        self.number
    }

    fn pos(&self) -> Pos<'a> {
        self.pos
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MacroTok<'a> {
    Tok(Tok),
    Str(&'a str),
    Id(&'a str),
    Num(i32),
    Arg(usize),
    Narg,
    Shift,
    Uniq,
    Join(&'a [(Pos<'a>, MacroTok<'a>)]),
}

#[derive(Clone, Copy)]
struct Macro<'a> {
    toks: &'a [(Pos<'a>, MacroTok<'a>)],
}

struct MacroInvocation<'a> {
    inner: Macro<'a>,
    name: &'a str,
    unique: usize,
    index: usize,
    fmt_buf: String,
    args: VecDeque<&'a [(Pos<'a>, MacroTok<'a>)]>,
    arg_index: usize,
    pos: Pos<'a>,
}

impl<'a> TokStream<'a> for MacroInvocation<'a> {
    fn err(&self, msg: &str) -> io::Error {
        let pos = self.pos();
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: in macro \"{}\"\n\t{}:{}:{}: {msg}",
                self.pos.file.display(),
                self.pos.line,
                self.pos.col,
                self.name,
                pos.file.display(),
                pos.line,
                pos.col
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if self.index == self.inner.toks.len() {
            return Ok(Tok::EOF);
        }
        match self.inner.toks[self.index] {
            (_, MacroTok::Tok(tok)) => Ok(tok),
            (_, MacroTok::Str(_)) => Ok(Tok::STR),
            (_, MacroTok::Id(_)) => Ok(Tok::ID),
            (_, MacroTok::Num(_)) => Ok(Tok::NUM),
            (_, MacroTok::Arg(index)) => {
                if index >= self.args.len() {
                    return Err(self.err("argument is undefined"));
                }
                match self.args[index][self.arg_index] {
                    (_, MacroTok::Tok(tok)) => Ok(tok),
                    (_, MacroTok::Str(_)) => Ok(Tok::STR),
                    (_, MacroTok::Id(_)) => Ok(Tok::ID),
                    (_, MacroTok::Num(_)) => Ok(Tok::NUM),
                    _ => unreachable!(),
                }
            }
            (_, MacroTok::Narg) => Ok(Tok::NUM),
            (_, MacroTok::Shift) => {
                if self.args.is_empty() {
                    return Err(self.err("no arguments to shift"));
                }
                Ok(Tok::NEWLINE)
            }
            (_, MacroTok::Uniq) => {
                self.fmt_buf.clear();
                write!(&mut self.fmt_buf, "_{}", self.unique).unwrap();
                Ok(Tok::ID)
            }
            (_, MacroTok::Join(toks)) => {
                self.fmt_buf.clear();
                for (_, tok) in toks {
                    match tok {
                        MacroTok::Id(string) => self.fmt_buf.push_str(string),
                        MacroTok::Str(string) => self.fmt_buf.push_str(string),
                        MacroTok::Num(val) => write!(&mut self.fmt_buf, "{val:X}").unwrap(),
                        MacroTok::Arg(index) => match self.args[*index][self.arg_index] {
                            (_, MacroTok::Str(string)) => self.fmt_buf.push_str(string),
                            (_, MacroTok::Id(string)) => self.fmt_buf.push_str(string),
                            _ => unreachable!(),
                        },
                        MacroTok::Uniq => write!(&mut self.fmt_buf, "_{}", self.unique).unwrap(),
                        _ => unreachable!(),
                    }
                }
                match toks.first().unwrap() {
                    (_, MacroTok::Id(_)) => Ok(Tok::ID),
                    (_, MacroTok::Str(_)) => Ok(Tok::STR),
                    (_, MacroTok::Num(_)) => Ok(Tok::STR),
                    (_, MacroTok::Arg(index)) => match self.args[*index][self.arg_index] {
                        (_, MacroTok::Str(_)) => Ok(Tok::STR),
                        (_, MacroTok::Id(_)) => Ok(Tok::ID),
                        _ => unreachable!(),
                    },
                    (_, MacroTok::Uniq) => Ok(Tok::ID),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eat(&mut self) {
        match self.inner.toks[self.index] {
            (_, MacroTok::Shift) => {
                self.args.pop_front();
            }
            (_, MacroTok::Arg(index)) => {
                self.arg_index += 1;
                if self.arg_index < self.args[index].len() {
                    return;
                }
                self.arg_index = 0;
            }
            _ => {}
        }
        self.index += 1;
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.index = 0;
        Ok(())
    }

    fn str(&self) -> &str {
        match self.inner.toks[self.index] {
            (_, MacroTok::Str(string)) => string,
            (_, MacroTok::Id(string)) => string,
            (_, MacroTok::Arg(index)) => match self.args[index][self.arg_index] {
                (_, MacroTok::Str(string)) => string,
                (_, MacroTok::Id(string)) => string,
                _ => unreachable!(),
            },
            (_, MacroTok::Join(_)) => &self.fmt_buf,
            (_, MacroTok::Uniq) => &self.fmt_buf,
            _ => unreachable!(),
        }
    }

    fn num(&self) -> i32 {
        match self.inner.toks[self.index] {
            (_, MacroTok::Num(val)) => val,
            (_, MacroTok::Arg(index)) => match self.args[index][self.arg_index] {
                (_, MacroTok::Num(val)) => val,
                _ => unreachable!(),
            },
            (_, MacroTok::Narg) => self.args.len() as i32,
            _ => unreachable!(),
        }
    }

    fn pos(&self) -> Pos<'a> {
        self.inner.toks[self.index].0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LoopTok<'a> {
    Tok(Tok),
    Str(&'a str),
    Id(&'a str),
    Num(i32),
    Iter,
    Join(&'a [(Pos<'a>, LoopTok<'a>)]),
}

struct Loop<'a> {
    toks: &'a [(Pos<'a>, LoopTok<'a>)],
    index: usize,
    join_buf: String,
    iter: usize,
    end: usize,
}

impl<'a> TokStream<'a> for Loop<'a> {
    fn err(&self, msg: &str) -> io::Error {
        let pos = self.pos();
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: in loop index {}: {msg}",
                pos.file.display(),
                pos.line,
                pos.col,
                self.iter,
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if self.iter >= self.end {
            return Ok(Tok::EOF);
        }
        match self.toks[self.index] {
            (_, LoopTok::Tok(tok)) => Ok(tok),
            (_, LoopTok::Str(_)) => Ok(Tok::STR),
            (_, LoopTok::Id(_)) => Ok(Tok::ID),
            (_, LoopTok::Num(_)) => Ok(Tok::NUM),
            (_, LoopTok::Iter) => Ok(Tok::NUM),
            (_, LoopTok::Join(toks)) => {
                self.join_buf.clear();
                for (_, tok) in toks {
                    match tok {
                        LoopTok::Id(string) => self.join_buf.push_str(string),
                        LoopTok::Str(string) => self.join_buf.push_str(string),
                        LoopTok::Num(val) => write!(&mut self.join_buf, "{val:X}").unwrap(),
                        LoopTok::Iter => write!(&mut self.join_buf, "{}", self.iter).unwrap(),
                        _ => unreachable!(),
                    }
                }
                match toks.first().unwrap() {
                    (_, LoopTok::Id(_)) => Ok(Tok::ID),
                    (_, LoopTok::Str(_)) => Ok(Tok::STR),
                    (_, LoopTok::Num(_)) => Ok(Tok::STR),
                    (_, LoopTok::Iter) => Ok(Tok::STR),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eat(&mut self) {
        self.index += 1;
        if self.index == self.toks.len() {
            self.iter += 1;
            self.index = 0;
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        unreachable!()
    }

    fn str(&self) -> &str {
        match self.toks[self.index] {
            (_, LoopTok::Str(string)) => string,
            (_, LoopTok::Id(string)) => string,
            (_, LoopTok::Join(_)) => &self.join_buf,
            _ => unreachable!(),
        }
    }

    fn num(&self) -> i32 {
        match self.toks[self.index] {
            (_, LoopTok::Num(val)) => val,
            (_, LoopTok::Iter) => self.iter as i32,
            _ => unreachable!(),
        }
    }

    fn pos(&self) -> Pos<'a> {
        self.toks[self.index].0
    }
}

struct CharReader<R> {
    reader: R,
    line: u32,
    col: u32,
    stash: Option<char>,
}

impl<R: Read + Seek> CharReader<R> {
    fn new(reader: R) -> Self {
        Self {
            reader,
            line: 1,
            col: 1,
            stash: None,
        }
    }

    fn peek(&mut self) -> io::Result<Option<char>> {
        if self.stash.is_none() {
            let mut buf = [0, 0, 0, 0];
            for i in 0..buf.len() {
                match self.reader.read_exact(&mut buf[i..i + 1]) {
                    Err(err) if err.kind() == ErrorKind::UnexpectedEof => {
                        if i == 0 {
                            self.stash = None;
                            return Ok(self.stash);
                        }
                        match str::from_utf8(&buf[..i + 1]) {
                            Err(err) => return Err(io::Error::new(ErrorKind::InvalidData, err)),
                            Ok(s) => {
                                self.stash = Some(s.chars().next().expect("must be valid"));
                                return Ok(self.stash);
                            }
                        }
                    }
                    Err(err) => return Err(err),
                    Ok(()) => match str::from_utf8(&buf[..i + 1]) {
                        Err(err) => return Err(io::Error::new(ErrorKind::InvalidData, err)),
                        Ok(s) => {
                            self.stash = Some(s.chars().next().expect("must be valid"));
                            return Ok(self.stash);
                        }
                    },
                }
            }
            return Err(io::Error::new(
                ErrorKind::InvalidData,
                str::from_utf8(&buf).expect_err("must be err"),
            ));
        }
        Ok(self.stash)
    }

    fn eat(&mut self) {
        self.col += 1;
        if let Some('\n') = self.stash.take() {
            self.line += 1;
            self.col = 1;
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.line = 1;
        self.col = 1;
        self.stash = None;
        self.reader.rewind()
    }
}
