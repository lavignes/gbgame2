use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    path::Path,
    str::FromStr,
};

mod intern;

pub use intern::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label<'a> {
    pub scope: Option<&'a str>,
    pub string: &'a str,
}

impl<'a> Label<'a> {
    pub fn new(scope: Option<&'a str>, string: &'a str) -> Self {
        Self { scope, string }
    }
}

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(scope) = self.scope {
            write!(f, "{scope}{}", self.string)
        } else {
            write!(f, "{}", self.string)
        }
    }
}

impl<'a, T: AsRef<str>> PartialEq<T> for Label<'a> {
    fn eq(&self, other: &T) -> bool {
        let other = other.as_ref();
        if let Some(scope) = self.scope {
            ((scope.len() + self.string.len()) == other.len())
                && other.starts_with(scope)
                && other.ends_with(self.string)
        } else {
            other == self.string
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Expr<'a> {
    Const(i32),
    List(&'a [ExprNode<'a>]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprNode<'a> {
    Const(i32),
    Addr(&'a str, u32),
    Op(Op),
    Label(Label<'a>),
    Tag(Label<'a>, &'a str),
}

pub struct SymFlags;

impl SymFlags {
    pub const EQU: u8 = 1 << 0;
}

#[derive(Debug)]
pub struct Sym<'a> {
    pub value: Expr<'a>,
    pub unit: &'a str,
    pub section: &'a str,
    pub pos: Pos<'a>,
    pub flags: u8,
}

pub struct RelocFlags;

impl RelocFlags {
    pub const HI: u8 = 1 << 0;
    pub const JP: u8 = 1 << 1;
}

#[derive(Debug, Clone, Copy)]
pub struct Reloc<'a> {
    pub offset: usize,
    pub width: u8,
    pub value: &'a [ExprNode<'a>],
    pub unit: &'a str,
    pub pos: Pos<'a>,
    pub flags: u8,
}

#[derive(Debug)]
pub struct Section<'a> {
    pub name: &'a str,
    pub pc: u32,
    pub data: Vec<u8>,
    pub relocs: Vec<Reloc<'a>>,
}

impl<'a> Section<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            pc: 0,
            data: Vec::new(),
            relocs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Binary(Tok),
    Unary(Tok),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Tok(pub char);

impl Tok {
    pub const NEWLINE: Self = Self('\n');
    pub const MODULUS: Self = Self('%');
    pub const SOLIDUS: Self = Self('/');
    pub const STAR: Self = Self('*');
    pub const PLUS: Self = Self('+');
    pub const MINUS: Self = Self('-');
    pub const LT: Self = Self('<');
    pub const GT: Self = Self('>');
    pub const AMP: Self = Self('&');
    pub const CARET: Self = Self('^');
    pub const PIPE: Self = Self('|');
    pub const LPAREN: Self = Self('(');
    pub const RPAREN: Self = Self(')');
    pub const LBRACKET: Self = Self('[');
    pub const RBRACKET: Self = Self(']');
    pub const BANG: Self = Self('!');
    pub const TILDE: Self = Self('~');
    pub const HASH: Self = Self('#');
    pub const COMMA: Self = Self(',');
    pub const COLON: Self = Self(':');
    pub const EQU: Self = Self('=');

    pub const A: Self = Self('A');
    pub const B: Self = Self('B');
    pub const C: Self = Self('C');
    pub const D: Self = Self('D');
    pub const E: Self = Self('E');
    pub const H: Self = Self('H');
    pub const L: Self = Self('L');
    pub const Z: Self = Self('Z');

    pub const EOF: Self = Self('\u{100000}');
    pub const ID: Self = Self('\u{100001}');
    pub const NUM: Self = Self('\u{100002}');
    pub const STR: Self = Self('\u{100003}');

    pub const BYTE: Self = Self('\u{1000D0}');
    pub const WORD: Self = Self('\u{1000D1}');
    pub const SECTION: Self = Self('\u{1000D2}');
    pub const INCLUDE: Self = Self('\u{1000D3}');
    pub const IF: Self = Self('\u{1000D4}');
    pub const IFDEF: Self = Self('\u{100D5}');
    pub const IFNDEF: Self = Self('\u{1000D6}');
    pub const END: Self = Self('\u{1000D7}');
    pub const SPACE: Self = Self('\u{1000D8}');
    pub const MACRO: Self = Self('\u{1000D9}');
    pub const LOOP: Self = Self('\u{1000DA}');
    pub const FAIL: Self = Self('\u{1000DB}');
    pub const WARN: Self = Self('\u{1000DC}');
    pub const STRUCT: Self = Self('\u{1000DD}');
    pub const CREATE: Self = Self('\u{1000DE}');
    pub const TAG: Self = Self('\u{1000DF}');
    pub const LEN: Self = Self('\u{1000E0}');

    pub const ARG: Self = Self('\u{1000F0}');
    pub const NARG: Self = Self('\u{1000F1}');
    pub const SHIFT: Self = Self('\u{1000F2}');
    pub const UNIQ: Self = Self('\u{1000F3}');
    pub const JOIN: Self = Self('\u{1000F4}');
    pub const BREAK: Self = Self('\u{1000F5}');

    pub const ASL: Self = Self('\u{100090}'); // <<
    pub const ASR: Self = Self('\u{100091}'); // >>
    pub const LSR: Self = Self('\u{100092}'); // ~>
    pub const LTE: Self = Self('\u{100093}'); // <=
    pub const GTE: Self = Self('\u{100094}'); // >=
    pub const DEQU: Self = Self('\u{100095}'); // ==
    pub const NEQU: Self = Self('\u{100096}'); // !=
    pub const AND: Self = Self('\u{100097}'); // &&
    pub const OR: Self = Self('\u{100098}'); // ||
    pub const DCOLON: Self = Self('\u{100099}'); // ::
    pub const DSTAR: Self = Self('\u{10009A}'); // **

    pub const AF: Self = Self('\u{1000A0}'); // AF
    pub const BC: Self = Self('\u{1000A1}'); // BC
    pub const DE: Self = Self('\u{1000A2}'); // DE
    pub const HL: Self = Self('\u{1000A3}'); // HL
    pub const SP: Self = Self('\u{1000A4}'); // SP
    pub const NC: Self = Self('\u{1000A5}'); // NC
    pub const NZ: Self = Self('\u{1000A6}'); // NZ
}

impl Display for Tok {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Tok::A => write!(f, "register `A`")?,
            Tok::B => write!(f, "register `B`")?,
            Tok::C => write!(f, "register/condition code `C`")?,
            Tok::D => write!(f, "register `D`")?,
            Tok::H => write!(f, "register `H`")?,
            Tok::L => write!(f, "register `L`")?,
            Tok::Z => write!(f, "condition code `Z`")?,

            Tok::EOF => write!(f, "end of file")?,
            Tok::ID => write!(f, "identifier")?,
            Tok::NUM => write!(f, "number")?,
            Tok::STR => write!(f, "string")?,

            Tok::BYTE => write!(f, "`\\BYTE`")?,
            Tok::WORD => write!(f, "`\\WORD`")?,
            Tok::SECTION => write!(f, "`\\SECTION`")?,
            Tok::INCLUDE => write!(f, "`\\INCLUDE`")?,
            Tok::IF => write!(f, "`\\IF`")?,
            Tok::IFDEF => write!(f, "`\\IFDEF`")?,
            Tok::IFNDEF => write!(f, "`\\IFNDEF`")?,
            Tok::END => write!(f, "`\\END`")?,
            Tok::SPACE => write!(f, "`\\SPACE`")?,
            Tok::MACRO => write!(f, "`\\MACRO`")?,
            Tok::LOOP => write!(f, "`\\LOOP`")?,
            Tok::FAIL => write!(f, "`\\FAIL`")?,
            Tok::WARN => write!(f, "`\\WARN`")?,
            Tok::STRUCT => write!(f, "`\\STRUCT`")?,
            Tok::CREATE => write!(f, "`\\CREATE`")?,
            Tok::TAG => write!(f, "`\\TAG`")?,
            Tok::LEN => write!(f, "`\\LEN`")?,

            Tok::ARG => write!(f, "macro argument")?,
            Tok::NARG => write!(f, "`\\NARG`")?,
            Tok::SHIFT => write!(f, "`\\SHIFT`")?,
            Tok::UNIQ => write!(f, "`\\UNIQ`")?,
            Tok::JOIN => write!(f, "`\\JOIN`")?,
            Tok::BREAK => write!(f, "`\\BREAK`")?,

            Tok::ASL => write!(f, "`<<`")?,
            Tok::ASR => write!(f, "`>>`")?,
            Tok::LSR => write!(f, "`~>`")?,
            Tok::LTE => write!(f, "`<=`")?,
            Tok::GTE => write!(f, "`>=`")?,
            Tok::DEQU => write!(f, "`==`")?,
            Tok::NEQU => write!(f, "`!=`")?,
            Tok::AND => write!(f, "`&&`")?,
            Tok::OR => write!(f, "`||`")?,
            Tok::DCOLON => write!(f, "`::`")?,
            Tok::DSTAR => write!(f, "`**`")?,

            Tok::AF => write!(f, "register pair `AF`")?,
            Tok::BC => write!(f, "register pair `BC`")?,
            Tok::DE => write!(f, "register pair `DE`")?,
            Tok::HL => write!(f, "register pair `HL`")?,
            Tok::SP => write!(f, "register `SP")?,
            Tok::NC => write!(f, "condition code `NC`")?,
            Tok::NZ => write!(f, "condition code `NZ`")?,

            Tok(c) => write!(f, "`{c}`")?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos<'a> {
    pub file: &'a Path,
    pub line: u32,
    pub col: u32,
}

pub fn parse_defines<T, U>(s: &str) -> Result<(T, U), Box<dyn Error + Send + Sync + 'static>>
where
    T: FromStr,
    T::Err: Error + Send + Sync + 'static,
    U: FromStr,
    U::Err: Error + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid SYMBOL=value: no `=` found in `{s}`"))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}
