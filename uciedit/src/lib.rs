extern crate self as uciedit;

pub use inpt::inpt;
use inpt::split::{unescape, Quoted, SingleQuoted, Spaced};
use inpt::{inpt_step, Inpt, InptStep};
use std::fmt::Display;
use std::io::{self, BufRead, BufWriter, Seek};
use std::str::{FromStr, Utf8Error};
use std::{borrow::Cow, fs::File, path::Path};
use std::{fmt, fs};
pub use uciedit_macros::UciSection;

pub mod openwrt;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Utf8(#[from] Utf8Error),
    #[error(transparent)]
    FdLock(#[from] fd_lock_rs::Error),
    #[error("bad section type on line {line_number}: {desc}")]
    BadSection { line_number: usize, desc: String },
    #[error("bad option on line {line_number}: {desc}")]
    BadOption { line_number: usize, desc: String },
    #[error("bad list on line {line_number}: {desc}")]
    BadList { line_number: usize, desc: String },
    #[error("unknown uci keyword {found:?} on {line_number}")]
    UnknownKeyword { line_number: usize, found: String },
    #[error("expected uci section on line {line_number}")]
    ExpectedSection { line_number: usize },
    #[error("expected {found:?} section on line {line_number} to be {expected:?}")]
    ExpectedSectionType {
        line_number: usize,
        expected: String,
        found: String,
    },
    #[error("error parsing a value on line {line_number}: {error:?}")]
    ValueDyn {
        line_number: usize,
        error: Box<dyn std::error::Error + Sync + Send>,
    },
    #[error("error parsing a value on line {line_number}: {desc:?}")]
    ValueMsg { line_number: usize, desc: String },
    #[error("error parsing a value on line {line_number}: {found:?} should be a boolean")]
    ValueBoolean { line_number: usize, found: String },
    #[error("missing option {missing:?} on line {line_number}")]
    MissingOption { line_number: usize, missing: String },
}

impl Error {
    pub fn parse(
        error: impl std::error::Error + Sync + Send + 'static,
        line_number: usize,
    ) -> Self {
        Error::ValueDyn {
            line_number,
            error: Box::new(error),
        }
    }
}

pub fn parse_config<V, E: From<Error> + From<io::Error>>(
    path: impl AsRef<Path>,
    with: impl FnOnce(Sections) -> Result<V, E>,
) -> Result<V, E> {
    let text = match fs::read_to_string(path) {
        Ok(text) => text,
        Err(err) if err.kind() == io::ErrorKind::NotFound => String::new(),
        Err(err) => return Err(err.into()),
    };
    parse_config_string(&text, with)
}

pub fn parse_config_string<V, E: From<Error>>(
    config: &str,
    with: impl FnOnce(Sections) -> Result<V, E>,
) -> Result<V, E> {
    let lines = config
        .lines()
        .enumerate()
        .map(|(n, l)| Line::parse(l, n))
        .collect::<Result<_, _>>()?;
    let arena = Arena::new();
    with(Sections {
        lines: &lines,
        arena: &arena,
        index: 0,
        started: false,
    })
}

// TODO: we need a way to open multiple configs, and cancel all if any of the
// rewrites error
// TODO: a lot of rewrites require multiple passes. right now this is done
// with [SectionsMut::reset] but would be nicer if open and visit were separate
// options, so one open could result in many visits. this is related to opening
// multiple configs
// TODO: async version?
pub fn rewrite_config<V, E: From<Error> + From<io::Error>>(
    path: impl AsRef<Path>,
    with: impl for<'a> FnOnce(SectionsMut) -> Result<V, E>,
) -> Result<V, E> {
    use std::io::Write;

    use fd_lock_rs::{FdLock, LockType};
    use std::io::BufReader;
    let file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(path)?;
    let mut locked = FdLock::lock(file, LockType::Exclusive, true).map_err(Error::FdLock)?;
    let mut lines = Vec::new();
    let arena = Arena::new();
    for line in BufReader::new(&mut *locked).lines() {
        let line = arena.alloc(line?);
        let parse = Line::parse(line, lines.len())?;
        lines.push(parse);
    }
    let v = with(SectionsMut {
        lines: &mut lines,
        index: 0,
        arena: &arena,
        section_start: None,
        retain: true,
    })?;
    locked.set_len(0)?;
    locked.seek(std::io::SeekFrom::Start(0))?;
    let mut writer = BufWriter::new(&mut *locked);
    for line in lines {
        write!(writer, "{}", line)?;
    }
    Ok(v)
}

pub fn rewrite_config_string<E: From<Error> + From<io::Error>>(
    config: String,
    with: impl for<'a> FnOnce(SectionsMut) -> Result<(), E>,
) -> Result<String, E> {
    use std::io::Write;

    let mut lines = Vec::new();
    let arena = Arena::new();
    for (num, line) in arena.alloc(config).lines().enumerate() {
        lines.push(Line::parse(line, num)?);
    }
    with(SectionsMut {
        lines: &mut lines,
        index: 0,
        arena: &arena,
        section_start: None,
        retain: true,
    })?;
    let mut writer = io::Cursor::new(Vec::new());
    for line in lines {
        write!(writer, "{}", line)?;
    }
    String::from_utf8(writer.into_inner()).map_err(|err| Error::Utf8(err.utf8_error()).into())
}

pub type Lines<'a> = Vec<Line<'a>>;
pub type Arena = typed_arena::Arena<String>;

pub struct Sections<'a> {
    lines: &'a Lines<'a>,
    arena: &'a Arena,
    index: usize,
    started: bool,
}

impl<'a> Sections<'a> {
    pub fn ty(&self) -> Cow<str> {
        if !self.started {
            panic!("call step at least once");
        }
        if let Line::Section { ty, .. } = &self.lines[self.index] {
            return ty.as_str();
        }
        panic!("section ctx not at a section")
    }

    pub fn name(&self) -> Option<Cow<str>> {
        if !self.started {
            panic!("call step at least once");
        }
        if let Line::Section { name, .. } = &self.lines[self.index] {
            return name.as_ref().map(|n| n.as_str());
        }
        panic!("section ctx not at a section")
    }

    pub fn get<S: UciSection<'a>>(&self) -> Result<S, Error> {
        if !self.started {
            panic!("call step at least once");
        }
        S::read(self.lines, self.arena, self.index)
    }

    /// like [Sections::get] but returns None if the section is a different type
    pub fn get_typed<S: UciSection<'a>>(&self) -> Result<Option<S>, Error> {
        if !S::is_type(&self.ty()) {
            return Ok(None);
        }
        match self.get() {
            Ok(s) => Ok(Some(s)),
            Err(e) => Err(e),
        }
    }

    pub fn each<S: UciSection<'a>>(
        &mut self,
        mut with: impl FnMut(Option<&str>, S),
    ) -> Result<(), Error> {
        self.restart();
        while self.step() {
            if S::is_type(&self.ty()) {
                let name = self.name();
                with(name.as_deref(), self.get()?);
            }
        }
        Ok(())
    }

    pub fn try_each<S: UciSection<'a>, E>(
        &mut self,
        mut with: impl FnMut(Option<&str>, S) -> Result<(), E>,
    ) -> Result<(), E>
    where
        E: From<Error>,
    {
        self.restart();
        while self.step() {
            if S::is_type(&self.ty()) {
                let name = self.name();
                with(name.as_deref(), self.get()?)?;
            }
        }
        Ok(())
    }

    pub fn restart(&mut self) {
        self.index = 0;
        self.started = false;
    }

    pub fn step(&mut self) -> bool {
        if self.started {
            self.index += 1;
        }

        self.started = true;
        while let Some(line) = self.lines.get(self.index) {
            match line {
                Line::Section { .. } => {
                    return true;
                }
                _ => {
                    self.index += 1;
                    continue;
                }
            };
        }

        // Got to the end
        self.started = false;
        false
    }
}

pub struct SectionsMut<'l, 'a> {
    lines: &'l mut Lines<'a>,
    index: usize,
    arena: &'a Arena,
    section_start: Option<usize>,
    retain: bool,
}

impl<'a> SectionsMut<'_, 'a> {
    pub fn readonly(&mut self) -> Sections<'_> {
        Sections {
            lines: self.lines,
            arena: self.arena,
            index: self.index,
            started: self.section_start.is_some(),
        }
    }

    pub fn ty(&self) -> Cow<str> {
        if self.section_start.is_none() {
            panic!("call step at least once");
        }
        if let Line::Section { ty, .. } = &self.lines[self.index] {
            return ty.as_str();
        }
        panic!("section ctx not at a section")
    }

    pub fn name(&self) -> Option<Cow<str>> {
        if self.section_start.is_none() {
            panic!("call step at least once");
        }
        if let Line::Section { name, .. } = &self.lines[self.index] {
            return name.as_ref().map(|n| n.as_str());
        }
        panic!("section ctx not at a section")
    }

    pub fn get<S: UciSection<'a>>(&self) -> Result<S, Error> {
        if self.section_start.is_none() {
            panic!("call step at least once");
        }
        S::read(self.lines, self.arena, self.index)
    }

    /// like [SectionsMut::get] but returns None if the section is a different type
    pub fn get_typed<S: UciSection<'a>>(&self) -> Result<Option<S>, Error> {
        if !S::is_type(&self.ty()) {
            return Ok(None);
        }
        match self.get() {
            Ok(s) => Ok(Some(s)),
            Err(e) => Err(e),
        }
    }

    pub fn set<S: UciSection<'a>>(&mut self, section: &S) -> Result<(), Error> {
        if self.section_start.is_none() {
            panic!("call step at least once");
        }
        section.write(self.lines, self.arena, self.index)
    }

    pub fn push<S: UciSection<'a>>(
        &mut self,
        section: &S,
        name: Option<&str>,
    ) -> Result<(), Error> {
        section.append(
            self.lines,
            self.arena,
            name.map(|n| self.arena.alloc(n.to_string()).as_str()),
        )
    }

    pub fn remove(&mut self) {
        self.set_retain(false);
    }

    pub fn set_retain(&mut self, retain: bool) {
        if self.section_start.is_none() {
            panic!("call step at least once");
        }
        self.retain = retain;
    }

    pub fn restart(&mut self) {
        // make sure flagged sections are removed
        while self.step() {}
        self.index = 0;
        self.section_start = None;
    }

    pub fn step(&mut self) -> bool {
        if let Some(first_index) = self.section_start {
            if self.retain {
                // Retain the section and move on
                self.index += 1;
            } else {
                // Remove the section
                let mut last_index = self.index;
                for i in self.index + 1..self.lines.len() {
                    if matches!(self.lines[i], Line::Section { .. }) {
                        break;
                    }
                    if self.lines[i].is_in_section() {
                        last_index = i;
                    }
                }
                self.lines.splice(first_index..=last_index, []);
                self.index = first_index;
            }
        }

        let mut first_index = self.index;
        while let Some(line) = self.lines.get(self.index) {
            match line {
                Line::Section { .. } => {
                    self.section_start = Some(first_index);
                    self.retain = true;
                    return true;
                }
                Line::Empty => {
                    self.index += 1;
                    first_index = self.index;
                    continue;
                }
                _ if line.is_in_section() => {
                    self.index += 1;
                    first_index = self.index;
                    continue;
                }
                _ => {
                    self.index += 1;
                    continue;
                }
            };
        }

        // Got to the end. If called a second time, check the same index.
        self.section_start = None;
        false
    }
}

impl Drop for SectionsMut<'_, '_> {
    fn drop(&mut self) {
        // make sure flagged sections are removed
        while self.step() {}
    }
}

pub trait UciSection<'a>: Sized {
    fn is_type(ty: &str) -> bool;
    fn read(lines: &Lines<'a>, arena: &'a Arena, index: usize) -> Result<Self, Error>;
    fn write(&self, lines: &mut Lines<'a>, arena: &'a Arena, index: usize) -> Result<(), Error>;
    fn append(
        &self,
        lines: &mut Lines<'a>,
        arena: &'a Arena,
        name: Option<&'a str>,
    ) -> Result<(), Error>;
}

#[derive(Debug, Inpt, Clone, Copy)]
#[inpt(trim = "")] // TODO: trim isn't working?
pub enum LineComment<'a> {
    #[inpt(regex = r"\s*")]
    None,
    #[inpt(regex = r"\s*#(.*)")]
    Commented(&'a str),
}

impl fmt::Display for LineComment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LineComment::None => Ok(()),
            LineComment::Commented(text) => write!(f, " #{}", text),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Line<'a> {
    Empty,
    Comment {
        indent: bool,
        text: &'a str,
    },
    Section {
        ty: Token<'a>,
        name: Option<Token<'a>>,
        comment: LineComment<'a>,
    },
    Option {
        option: Token<'a>,
        value: Token<'a>,
        comment: LineComment<'a>,
    },
    List {
        list: Token<'a>,
        item: Token<'a>,
        comment: LineComment<'a>,
    },
    Skip,
}

impl fmt::Display for Line<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Line::Empty => writeln!(f),
            Line::Comment {
                indent: false,
                text,
            } => writeln!(f, "#{}", text),
            Line::Comment { indent: true, text } => writeln!(f, "\t#{}", text),
            Line::Section {
                ty,
                name: None,
                comment: tail,
            } => writeln!(f, "config {}{}", ty, tail),
            Line::Section {
                ty,
                name: Some(name),
                comment: tail,
            } => writeln!(f, "config {} {}{}", ty, name, tail),
            Line::Option {
                option,
                value,
                comment: tail,
            } => writeln!(f, "\toption {} {}{}", option, value, tail),
            Line::List {
                list,
                item,
                comment: tail,
            } => writeln!(f, "\tlist {} {}{}", list, item, tail),
            Line::Skip => Ok(()),
        }
    }
}

impl<'a> Line<'a> {
    pub fn parse(line: &'a str, line_number: usize) -> Result<Self, Error> {
        #[derive(Inpt, Clone, Copy)]
        enum ConfigLine<'a> {
            Named(Token<'a>, Token<'a>, LineComment<'a>),
            Unnamed(Token<'a>, LineComment<'a>),
        }

        let rest = line.trim();
        if rest.is_empty() {
            return Ok(Line::Empty);
        }
        if let Some(rest) = rest.strip_prefix('#') {
            return Ok(Line::Comment {
                indent: line.starts_with(char::is_whitespace),
                text: rest,
            });
        }
        let InptStep {
            data: Ok(keyword),
            rest,
        } = inpt_step::<Token>(rest)
        else {
            unreachable!()
        };
        Ok(match &*keyword.as_str() {
            "config" => {
                match inpt(rest).map_err(|err| Error::BadSection {
                    line_number,
                    desc: err.to_string(),
                })? {
                    ConfigLine::Named(ty, name, comment) => Line::Section {
                        ty,
                        name: Some(name),
                        comment,
                    },
                    ConfigLine::Unnamed(ty, comment) => Line::Section {
                        ty,
                        name: None,
                        comment,
                    },
                }
            }
            "option" => {
                let (option, value, comment): (Token, Token, LineComment) =
                    inpt(rest).map_err(|err| Error::BadOption {
                        line_number,
                        desc: err.to_string(),
                    })?;
                Line::Option {
                    option,
                    value,
                    comment,
                }
            }
            "list" => {
                let (list, item, comment): (Token, Token, LineComment) =
                    inpt(rest).map_err(|err| Error::BadList {
                        line_number,
                        desc: err.to_string(),
                    })?;
                Line::List {
                    list,
                    item,
                    comment,
                }
            }
            kw => {
                return Err(Error::UnknownKeyword {
                    line_number,
                    found: kw.into(),
                })
            }
        })
    }

    pub fn is_in_section(&self) -> bool {
        matches!(
            self,
            Line::Comment { indent: true, .. } | Line::Option { .. } | Line::List { .. }
        )
    }

    pub fn option_from_display(option: &'a str, value: impl Display, arena: &'a Arena) -> Self {
        let option = Token::from_str(option, arena);
        Line::Option {
            option,
            value: Token::from_display(value, arena),
            comment: LineComment::None,
        }
    }

    pub fn list_from_display<'i>(
        list: &'a str,
        items: &'i [impl Display],
        arena: &'a Arena,
    ) -> impl Iterator<Item = Self> + 'i
    where
        'a: 'i,
    {
        let list = Token::from_str(list, arena);
        items.iter().map(move |item| Line::List {
            list,
            item: Token::from_display(item, arena),
            comment: LineComment::None,
        })
    }

    pub fn option_from_bool(option: &'a str, value: bool, arena: &'a Arena) -> Self {
        let option = Token::from_str(option, arena);
        Line::Option {
            option,
            value: Token::from_bool(value),
            comment: LineComment::None,
        }
    }

    pub fn list_from_bool<'i>(
        list: &'a str,
        items: &'i [bool],
        arena: &'a Arena,
    ) -> impl Iterator<Item = Self> + 'i
    where
        'a: 'i,
    {
        let list = Token::from_str(list, arena);
        items.iter().map(move |item| Line::List {
            list,
            item: Token::from_bool(*item),
            comment: LineComment::None,
        })
    }

    pub fn section_from(ty: &'a str, name: Option<&'a str>, arena: &'a Arena) -> Self {
        Line::Section {
            ty: Token::from_str(ty, arena),
            name: name.map(|n| Token::from_str(n, arena)),
            comment: LineComment::None,
        }
    }
}

#[derive(Inpt, Clone, Copy, Debug)]
pub enum Token<'a> {
    Q(Quoted<&'a str>),
    Sq(SingleQuoted<&'a str>),
    W(Spaced<&'a str>),
}

impl<'a> Token<'a> {
    pub fn as_str(self) -> Cow<'a, str> {
        // TODO: inpt doesn't currently do unescaping
        match self {
            Token::Q(x) => unescape(x.inner),
            Token::Sq(x) => unescape(x.inner),
            Token::W(x) => Cow::Borrowed(x.inner),
        }
    }

    pub fn as_arena_str(self, arena: &'a Arena) -> &'a str {
        match self.as_str() {
            Cow::Borrowed(x) => x,
            Cow::Owned(x) => arena.alloc(x),
        }
    }

    pub fn from_display(s: impl fmt::Display, arena: &'a Arena) -> Self {
        Self::from_string(s.to_string(), arena)
    }

    pub fn from_string(s: String, arena: &'a Arena) -> Self {
        if s.contains(|c: char| !(c.is_alphanumeric() || ['_', '-', '.'].contains(&c))) {
            let q = arena.alloc(format!("{:?}", s));
            Token::Q(Quoted {
                inner: &q[1..q.len() - 1],
            })
        } else {
            let s = arena.alloc(s);
            Token::W(Spaced { inner: s })
        }
    }

    pub fn from_str(s: &'a str, arena: &'a Arena) -> Self {
        if s.contains(|c: char| !(c.is_alphanumeric() || ['_', '-', '.'].contains(&c))) {
            let q = arena.alloc(format!("{:?}", s));
            Token::Q(Quoted {
                inner: &q[1..q.len() - 1],
            })
        } else {
            Token::W(Spaced { inner: s })
        }
    }

    pub fn from_bool(s: bool) -> Self {
        match s {
            false => Token::W(Spaced { inner: "0" }),
            true => Token::W(Spaced { inner: "1" }),
        }
    }

    pub fn parse_fromstr<T: FromStr>(self, line_number: usize) -> Result<T, Error>
    where
        T::Err: std::error::Error + Sync + Send + 'static,
    {
        match self.as_str().parse::<T>() {
            Ok(v) => Ok(v),
            Err(e) => Err(Error::ValueDyn {
                line_number,
                error: Box::new(e),
            }),
        }
    }

    pub fn parse_inpt<T: Inpt<'a>>(self, line_number: usize, arena: &'a Arena) -> Result<T, Error> {
        match inpt(self.as_arena_str(arena)) {
            Ok(v) => Ok(v),
            Err(e) => Err(Error::ValueMsg {
                line_number,
                desc: e.to_string(),
            }),
        }
    }

    pub fn parse_bool(self, line_number: usize) -> Result<bool, Error> {
        match self.as_str().trim() {
            "0" | "no" | "off" | "false" | "disabled" => Ok(false),
            "1" | "yes" | "on" | "true" | "enabled" => Ok(true),
            other => Err(Error::ValueBoolean {
                line_number,
                found: other.to_string(),
            }),
        }
    }
}

impl PartialEq<str> for Token<'_> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Q(quoted) => write!(f, "\"{}\"", quoted.inner),
            Token::Sq(single_quoted) => write!(f, "'{}'", single_quoted.inner),
            Token::W(word) => write!(f, "{}", word.inner),
        }
    }
}

#[cfg(test)]
mod tests;
