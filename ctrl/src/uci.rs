use crate::utils::DeserializeStdin;
use crate::CtrlContext;
use crate::{utils::HandlerExtSerde, Error, ErrorKind};
use chrono::{offset::Utc, DateTime};
use clap::Parser;
use rpc_toolkit::{from_fn, Context, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::io::{BufWriter, Seek as _, Write as _};
use uciedit::{parse_config, Arena, Line, LineComment, Token};

pub fn uci<C: CtrlContext>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
        .subcommand("edit", from_fn(edit::<C>).with_display_serializable())
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Section {
    pub ty: String,
    pub name: Option<String>,
    pub options: HashMap<String, String>,
    pub lists: HashMap<String, Vec<String>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UciFile {
    pub sections: Vec<Section>,
    pub modified: Option<DateTime<Utc>>,
}

#[derive(Parser, Serialize, Deserialize)]
pub struct FilesGet {
    name: String,
}

pub fn get<C: CtrlContext>(ctx: C, FilesGet { name }: FilesGet) -> Result<UciFile, Error> {
    let path = ctx.uci_path(&name);
    let mut sections = Vec::new();
    let modified = std::fs::metadata(&path)
        .and_then(|m| m.modified())
        .map(DateTime::<Utc>::from)
        .ok();
    parse_config(path, |mut cfg| {
        while cfg.step() {
            let ty = cfg.ty().into_owned();
            let name = cfg.name().map(|n| n.into_owned());
            let mut options = HashMap::new();
            let mut lists = HashMap::<_, Vec<_>>::new();
            for line in cfg.lines() {
                match line {
                    Line::Option { option, value, .. } => {
                        options.insert(option.unquoted_string(), value.unquoted_string());
                    }
                    Line::List { list, item, .. } => {
                        lists
                            .entry(list.unquoted_string())
                            .or_default()
                            .push(item.unquoted_string());
                    }
                    _ => (),
                }
            }
            sections.push(Section {
                ty,
                name,
                options,
                lists,
            })
        }
        Ok::<_, Error>(())
    })?;
    Ok(UciFile { sections, modified })
}

type Files = BTreeMap<String, UciFile>;

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(files): DeserializeStdin<Files>,
) -> Result<(), Error> {
    use fd_lock_rs::{FdLock, LockType};

    // Lock all the files at once.
    // We do it in lexicographic order so that deadlocks are impossible.
    let files = files
        .into_iter()
        .map(|(name, section)| {
            let path = ctx.uci_path(&name);
            let file = std::fs::File::options()
                .create(true)
                .write(true)
                .truncate(false)
                .open(&path)?;
            let locked = FdLock::lock(file, LockType::Exclusive, true)?;
            Ok::<_, Error>((name, section, locked))
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Check all the modified times.
    for (name, tosave, saved) in &files {
        match (
            saved
                .metadata()
                .and_then(|m| m.modified())
                .map(DateTime::<Utc>::from),
            tosave.modified,
        ) {
            (Ok(saved_time), Some(tosave_time)) if saved_time > tosave_time => {
                return Err(ErrorKind::UciSetConflict { name: name.clone() }.into());
            }
            _ => (),
        }
    }

    // Actually write the sections.
    let arena = Arena::new();
    for (_, tosave, mut saved) in files {
        // Only truncating now that we've checked the modified times.
        saved.set_len(0)?;
        saved.seek(std::io::SeekFrom::Start(0))?;
        let mut writer = BufWriter::new(&mut *saved);
        for section in tosave.sections {
            write!(
                &mut writer,
                "{}",
                Line::Section {
                    ty: Token::from_string(section.ty, &arena),
                    name: section.name.map(|s| Token::from_string(s, &arena)),
                    comment: LineComment::None,
                }
            )?;
            for (option, value) in section.options.into_iter() {
                write!(
                    &mut writer,
                    "{}",
                    Line::Option {
                        option: Token::from_string(option, &arena),
                        value: Token::from_string(value, &arena),
                        comment: LineComment::None,
                    }
                )?;
            }
            for (list, items) in section.lists.into_iter() {
                for item in items {
                    write!(
                        &mut writer,
                        "{}",
                        Line::List {
                            list: Token::from_str(&list, &arena),
                            item: Token::from_string(item, &arena),
                            comment: LineComment::None,
                        }
                    )?;
                }
            }
            write!(&mut writer, "{}", Line::Empty)?;
        }
    }

    Ok(())
}

pub fn edit<C: CtrlContext>(ctx: C, args: FilesGet) -> Result<(), Error> {
    let name = args.name.clone();
    let current_file = get(ctx.clone(), args)?;
    let modified_file = crate::utils::edit_in_editor(&current_file)?;

    // Wrap the single file in a BTreeMap for set
    let mut files = BTreeMap::new();
    files.insert(name, modified_file);

    set(ctx, DeserializeStdin(files))
}
