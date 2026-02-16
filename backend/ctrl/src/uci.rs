use crate::utils::DeserializeStdin;
use crate::CtrlContext;
use crate::{utils::HandlerExtSerde, Error};
use chrono::{offset::Utc, DateTime};
use clap::Parser;
use rpc_toolkit::{from_fn, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use uciedit::{parse_all, Arena, Line, LockedConfig, Token};

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

type UciFiles = BTreeMap<String, UciFile>;

#[derive(Parser, Serialize, Deserialize)]
pub struct GetArgs {
    names: Vec<String>,
}

pub fn get<C: CtrlContext>(ctx: C, GetArgs { names }: GetArgs) -> Result<UciFiles, Error> {
    let arena = Arena::new();
    let cfgs = parse_all(ctx.uci_root(), &arena, &names)?;
    let mut files = UciFiles::new();
    for (name, cfg) in cfgs.iter() {
        let mut sections = Vec::new();
        for section in &cfg.sections {
            let ty = section.ty().into_owned();
            let name = section.name().map(|n| n.into_owned());
            let mut options = HashMap::new();
            let mut lists = HashMap::<_, Vec<_>>::new();
            for line in &section.lines {
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
        files.insert(
            name.into(),
            UciFile {
                sections,
                modified: cfg.modified,
            },
        );
    }
    Ok(files)
}

pub fn set<C: CtrlContext>(
    ctx: C,
    DeserializeStdin(files): DeserializeStdin<UciFiles>,
) -> Result<BTreeMap<String, DateTime<Utc>>, Error> {
    // Lock all the files at once.
    // We do it in lexicographic order so that deadlocks are impossible.
    let mut files = files
        .into_iter()
        .map(|(name, input)| {
            let path = ctx.uci_root().join(&name);
            Ok::<_, Error>((name, input, LockedConfig::open(path)?))
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Check all the modified times.
    for (_, input, file) in &mut files {
        if let Some(expected) = input.modified {
            file.check_modified(expected)?;
        }
    }

    // Actually write the sections.
    let mut output = BTreeMap::new();
    let mut result = Ok(());
    for (name, input, mut file) in files {
        let arena = Arena::new();
        let mut cfg = file
            .parse(&arena)
            .unwrap_or_else(|_| uciedit::Config::new(&arena));
        cfg.sections.clear();
        for input_section in input.sections {
            let mut section = uciedit::Section::new(
                &arena,
                arena.alloc(input_section.ty),
                input_section.name.map(|n| arena.alloc(n).as_str()),
            );
            for (option, value) in input_section.options {
                section.lines.push(uciedit::Line::Option {
                    option: Token::from_string(option, &arena),
                    value: Token::from_string(value, &arena),
                    comment: uciedit::LineComment::None,
                })
            }
            for (list, items) in input_section.lists {
                let list = Token::from_string(list, &arena);
                for item in items {
                    section.lines.push(uciedit::Line::List {
                        list,
                        item: Token::from_string(item, &arena),
                        comment: uciedit::LineComment::None,
                    })
                }
            }
            cfg.sections.push(section);
        }
        result = result.and(file.dump(&cfg));
        if let Ok(modified) = file.get_modified() {
            output.insert(name, modified);
        }
    }

    result?;
    Ok(output)
}

pub fn edit<C: CtrlContext>(
    ctx: C,
    args: GetArgs,
) -> Result<BTreeMap<String, DateTime<Utc>>, Error> {
    let current_files = get(ctx.clone(), args)?;
    let modified_files = crate::utils::edit_in_editor(&current_files)?;
    set(ctx, DeserializeStdin(modified_files))
}
