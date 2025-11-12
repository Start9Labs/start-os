use crate::utils::DeserializeStdin;
use crate::{utils::HandlerExtSerde, Error, ErrorKind};
use chrono::{offset::Utc, DateTime};
use clap::Parser;
use rpc_toolkit::{from_fn, Context, ParentHandler};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use uciedit::{parse_config, Line};

pub fn uci<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("set", from_fn(set::<C>).with_display_serializable())
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Section {
    pub ty: String,
    pub name: Option<String>,
    pub options: HashMap<String, String>,
    pub lists: HashMap<String, Vec<String>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct File {
    pub sections: Vec<Section>,
    pub modified: Option<DateTime<Utc>>,
}

#[derive(Parser, Serialize, Deserialize)]
pub struct FilesGet {
    name: String,
}

pub fn get<C: Context>(_ctx: C, FilesGet { name }: FilesGet) -> Result<File, Error> {
    let path = format!("./etc/config/{name}");
    let mut sections = Vec::new();
    let modified = std::fs::metadata(&path)
        .and_then(|m| m.modified())
        .map(|d| d.into())
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
    Ok(File { sections, modified })
}

type Files = HashMap<String, File>;

pub fn set<C: Context>(
    _ctx: C,
    DeserializeStdin(files): DeserializeStdin<Files>,
) -> Result<(), Error> {
    todo!()
}
