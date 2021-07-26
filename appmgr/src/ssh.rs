use rpc_toolkit::command;

use crate::util::display_none;
use crate::{context::EitherContext, Error};

#[derive(serde::Deserialize, serde::Serialize)]
pub struct PubKey(
    #[serde(serialize_with = "crate::util::serialize_display")]
    #[serde(deserialize_with = "crate::util::deserialize_from_str")]
    openssh_keys::PublicKey,
);
impl std::str::FromStr for PubKey {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(|pk| PubKey(pk)).map_err(|e| Error {
            source: e.into(),
            kind: crate::ErrorKind::ParseSshKey,
            revision: None,
        })
    }
}

#[command(subcommands(add, remove, list,))]
pub fn ssh(#[context] ctx: EitherContext) -> Result<EitherContext, Error> {
    Ok(ctx)
}

#[command(display(display_none))]
pub fn add(#[context] ctx: EitherContext, #[arg] key: PubKey) -> Result<(), Error> {
    let pool = &ctx.as_rpc().unwrap().secret_store;
    // check fingerprint for duplicates
    let fp = key.0.fingerprint_md5();
    // if no duplicates, insert into DB
    // insert into live key file
    // return fingerprint
    todo!()
}
#[command(display(display_none))]
pub fn remove(#[context] ctx: EitherContext, #[arg] fingerprint: String) -> Result<(), Error> {
    // check if fingerprint is in DB
    // if in DB, remove it from DB AND overlay key file
    // if not in DB, Err404
    todo!()
}
#[command(display(display_none))]
pub fn list(#[context] ctx: EitherContext) -> Result<Vec<String>, Error> {
    // list keys in DB and return them
    todo!()
}
