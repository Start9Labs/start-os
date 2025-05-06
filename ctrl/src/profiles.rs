use crate::{
    utils::{DeserializeStdin, HandlerExtSerde},
    Error, ErrorKind,
};
use clap::Parser;
use color_eyre::eyre::eyre;
use rpc_toolkit::{from_fn, Context, HandlerExt as _, ParentHandler};
use serde::{Deserialize, Serialize};
use std::net::Ipv4Addr;
use uciedit::parse_config;

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LanAccess {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "SAME_PROFILE")]
    SameProfile,
    OtherProfiles(Vec<String>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum WanAccess {
    #[serde(rename = "ALL")]
    All,
    #[serde(rename = "NONE")]
    None,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Profile {
    name: String,
    interface: Option<String>,
    gateway_ip: Ipv4Addr,
    subnet_ip: Ipv4Addr,
    lan_access: LanAccess,
    wan_access: WanAccess,
    block_new_profiles: bool,
    vlan_tag: Option<u16>,
}

pub fn profiles<C: Context>() -> ParentHandler<C> {
    ParentHandler::new()
        .subcommand("get", from_fn(get::<C>).with_display_serializable())
        .subcommand("delete", from_fn(delete::<C>).no_display())
        .subcommand("list", from_fn(list::<C>).with_display_serializable())
        .subcommand("create", from_fn(create::<C>).no_display())
        .subcommand("update", from_fn(update::<C>).no_display())
}

#[derive(Debug, Deserialize, Serialize, Parser)]
pub struct ProfileId {
    vlanid: u64,
}

pub fn get<C: Context>(ctx: C, ProfileId { vlanid }: ProfileId) -> Result<Profile, Error> {
    todo!()
}

pub fn delete<C: Context>(ctx: C, ProfileId { vlanid }: ProfileId) -> Result<(), Error> {
    todo!()
}

pub fn list<C: Context>(ctx: C) -> Result<Vec<Profile>, Error> {
    todo!()
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ProfileValueArgs {
    profile: Profile,
}

pub fn create<C: Context>(
    ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile>,
) -> Result<ProfileId, Error> {
    let interface = match profile.interface {
        Some(i) => i,
        None => allocate_interface_name()?,
    };
    let vlan_tag = match profile.vlan_tag {
        Some(i) => i,
        None => allocate_vlan_tag()?,
    };
    todo!()
}

pub fn update<C: Context>(
    ctx: C,
    DeserializeStdin(profile): DeserializeStdin<Profile>,
) -> Result<ProfileId, Error> {
    let interface = match profile.interface {
        Some(i) => i,
        None => return Err(eyre!("provide interface when updating").into()),
    };
    let vlan_tag = match profile.vlan_tag {
        Some(i) => i,
        None => return Err(eyre!("provide vlan_tag when updating").into()),
    };
    todo!()
}

pub fn validate_interface(name: &str) -> Result<(), Error> {
    match name {
        "lan" | "wan" | "wan6" | "br-lan" => Err(ErrorKind::InterfaceNameConflict.into()),
        _ if !name.chars().any(|c| c.is_ascii_alphanumeric()) => {
            Err(ErrorKind::InterfaceNameInvalid.into())
        }
        _ if name.len() > 5 => Err(ErrorKind::InterfaceNameTooLong.into()),
        _ => Ok(()),
    }
}

pub fn allocate_interface_name() -> Result<String, Error> {
    todo!()
}

pub fn allocate_vlan_tag() -> Result<u16, Error> {
    todo!()
}
