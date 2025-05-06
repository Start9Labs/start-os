use crate::state::{Config, Connection, ConnectionId, LanAccess, SecProfile, State, WatchState};
use color_eyre::eyre::Error;
use macaddr::MacAddr;
use std::{fmt::Write, future::Future, net::IpAddr};
use tokio::{process::Command, task::JoinSet};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum Zone {
    Lan,
    Wan,
}

impl Zone {
    pub fn iptables_zone(self, postfix: &str) -> String {
        format!(
            "zone_{}_{}",
            match self {
                Zone::Lan => "lan",
                Zone::Wan => "wan",
            },
            postfix
        )
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct AllowRule {
    pub src_zone: Zone,
    pub src_ip: Option<IpAddr>,
    pub src_mac: Option<MacAddr>,
    pub dest_zone: Zone,
    pub dest_ip: Option<IpAddr>,
}

pub fn generate_profile2profile_allows(
    state: &State,
    src_ip: IpAddr,
    src_mac: MacAddr,
    dst_profile: &str,
    allows: &mut Vec<AllowRule>,
) {
    for (id, Connection { profile, ips, .. }) in &state.connections {
        if id.mac == src_mac {
            // don't explicitly need to allow lan connections from a device to itself
            // TODO: is this true?
            continue;
        }

        if profile.as_deref() != Some(dst_profile) {
            continue;
        }

        for &dest_ip in ips {
            match (src_ip, dest_ip) {
                (IpAddr::V4(_), IpAddr::V4(_)) => (),
                // TODO: allow lan connections between v6 addrs?
                (IpAddr::V6(_), IpAddr::V6(_)) => continue,
                // v6<->v4 doesn't make any sense
                _ => continue,
            }

            allows.push(AllowRule {
                src_zone: Zone::Lan,
                src_ip: Some(src_ip),
                src_mac: Some(src_mac),
                dest_zone: Zone::Lan,
                dest_ip: Some(dest_ip),
            })
        }
    }
}

pub fn generate_allows(state: &State, allows: &mut Vec<AllowRule>) {
    for (&ConnectionId { mac, .. }, Connection { profile, ips, .. }) in state.connections.iter() {
        // TODO: use a zone for interface profiles, instead of doing the ip<->ip thing

        let Some(profile) = profile else { continue };

        let Some(SecProfile { lan, wan }) = state.config.profiles.get(profile) else {
            continue;
        };

        for &ip in ips {
            match wan {
                false => (),
                true => allows.push(AllowRule {
                    src_zone: Zone::Lan,
                    src_ip: Some(ip),
                    src_mac: Some(mac),
                    dest_zone: Zone::Wan,
                    dest_ip: None,
                }),
            }

            match lan {
                LanAccess::AllDevices => allows.push(AllowRule {
                    src_zone: Zone::Lan,
                    src_ip: Some(ip),
                    src_mac: Some(mac),
                    dest_zone: Zone::Lan,
                    dest_ip: None,
                }),
                LanAccess::NoDevices => (),
                LanAccess::OtherProfile(dst_profiles) => {
                    for dst_profile in dst_profiles {
                        generate_profile2profile_allows(state, ip, mac, dst_profile, allows);
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum RuleChange {
    Add(AllowRule),
    Delete(AllowRule),
}

impl RuleChange {
    pub fn iptables(&self) -> Command {
        // should be compatable with /etc/cfg/firewall
        // https://openwrt.org/docs/guide-user/firewall/netfilter_iptables/netfilter_openwrt#fw3_and_netfilter_detailed_example

        let mut c = Command::new("iptables");
        c.arg("-t");
        c.arg("filter");
        let AllowRule {
            src_zone,
            src_ip,
            src_mac,
            dest_zone,
            dest_ip,
        } = match self {
            RuleChange::Add(rule) => {
                c.arg("-A");
                rule
            }
            RuleChange::Delete(rule) => {
                c.arg("-D");
                rule
            }
        };
        c.arg(src_zone.iptables_zone("forward"));
        if let Some(src_mac) = src_mac {
            //c.arg("--mac-source");
            //c.arg(src_mac.to_string());
        }
        if let Some(src_ip) = src_ip {
            c.arg("-s");
            c.arg(src_ip.to_string());
        }
        if let Some(dest_ip) = dest_ip {
            c.arg("-d");
            c.arg(dest_ip.to_string());
        }
        c.arg("-j");
        c.arg(dest_zone.iptables_zone("dest_ACCEPT"));
        c
    }
}

fn rule_changes(a: &[AllowRule], b: &[AllowRule], mut with: impl FnMut(RuleChange)) {
    use RuleChange::*;
    let mut a_idx = 0;
    let mut b_idx = 0;
    loop {
        match (a.get(a_idx), b.get(b_idx)) {
            (None, None) => break,
            (Some(a), None) => {
                with(Delete(a.clone()));
                a_idx += 1;
            }
            (None, Some(b)) => {
                with(Add(b.clone()));
                b_idx += 1;
            }
            (Some(a), Some(b)) if a < b => {
                with(Delete(a.clone()));
                a_idx += 1;
            }
            (Some(a), Some(b)) if a > b => {
                with(Add(b.clone()));
                b_idx += 1;
            }
            (Some(_), Some(_)) => {
                a_idx += 1;
                b_idx += 1;
            }
        }
    }
}

pub async fn produce_rule_changes<F, O>(mut state: WatchState, mut with: F) -> Result<(), Error>
where
    F: FnMut(RuleChange) -> O,
    O: Future<Output = Result<(), Error>> + Sync + Send + 'static,
{
    let mut current_rules = Vec::new();
    let mut join_set = JoinSet::new();

    loop {
        let mut new_rules = Vec::new();
        state.peek_and_mark_seen(|state| generate_allows(state, &mut new_rules));
        new_rules.sort_unstable();
        rule_changes(&current_rules, &new_rules, |change| {
            join_set.spawn(with(change));
        });
        current_rules = new_rules;

        while let Some(e) = join_set.join_next().await {
            e??;
        }
        state.changed().await;
    }
}

pub async fn maintain_iptables(state: WatchState) -> Result<(), Error> {
    produce_rule_changes(state, |change| async move {
        let mut command = change.iptables();
        dbg!(&command);
        let _status = command.spawn()?.wait().await?;
        Ok(())
    })
    .await
}

pub async fn write_basic_firewall_config(_cfg: &Config) -> Result<(), Error> {
    use uciedit::openwrt::FirewallRule;
    use uciedit::openwrt::FirewallTarget::{ACCEPT, REJECT};
    use uciedit::rewrite_config;

    const LAN_RULE_NAME: &str = "reject lan->lan unless accepted by start-wrt secprofs";
    const WAN_RULE_NAME: &str = "reject lan->wan unless accepted by start-wrt secprofs";
    const LOCALHOST_LAN_RULE_NAME: &str = "accept lan->localhost to allow admin access";
    const LOCALHOST_WAN_RULE_NAME: &str = "accept localhost->wan to allow admin access";

    rewrite_config("/etc/config/firewall", |mut ctx| {
        let mut found_lan_rule = false;
        let mut found_wan_rule = false;
        let mut found_localhost_lan_rule = false;
        let mut found_localhost_wan_rule = false;
        while ctx.step() {
            if ctx.ty() != "rule" {
                continue;
            }
            let Ok(mut rule) = ctx.get::<FirewallRule>() else {
                continue;
            };
            if rule.name == LAN_RULE_NAME {
                found_lan_rule = true;
                rule.target = REJECT;
                ctx.set(rule)?;
            } else if rule.name == WAN_RULE_NAME {
                found_wan_rule = true;
                rule.target = REJECT;
                ctx.set(rule)?;
            } else if rule.name == LOCALHOST_LAN_RULE_NAME {
                found_localhost_lan_rule = true;
                rule.target = ACCEPT;
                ctx.set(rule)?;
            } else if rule.name == LOCALHOST_WAN_RULE_NAME {
                found_localhost_wan_rule = true;
                rule.target = ACCEPT;
                ctx.set(rule)?;
            }
        }
        if !found_lan_rule {
            ctx.push(
                FirewallRule {
                    name: LAN_RULE_NAME.into(),
                    src: "lan".into(),
                    dest: "lan".into(),
                    target: REJECT,
                    ..Default::default()
                },
                None,
            )?;
        }
        if !found_wan_rule {
            ctx.push(
                FirewallRule {
                    name: WAN_RULE_NAME.into(),
                    src: "lan".into(),
                    dest: "wan".into(),
                    target: REJECT,
                    ..Default::default()
                },
                None,
            )?;
        }
        if !found_localhost_lan_rule {
            ctx.push(
                FirewallRule {
                    name: LOCALHOST_LAN_RULE_NAME.into(),
                    src: "lan".into(),
                    dest: "lan".into(),
                    dest_ip: Some("192.168.1.1".into()),
                    target: ACCEPT,
                    ..Default::default()
                },
                None,
            )?;
        }
        if !found_localhost_wan_rule {
            // TODO: what should this be?
        }
        Ok::<_, Error>(())
    })?;

    Command::new("/etc/init.d/firewall")
        .arg("reload")
        .spawn()?
        .wait()
        .await?;

    Ok(())
}
