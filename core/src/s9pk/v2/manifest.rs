use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use clap::builder::ValueParserFactory;
use color_eyre::eyre::eyre;
use exver::{Version, VersionRange};
use imbl_value::{InOMap, InternedString};
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use url::Url;

pub use crate::PackageId;
use crate::dependencies::Dependencies;
use crate::prelude::*;
use crate::s9pk::git_hash::GitHash;
use crate::s9pk::merkle_archive::directory_contents::DirectoryContents;
use crate::s9pk::merkle_archive::expected::{Expected, Filter};
use crate::s9pk::v2::pack::ImageConfig;
use crate::util::lshw::{LshwDevice, LshwDisplay, LshwProcessor};
use crate::util::serde::Regex;
use crate::util::{FromStrParser, VersionString, mime};
use crate::version::{Current, VersionT};
use crate::{ImageId, VolumeId};

fn current_version() -> Version {
    Current::default().semver()
}

#[derive(Clone, Debug, Deserialize, Serialize, HasModel, TS)]
#[serde(rename_all = "camelCase")]
#[model = "Model<Self>"]
#[ts(export)]
pub struct Manifest {
    pub id: PackageId,
    #[ts(type = "string")]
    pub title: InternedString,
    pub version: VersionString,
    pub satisfies: BTreeSet<VersionString>,
    pub release_notes: LocaleString,
    #[ts(type = "string")]
    pub can_migrate_to: VersionRange,
    #[ts(type = "string")]
    pub can_migrate_from: VersionRange,
    #[ts(type = "string")]
    pub license: InternedString, // type of license
    #[ts(type = "string")]
    pub wrapper_repo: Url,
    #[ts(type = "string")]
    pub upstream_repo: Url,
    #[ts(type = "string")]
    pub support_site: Url,
    #[ts(type = "string")]
    pub marketing_site: Url,
    #[ts(type = "string | null")]
    pub donation_url: Option<Url>,
    #[ts(type = "string | null")]
    pub docs_url: Option<Url>,
    pub description: Description,
    pub images: BTreeMap<ImageId, ImageConfig>,
    pub volumes: BTreeSet<VolumeId>,
    #[serde(default)]
    pub alerts: Alerts,
    #[serde(default)]
    pub dependencies: Dependencies,
    #[serde(default)]
    pub hardware_requirements: HardwareRequirements,
    #[serde(default)]
    pub hardware_acceleration: bool,
    pub git_hash: Option<GitHash>,
    #[serde(default = "current_version")]
    #[ts(type = "string")]
    pub os_version: Version,
    #[ts(type = "string | null")]
    pub sdk_version: Option<Version>,
}
impl Manifest {
    pub fn validate_for<'a, T: Clone>(
        &self,
        arch: Option<&str>,
        archive: &'a DirectoryContents<T>,
    ) -> Result<Filter, Error> {
        let mut expected = Expected::new(archive);
        expected.check_file("manifest.json")?;
        expected.check_stem("icon", |ext| {
            ext.and_then(|e| e.to_str())
                .and_then(mime)
                .map_or(false, |mime| mime.starts_with("image/"))
        })?;
        expected.check_file("LICENSE.md")?;
        expected.check_file("javascript.squashfs")?;
        for (dependency, _) in &self.dependencies.0 {
            let dep_path = Path::new("dependencies").join(dependency);
            let _ = expected.check_file(dep_path.join("metadata.json"));
            let _ = expected.check_stem(dep_path.join("icon"), |ext| {
                ext.and_then(|e| e.to_str())
                    .and_then(mime)
                    .map_or(false, |mime| mime.starts_with("image/"))
            });
        }
        if let Err(e) = expected.check_file(Path::new("assets.squashfs")) {
            // backwards compatibility for alpha s9pks - remove eventually
            if expected.check_dir("assets").is_err() {
                tracing::warn!("{e}");
                tracing::debug!("{e:?}");
                // return Err(e);
            }
        }
        for (image_id, config) in &self.images {
            let mut check_arch = |arch: &str| {
                let mut arch = arch;
                if let Err(e) = expected.check_file(
                    Path::new("images")
                        .join(arch)
                        .join(image_id)
                        .with_extension("squashfs"),
                ) {
                    if let Some(emulate_as) = &config.emulate_missing_as {
                        expected.check_file(
                            Path::new("images")
                                .join(emulate_as)
                                .join(image_id)
                                .with_extension("squashfs"),
                        )?;
                        arch = &**emulate_as;
                    } else {
                        return Err(e);
                    }
                }
                expected.check_file(
                    Path::new("images")
                        .join(arch)
                        .join(image_id)
                        .with_extension("json"),
                )?;
                expected.check_file(
                    Path::new("images")
                        .join(arch)
                        .join(image_id)
                        .with_extension("env"),
                )?;
                Ok(())
            };
            if let Some(arch) = arch {
                check_arch(arch)?;
            } else if let Some(arches) = &self.hardware_requirements.arch {
                for arch in arches {
                    check_arch(arch)?;
                }
            } else if let Some(arch) = config.emulate_missing_as.as_deref() {
                if !config.arch.contains(arch) {
                    return Err(Error::new(
                        eyre!("`emulateMissingAs` must match an included `arch`"),
                        ErrorKind::ParseS9pk,
                    ));
                }
                for arch in &config.arch {
                    check_arch(&arch)?;
                }
            } else {
                return Err(Error::new(
                    eyre!(
                        "`emulateMissingAs` required for all images if no `arch` specified in `hardwareRequirements`"
                    ),
                    ErrorKind::ParseS9pk,
                ));
            }
        }
        Ok(expected.into_filter())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, TS, PartialEq)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HardwareRequirements {
    #[serde(default)]
    pub device: Vec<DeviceFilter>,
    #[ts(type = "number | null")]
    pub ram: Option<u64>,
    #[ts(type = "string[] | null")]
    pub arch: Option<BTreeSet<InternedString>>,
}
impl HardwareRequirements {
    /// returns a value that can be used as a sort key to get most specific requirements first
    pub fn specificity_desc(&self) -> (u32, u32, u64) {
        (
            u32::MAX - self.device.len() as u32, // more device requirements = more specific
            self.arch.as_ref().map_or(u32::MAX, |a| a.len() as u32), // more arches = less specific
            self.ram.map_or(0, |r| r),           // more ram = more specific
        )
    }
}

#[derive(Clone, Debug, PartialEq, TS)]
#[ts(type = "string | Record<string, string>")]
pub enum LocaleString {
    Translated(String),
    LanguageMap(InOMap<InternedString, String>),
}
impl std::str::FromStr for LocaleString {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Try JSON parse first (for maps or quoted strings)
        if let Ok(parsed) = serde_json::from_str::<LocaleString>(s) {
            return Ok(parsed);
        }
        // Fall back to plain string
        Ok(LocaleString::Translated(s.to_owned()))
    }
}
impl LocaleString {
    pub fn localize_for(&mut self, locale: &str) {
        if let Self::LanguageMap(map) = self {
            if let Some(translated) = map.remove(locale) {
                *self = Self::Translated(translated);
                return;
            }
            let prefix = locale.split_inclusive("_").next().unwrap();
            let mut first = None;
            for (lang, translated) in std::mem::take(map) {
                if lang.starts_with(prefix) {
                    *self = Self::Translated(translated);
                    return;
                }
                if first.is_none() {
                    first = Some(translated);
                }
            }
            *self = Self::Translated(first.unwrap_or_default())
        }
    }
    pub fn localized_for(mut self, locale: &str) -> String {
        self.localize_for(locale);
        if let Self::Translated(s) = self {
            s
        } else {
            unreachable!()
        }
    }
    pub fn localize(&mut self) {
        self.localize_for(&*rust_i18n::locale());
    }
    pub fn localized(self) -> String {
        self.localized_for(&*rust_i18n::locale())
    }
}
impl<'de> Deserialize<'de> for LocaleString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct LocaleStringVisitor;

        impl<'de> serde::de::Visitor<'de> for LocaleStringVisitor {
            type Value = LocaleString;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string or a map of language codes to strings")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(LocaleString::Translated(value.to_owned()))
            }

            fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(LocaleString::Translated(value))
            }

            fn visit_map<M>(self, map: M) -> Result<Self::Value, M::Error>
            where
                M: serde::de::MapAccess<'de>,
            {
                let language_map =
                    InOMap::deserialize(serde::de::value::MapAccessDeserializer::new(map))?;
                Ok(LocaleString::LanguageMap(language_map))
            }
        }

        deserializer.deserialize_any(LocaleStringVisitor)
    }
}
impl Serialize for LocaleString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            LocaleString::Translated(s) => serializer.serialize_str(s),
            LocaleString::LanguageMap(map) => map.serialize(serializer),
        }
    }
}
impl ValueParserFactory for LocaleString {
    type Parser = FromStrParser<Self>;
    fn value_parser() -> Self::Parser {
        FromStrParser::new()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DeviceFilter {
    pub description: String,
    #[ts(type = "\"processor\" | \"display\"")]
    pub class: InternedString,
    #[ts(type = "string | null")]
    pub product: Option<Regex>,
    #[ts(type = "string | null")]
    pub vendor: Option<Regex>,
    #[ts(optional)]
    pub capabilities: Option<BTreeSet<InternedString>>,
    #[ts(optional)]
    pub driver: Option<InternedString>,
}
// Omit description
impl PartialEq for DeviceFilter {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class
            && self.product == other.product
            && self.vendor == other.vendor
            && self.capabilities == other.capabilities
            && self.driver == other.driver
    }
}
impl DeviceFilter {
    pub fn matches(&self, device: &LshwDevice) -> bool {
        if &*self.class != device.class() {
            return false;
        }
        match device {
            LshwDevice::Processor(LshwProcessor {
                product,
                vendor,
                capabilities,
            }) => {
                if let Some(match_product) = &self.product {
                    if !product
                        .as_deref()
                        .map_or(false, |p| match_product.as_ref().is_match(p))
                    {
                        return false;
                    }
                }
                if let Some(match_vendor) = &self.vendor {
                    if !vendor
                        .as_deref()
                        .map_or(false, |v| match_vendor.as_ref().is_match(v))
                    {
                        return false;
                    }
                }
                if !self
                    .capabilities
                    .as_ref()
                    .map_or(true, |c| c.is_subset(capabilities))
                {
                    return false;
                }
                true
            }
            LshwDevice::Display(LshwDisplay {
                product,
                vendor,
                capabilities,
                driver,
            }) => {
                if let Some(match_product) = &self.product {
                    if !product
                        .as_deref()
                        .map_or(false, |p| match_product.as_ref().is_match(p))
                    {
                        return false;
                    }
                }
                if let Some(match_vendor) = &self.vendor {
                    if !vendor
                        .as_deref()
                        .map_or(false, |v| match_vendor.as_ref().is_match(v))
                    {
                        return false;
                    }
                }
                if !self
                    .capabilities
                    .as_ref()
                    .map_or(true, |c| c.is_subset(capabilities))
                {
                    return false;
                }
                if !self
                    .driver
                    .as_ref()
                    .map_or(true, |d| Some(d) == driver.as_ref())
                {
                    return false;
                }
                true
            }
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, TS, PartialEq)]
#[ts(export)]
pub struct Description {
    pub short: LocaleString,
    pub long: LocaleString,
}
impl Description {
    pub fn localize_for(&mut self, locale: &str) {
        self.short.localize_for(locale);
        self.long.localize_for(locale);
    }

    pub fn validate(&self) -> Result<(), Error> {
        if match &self.short {
            LocaleString::Translated(s) => s.len() > 160,
            LocaleString::LanguageMap(map) => map.values().any(|s| s.len() > 160),
        } {
            return Err(Error::new(
                eyre!("Short description must be 160 characters or less."),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        if match &self.short {
            LocaleString::Translated(s) => s.len() > 5000,
            LocaleString::LanguageMap(map) => map.values().any(|s| s.len() > 5000),
        } {
            return Err(Error::new(
                eyre!("Long description must be 5000 characters or less."),
                crate::ErrorKind::ValidateS9pk,
            ));
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize, TS, PartialEq)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct Alerts {
    pub install: Option<LocaleString>,
    pub uninstall: Option<LocaleString>,
    pub restore: Option<LocaleString>,
    pub start: Option<LocaleString>,
    pub stop: Option<LocaleString>,
}
impl Alerts {
    pub fn localize_for(&mut self, locale: &str) {
        self.install.as_mut().map(|s| s.localize_for(locale));
        self.uninstall.as_mut().map(|s| s.localize_for(locale));
        self.restore.as_mut().map(|s| s.localize_for(locale));
        self.start.as_mut().map(|s| s.localize_for(locale));
        self.stop.as_mut().map(|s| s.localize_for(locale));
    }
}
