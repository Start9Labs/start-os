use std::path::Path;

use tokio::process::Command;
use ts_rs::TS;

use crate::prelude::*;
use crate::util::Invoke;

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, TS)]
#[ts(type = "string")]
pub struct GitHash(String);

impl GitHash {
    pub async fn from_path(path: impl AsRef<Path>) -> Result<GitHash, Error> {
        let mut hash = String::from_utf8(
            Command::new("git")
                .arg("rev-parse")
                .arg("HEAD")
                .current_dir(&path)
                .invoke(ErrorKind::Git)
                .await?,
        )?;
        if Command::new("git")
            .arg("diff-index")
            .arg("--quiet")
            .arg("HEAD")
            .arg("--")
            .invoke(ErrorKind::Git)
            .await
            .is_err()
        {
            hash += "-modified";
        }
        Ok(GitHash(hash))
    }
    pub fn load_sync() -> Option<GitHash> {
        let mut hash = String::from_utf8(
            std::process::Command::new("git")
                .arg("rev-parse")
                .arg("HEAD")
                .output()
                .ok()?
                .stdout,
        )
        .ok()?;
        if !std::process::Command::new("git")
            .arg("diff-index")
            .arg("--quiet")
            .arg("HEAD")
            .arg("--")
            .output()
            .ok()?
            .status
            .success()
        {
            hash += "-modified";
        }

        Some(GitHash(hash))
    }
}

impl AsRef<str> for GitHash {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// #[tokio::test]
// async fn test_githash_for_current() {
//     let answer: GitHash = GitHash::from_path(std::env::current_dir().unwrap())
//         .await
//         .unwrap();
//     let answer_str: &str = answer.as_ref();
//     assert!(
//         !answer_str.is_empty(),
//         "Should have a hash for this current working"
//     );
// }
