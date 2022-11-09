use std::path::Path;

use crate::Error;

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct GitHash(String);

impl GitHash {
    pub async fn from_path(path: impl AsRef<Path>) -> Result<GitHash, Error> {
        let hash = tokio::process::Command::new("git")
            .args(["describe", "--always", "--abbrev=40", "--dirty=-modified"])
            .current_dir(path)
            .output()
            .await?;
        if !hash.status.success() {
            return Err(Error::new(
                color_eyre::eyre::eyre!("Could not get hash: {}", String::from_utf8(hash.stderr)?),
                crate::ErrorKind::Filesystem,
            ));
        }
        Ok(GitHash(String::from_utf8(hash.stdout)?))
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
