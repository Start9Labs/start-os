use yasi::InternedString;

#[derive(Debug, thiserror::Error)]
#[error("Invalid ID: {0}")]
pub struct InvalidId(pub(super) InternedString);
