#[cfg(feature = "arti")]
mod arti;

#[cfg(not(feature = "arti"))]
mod ctor;

#[cfg(feature = "arti")]
pub use arti::{tor_api, OnionAddress, OnionStore, TorController, TorSecretKey};
#[cfg(not(feature = "arti"))]
pub use ctor::{tor_api, OnionAddress, OnionStore, TorController, TorSecretKey};
