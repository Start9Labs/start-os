use axum::Router;

use crate::context::RegistryContext;

pub fn router(ctx: &RegistryContext) -> Router {
    Router::new()
}
