use std::time::Instant;

use base64::Engine;
use chrono::Utc;
use ed25519::pkcs8::EncodePublicKey;
use ed25519_dalek::{Signer, SigningKey};
use reqwest::Client;
use tokio::sync::mpsc;
use tracing::{error, instrument};

use super::{DeliveryAttempt, RegistryEvent, WebhookEventRecord, WebhookSubscriber};
use crate::registry::context::RegistryContext;
use crate::rpc_continuations::Guid;

pub const PUBKEY_HEADER: &str = "x-startos-registry-pubkey";
pub const SIGNATURE_HEADER: &str = "x-startos-registry-signature";
pub const TOPIC_HEADER: &str = "x-startos-registry-topic";
pub const EVENT_ID_HEADER: &str = "x-startos-registry-event-id";

#[instrument(skip_all)]
pub async fn dispatcher_loop(
    ctx: RegistryContext,
    subscriber: Option<WebhookSubscriber>,
    signing_key: SigningKey,
    mut rx: mpsc::UnboundedReceiver<RegistryEvent>,
) {
    let Some(sub) = subscriber else {
        while rx.recv().await.is_some() {}
        return;
    };
    while let Some(event) = rx.recv().await {
        let id = event.id.clone();
        let record = WebhookEventRecord {
            event: event.clone(),
            attempts: vec![],
        };
        if let Err(e) = ctx
            .db
            .mutate(|db| {
                db.as_webhook_log_mut()
                    .as_events_mut()
                    .insert(&id, &record)
            })
            .await
            .result
        {
            error!("failed to persist webhook event {}: {e}", id.as_ref());
            continue;
        }

        let attempt = deliver(&ctx.client, &sub, &signing_key, &event, false).await;
        append_attempt(&ctx, &id, attempt).await;
    }
}

pub async fn append_attempt(ctx: &RegistryContext, event_id: &Guid, attempt: DeliveryAttempt) {
    if let Err(e) = ctx
        .db
        .mutate(|db| {
            if let Some(rec) = db
                .as_webhook_log_mut()
                .as_events_mut()
                .as_idx_mut(event_id)
            {
                rec.as_attempts_mut().mutate(|a| {
                    a.push(attempt);
                    Ok(())
                })?;
            }
            Ok(())
        })
        .await
        .result
    {
        error!(
            "failed to record delivery attempt for {}: {e}",
            event_id.as_ref()
        );
    }
}

pub async fn deliver(
    client: &Client,
    sub: &WebhookSubscriber,
    signing_key: &SigningKey,
    event: &RegistryEvent,
    replay: bool,
) -> DeliveryAttempt {
    let started = Instant::now();
    let elapsed_ms = |s: Instant| s.elapsed().as_millis() as u64;
    let fail = |err: String| DeliveryAttempt {
        at: Utc::now(),
        status_code: None,
        error: Some(err),
        duration_ms: elapsed_ms(started),
        replay,
    };

    let body = match serde_json::to_vec(event) {
        Ok(b) => b,
        Err(e) => return fail(format!("serialize event: {e}")),
    };
    let b64 = base64::engine::general_purpose::STANDARD;
    let pubkey_der = match signing_key.verifying_key().to_public_key_der() {
        Ok(d) => d,
        Err(e) => return fail(format!("encode pubkey der: {e}")),
    };
    let signature = signing_key.sign(&body);
    let pubkey_b64 = b64.encode(pubkey_der.as_bytes());
    let sig_b64 = b64.encode(signature.to_bytes());

    match client
        .post(sub.url.clone())
        .header("content-type", "application/json")
        .header(PUBKEY_HEADER, pubkey_b64)
        .header(SIGNATURE_HEADER, sig_b64)
        .header(TOPIC_HEADER, &event.topic)
        .header(EVENT_ID_HEADER, event.id.as_ref())
        .body(body)
        .send()
        .await
    {
        Ok(r) => {
            let status = r.status();
            DeliveryAttempt {
                at: Utc::now(),
                status_code: Some(status.as_u16()),
                error: if status.is_success() {
                    None
                } else {
                    Some(format!("status {status}"))
                },
                duration_ms: elapsed_ms(started),
                replay,
            }
        }
        Err(e) => fail(format!("send: {e}")),
    }
}
