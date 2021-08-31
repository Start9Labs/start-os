-- Add migration script here
CREATE TABLE IF NOT EXISTS tor
(
    package     TEXT NOT NULL,
    interface   TEXT NOT NULL,
    key         BLOB NOT NULL,
    PRIMARY KEY (package, interface)
);
CREATE TABLE IF NOT EXISTS session
(
    id         TEXT NOT NULL PRIMARY KEY,
    logged_in TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    logged_out TIMESTAMP,
    last_active TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    user_agent TEXT,
    metadata   TEXT NOT NULL DEFAULT 'null'
);
CREATE TABLE IF NOT EXISTS account
(
    id INTEGER PRIMARY KEY CHECK (id = 0),
    password TEXT NOT NULL,
    tor_key BLOB NOT NULL
);
CREATE TABLE IF NOT EXISTS ssh_keys
(
    fingerprint     TEXT NOT NULL,
    openssh_pubkey  TEXT NOT NULL,
    created_at      TEXT NOT NULL,
    PRIMARY KEY (fingerprint)
);