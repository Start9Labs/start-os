-- Add migration script here
CREATE TABLE IF NOT EXISTS tor
(
    package     TEXT NOT NULL,
    interface   TEXT NOT NULL,
    key         BLOB NOT NULL CHECK (length(key) = 64),
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
    tor_key BLOB NOT NULL CHECK (length(tor_key) = 64)
);
CREATE TABLE IF NOT EXISTS ssh_keys
(
    fingerprint     TEXT NOT NULL,
    openssh_pubkey  TEXT NOT NULL,
    created_at      TEXT NOT NULL,
    PRIMARY KEY (fingerprint)
);
CREATE TABLE IF NOT EXISTS certificates
(
    id INTEGER PRIMARY KEY, -- Root = 0, Int = 1, Other = 2..
    priv_key_pem TEXT NOT NULL,
    certificate_pem TEXT NOT NULL,
    lookup_string TEXT UNIQUE,
    created_at TEXT,
    updated_at TEXT
);
CREATE TABLE IF NOT EXISTS notifications
(
    id INTEGER PRIMARY KEY,
    package_id TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    code INTEGER NOT NULL,
    level TEXT NOT NULL,
    title TEXT NOT NULL,
    message TEXT NOT NULL,
    data TEXT
)