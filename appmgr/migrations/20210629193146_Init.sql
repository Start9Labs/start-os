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
    created_at TIMESTAMP NOT NULL,
    expires_at TIMESTAMP NOT NULL,
    metadata   JSON
);