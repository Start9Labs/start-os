-- Add migration script here
CREATE TABLE IF NOT EXISTS tor
(
    package     TEXT NOT NULL,
    interface   TEXT  NOT NULL,
    key         BLOB  NOT NULL,
    PRIMARY KEY (package, interface)
);