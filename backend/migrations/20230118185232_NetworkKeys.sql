-- Add migration script here
ALTER TABLE
    account
ADD
    COLUMN root_ca_key_pem TEXT,
ADD
    COLUMN root_ca_cert_pem TEXT,
ADD
    COLUMN int_ca_key_pem TEXT,
ADD
    COLUMN int_ca_cert_pem TEXT;

UPDATE
    account
SET
    root_ca_key_pem = (
        SELECT
            priv_key_pem
        FROM
            certificates
        WHERE
            id = 0
    ),
    root_ca_cert_pem = (
        SELECT
            certificate_pem
        FROM
            certificates
        WHERE
            id = 0
    ),
    int_ca_key_pem = (
        SELECT
            priv_key_pem
        FROM
            certificates
        WHERE
            id = 1
    ),
    int_ca_cert_pem = (
        SELECT
            certificate_pem
        FROM
            certificates
        WHERE
            id = 1
    )
WHERE
    id = 0;

ALTER TABLE
    account
ALTER COLUMN
    root_ca_key_pem
SET
    NOT NULL,
ALTER COLUMN
    root_ca_cert_pem
SET
    NOT NULL,
ALTER COLUMN
    int_ca_key_pem
SET
    NOT NULL,
ALTER COLUMN
    int_ca_cert_pem
SET
    NOT NULL;

CREATE TABLE IF NOT EXISTS network_keys (
    package TEXT NOT NULL,
    interface TEXT NOT NULL,
    key BYTEA NOT NULL CHECK (length(key) = 32),
    cert_pem TEXT NOT NULL,
    PRIMARY KEY (package, interface)
);