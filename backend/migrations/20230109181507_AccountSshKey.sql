-- Add migration script here
CREATE EXTENSION pgcrypto;

ALTER TABLE
    account
ADD
    COLUMN ssh_key BYTEA CHECK (length(ssh_key) = 32);

UPDATE
    account
SET
    ssh_key = gen_random_bytes(32)
WHERE
    id = 0;

ALTER TABLE
    account
ALTER COLUMN
    ssh_key
SET
    NOT NULL;