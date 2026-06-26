# Migrating LND to StartOS

How to transfer your LND node — including on-chain funds and open Lightning channels — from another platform to StartOS without closing channels.

> [!WARNING]
>
> After migrating your LND wallet to StartOS, **never restart your old node**. Turning on your old node can broadcast old channel states and result in loss of funds.

## Supported Source Platforms

StartOS's LND service can pull wallet and channel data directly from the following platforms over your local network:

- **Umbrel** (1.x and 0.5.x)
- **RaspiBlitz**
- **myNode**

If your source platform is not listed, you may still be able to migrate by manually copying the LND data directory. See [Manual Migration](#manual-migration) below.

## Prerequisites

- Both devices (source node and StartOS server) must be on the **same local network**.
- Your source node must be **running and accessible** at the time of migration.
- You need your source node's **local IP address** (check your router's admin page if unsure).
- You need your source node's **password(s)** for SSH or API access, depending on the platform.

## Migration Steps

### 1. Install LND on StartOS

Install LND from the StartOS Marketplace, but **do not start it**. Starting the service creates a new wallet, which prevents the migration action from running. If you have already started LND, uninstall it and install a fresh copy.

### 2. Run the Migration Action

Open LND on your StartOS server and go to **Actions**. Select the migration action that matches your source platform:

- **Migrate from Umbrel 1.x**
- **Migrate from Umbrel 0.5**
- **Migrate from RaspiBlitz**
- **Migrate from myNode**

Enter your source node's local IP address and any required passwords when prompted.

### 3. Wait for the Migration to Complete

The migration will copy your LND wallet, channel database, and configuration from the source node. This may take several minutes depending on the size of your channel database and network speed.

### 4. Disconnect the Old Node

Once the migration is complete, **shut down and disconnect your old node** before proceeding. This is critical — running two nodes with the same channel state will result in force-closures and potential loss of funds.

### 5. Start LND on StartOS

With the old node safely shut down, start LND on your StartOS server. It will begin syncing and reconnecting to your peers with the migrated channel state.

> [!WARNING]
>
> Never restart your old node after the migration has completed. If you need to go back to your old node for any reason, do **not** start LND on StartOS first.

## Manual Migration

If your source platform is not listed above, you can migrate by manually copying the LND data directory. The key files are:

- `wallet.db` — Your on-chain wallet
- `channel.backup` — Static channel backups (SCB)
- `data/graph/mainnet/channel.db` — Channel state database

> [!NOTE]
>
> Manual migration carries more risk than the built-in actions. If you are unsure about the process, consider closing your channels on the old node first, then restoring from seed on StartOS. This is safer but requires re-opening channels.

## Troubleshooting

**"Import action not available"** — LND has already been started and a wallet exists. Uninstall LND and install a fresh copy from the StartOS Marketplace.

**Migration times out or fails** — Ensure both devices are on the same local network and that the source node is running. Double-check the IP address and passwords.

**Channels force-close after migration** — This usually means the old node was restarted after migration, or the channel database was corrupted during transfer. Unfortunately, force-closed channels cannot be recovered — the funds will be returned to your on-chain wallet after the timelock expires.
