# StartWRT Init & Reflash Proposal

## Table of Contents

- [Context](#context)
- [Part 1: Manufacturing Init Tool](#part-1-manufacturing-init-tool)
  - [Architecture](#architecture)
  - [Init Tool Flow](#init-tool-flow)
  - [Password Character Set](#password-character-set)
- [Part 2: Reflash / Reset Flow](#part-2-reflash--reset-flow)
  - [Trigger Mechanism](#trigger-mechanism)
  - [Setup Wizard Detection Logic](#setup-wizard-detection-logic)
  - [Path A: Update](#path-a-update-keep-config-reuse-emmc-hash)
  - [Path B: Fresh Start](#path-b-fresh-start-full-reset-new-password)
  - [Web UI Architecture](#web-ui-architecture)
  - [Boot Detection: Setup Mode vs Normal Mode](#boot-detection-setup-mode-vs-normal-mode)
- [Implementation Details](#implementation-details)
  - [Files to Create / Modify](#files-to-create--modify)
  - [Reusable Code](#reusable-code)
  - [New Dependencies](#new-dependencies)
- [Verification](#verification)

---

## Context

StartWRT routers ship with a unique **12-character password** printed on a sticker on the bottom of the device. This single password is used for both:

- **Admin login** — web UI authentication
- **WiFi access** — WPA2-PSK

The WiFi SSID is **static** across all devices (e.g., `StartWRT`). A 12-character password from the 68-char unambiguous charset provides **~73 bits of entropy**, making rainbow table attacks infeasible even with a shared SSID (~207 million GPU-years computation, ~31 exabytes storage).

### Password lifecycle

| Stage | What happens |
|-------|-------------|
| **Manufacturing** | Factory worker enters sticker password via serial console. Two one-way hashes are stored on eMMC. |
| **End user setup** | User connects to WiFi (`StartWRT`) and web UI (`router.local`) using the sticker password. |
| **Factory reset** | Overlay is wiped, but eMMC hashes survive. Boot script auto-restores admin + WiFi credentials. |
| **Reflash / recovery** | User boots from microSD. Can either keep the eMMC password or set a new one. |

### What's stored on eMMC

Two one-way hashes — **no plaintext** is ever stored:

| File | Contents | Purpose |
|------|----------|---------|
| `/mnt/persistent/password_hash` | `$6$...` SHA-512 crypt hash | Admin authentication (`/etc/shadow`) |
| `/mnt/persistent/wifi_pmk` | 64 hex chars (PBKDF2 output) | WPA2-PSK key (`/etc/config/wireless`) |

---

## Part 1: Manufacturing Init Tool

### Architecture

**`startwrt-cli init`** — a new local-only subcommand of the existing CLI binary.

| Design decision | Rationale |
|----------------|-----------|
| Subcommand of `startwrt-cli` (not a separate binary) | Zero additional deployment overhead. `CliContext::init()` builds an HTTP client but doesn't connect — the `init` handler simply skips RPC calls. |
| Rust (not a shell script) | SHA-512 crypt hashing requires `pwhash` (already a dependency). Shell would need `openssl passwd -6` which isn't guaranteed on minimal OpenWrt. |
| agetty wrapper script | agetty's `-l` flag expects a single binary. A one-line wrapper bridges to `startwrt-cli init`. |

#### agetty integration

Wrapper script at `/usr/sbin/startwrt-init-wrapper`:

```sh
#!/bin/sh
exec startwrt-cli init
```

OpenWrt `/etc/inittab` entry:

```
ttyS0::respawn:/sbin/agetty -L -l /usr/sbin/startwrt-init-wrapper ttyS0 115200 vt100
```

#### eMMC persistent partition

A small dedicated eMMC partition mounted at `/mnt/persistent`. The partition and mount point are device-specific and configured per hardware target.

- Permissions: `0600`, owned by root
- Written atomically (temp file + rename + fsync)

### Init Tool Flow

```
 1. Check eMMC partition mounted at /mnt/persistent
    └─ If not mounted → attempt mount → if fails, print error and exit

 2. Check for existing /mnt/persistent/password_hash
    └─ If exists → print "Device already initialized." → exit 0

 3. Display banner: "StartWRT Device Initialization"

 4. Prompt: "Enter device password: " (no echo)

 5. Validate password:
    ├─ Exactly 12 characters
    ├─ Only unambiguous characters (see charset below)
    └─ If invalid → print error → reprompt (step 4)

 6. Prompt: "Confirm device password: " (no echo)

 7. Compare entries
    └─ If mismatch → print "Passwords do not match." → reprompt (step 4)

 8. Compute hashes:
    ├─ SHA-512 crypt (pwhash::sha512_crypt::hash) → admin auth
    └─ WPA-PSK PMK via PBKDF2(password, "StartWRT", 4096) → WiFi

 9. Write to eMMC (atomic):
    ├─ /mnt/persistent/password_hash
    └─ /mnt/persistent/wifi_pmk

10. Write admin hash → /etc/shadow (root user)

11. Write WiFi PMK → /etc/config/wireless (hex PSK key)

12. Enable WiFi: SSID "StartWRT", WPA2-PSK

13. Print "Device initialized successfully." → exit 0
```

### Password Character Set

| Category | Characters | Count |
|----------|-----------|-------|
| Uppercase | `A B C D E F G H J K L M N P Q R S T U V W X Y Z` | 24 (no `I`, `O`) |
| Lowercase | `a b c d e f g h i j k m n p q r s t u v w x y z` | 24 (no `l`, `o`) |
| Digits | `2 3 4 5 6 7 8 9` | 8 (no `0`, `1`) |
| Special | `! @ # $ % ^ & * - _ + =` | 12 |
| **Total** | | **68** |

> **Entropy**: 12 chars from 68-char set = log2(68^12) ≈ **73 bits**
>
> **Rainbow table cost**: ~207 million GPU-years computation, ~31 exabytes storage — **infeasible**

---

## Part 2: Reflash / Reset Flow

### Trigger Mechanism

Modeled after **start-os**: the reflash flow is triggered by **booting from a microSD card** containing a StartWRT image.

1. User flashes a StartWRT image to a microSD card
2. Inserts microSD into the router's slot
3. Bootloader (U-Boot) prioritizes microSD over onboard disk
4. Web-based setup wizard starts at **`router.local`** (mDNS)

### Setup Wizard Detection Logic

On boot from microSD, the wizard inspects the onboard disk and eMMC persistent partition:

| Onboard config? | eMMC hash? | Options presented |
|:---:|:---:|---|
| Yes | Yes | **Update** or **Fresh Start** |
| Yes | No | **Fresh Start** only |
| No | Yes | **Fresh Start** (eMMC hash offered as default) |
| No | No | **Fresh Start** only |

### Path A: Update (keep config, reuse eMMC hash)

Physical access (having the microSD) is sufficient authorization — **no password prompt required**.

```
1. Wizard detects onboard disk with existing StartWRT config
2. Wizard detects eMMC password hash
3. User selects "Update"
4. System flashes new firmware to onboard disk
5. Preserves config overlay (UCI files, SSH keys, etc.)
6. Restores eMMC password hash → /etc/shadow
7. Restores eMMC WiFi PMK → /etc/config/wireless
8. "Update complete. Remove microSD and reboot."
```

### Path B: Fresh Start (full reset, new password)

Recovery path for lost passwords. The user chooses a **new password of any length/format** — no sticker constraints.

```
 1. User selects "Fresh Start"
 2. Select Language
 3. Select Country
 4. Select Drive (if multiple targets available)
 5. Enter new password (user-chosen, any format/length)
 6. Confirm new password
 7. Compute hashes:
    ├─ SHA-512 crypt hash → admin auth
    └─ WPA-PSK PMK → WiFi
 8. Write new hashes to eMMC (replaces old values)
 9. Wipe onboard disk config overlay
10. Flash fresh firmware
11. Write hashes to /etc/shadow and /etc/config/wireless
12. "Setup complete. Remove microSD and reboot."
```

### Web UI Architecture

Modeled after start-os's setup wizard (`start-os/web/projects/setup-wizard/`):

| Component | Details |
|-----------|---------|
| **Frontend** | Angular app served from the microSD boot image |
| **Backend** | `startwrt-ctrld` in "setup mode" (like start-os's `startd` setup vs init detection) |
| **API endpoints** | Disk detection, firmware flashing, password setting, eMMC read/write |
| **Progress** | WebSocket or polling for flash progress updates |
| **Hostname** | `router.local` via mDNS (`umdns` or avahi on OpenWrt) |

### Boot Detection: Setup Mode vs Normal Mode

```
Boot from microSD detected?
├── Yes → Enter setup/reflash mode (serve wizard UI at router.local)
└── No  → Normal boot
          ├── /etc/shadow has root password? → Normal operation
          └── No password + eMMC hash exists? → Auto-restore → Normal operation
```

The **auto-restore** case handles factory reset: overlay is wiped (including `/etc/shadow` and `/etc/config/wireless`) but eMMC survives. A boot-time script restores both values silently:

- SHA-512 hash → `/etc/shadow` (admin login)
- WPA PMK → `/etc/config/wireless` (WiFi with SSID `StartWRT`)

---

## Implementation Details

### Files to Create / Modify

#### Init Tool (manufacturing)

| File | Action | Purpose |
|------|--------|---------|
| `ctrl/src/init.rs` | **Create** | Init subcommand: password prompting, validation, eMMC write |
| `ctrl/src/lib.rs` | **Modify** | Add `pub mod init;`, wire `init` into `main_api()` |
| `ctrl/src/auth.rs` | **Modify** | Make `update_shadow_hash()` public for reuse |
| `firstboot_config/inittab` | **Create** | OpenWrt inittab with agetty serial console config |

#### Reflash Flow

| File | Action | Purpose |
|------|--------|---------|
| `ctrl/src/setup.rs` | **Create** | Setup/reflash mode: disk detection, firmware flash, eMMC management |
| `ctrl/src/bin/startwrt-ctrld.rs` | **Modify** | Boot-time eMMC hash restore + setup mode detection |
| `web/` (setup wizard pages) | **Create** | Angular setup wizard UI: language, country, drive, password |

#### Shared

| File | Action | Purpose |
|------|--------|---------|
| `ctrl/src/emmc.rs` | **Create** | eMMC partition helpers (read/write hash, mount check) — used by both init tool and reflash |

### Reusable Code

| Source | What to reuse |
|--------|--------------|
| `ctrl/src/auth.rs:331` — `update_shadow_hash()` | Atomic `/etc/shadow` update. Make `pub`. |
| `ctrl/src/auth.rs:414` — `pwhash::sha512_crypt::hash()` | SHA-512 crypt hashing |
| `ctrl/src/auth.rs:432` — `rpassword::prompt_password()` | No-echo password prompting |
| `ctrl/src/auth.rs` — atomic write pattern | temp file → write → fsync → rename |
| `uciedit` (workspace crate) | UCI config parser for writing WiFi config |
| `ctrl/src/wifi.rs` | Reference for wireless UCI config structure |
| `start-os/web/projects/setup-wizard/` | Reference architecture for Angular wizard UI |

### New Dependencies

| Crate | Purpose |
|-------|---------|
| `pbkdf2` | PBKDF2 key derivation |
| `hmac` | HMAC for PBKDF2 |
| `sha1` | SHA-1 hash (WPA-PSK uses PBKDF2-SHA1) |

> These are needed to compute the WPA-PSK PMK: `PBKDF2-SHA1(password, SSID, 4096, 32)`. Alternatively, the existing `pwhash` crate or OpenSSL bindings may provide this.

---

## Verification

### Init Tool

| # | Test | What to verify |
|---|------|---------------|
| 1 | **Unit: charset validation** | Valid 12-char passwords pass; too short/long, ambiguous chars (`0`, `O`, `l`, `1`, `I`, `o`), disallowed specials are rejected |
| 2 | **Integration: eMMC write** | Set `STARTWRT_EMMC_PATH` to temp dir, pipe stdin, verify `password_hash` is valid `$6$...` and `wifi_pmk` is 64 hex chars |
| 3 | **Hash compatibility** | Stored admin hash works with `check_password()` in `auth.rs` (`pwhash::unix::verify`) |
| 4 | **WiFi PMK compatibility** | Stored PMK matches output of `wpa_passphrase "StartWRT" <password>` |
| 5 | **Manual serial test** | Full agetty → init tool flow on hardware. WiFi comes up with correct SSID and password. |

### Reflash Flow

| # | Test | What to verify |
|---|------|---------------|
| 6 | **Boot detection** | Setup mode entered from microSD boot; normal mode from onboard disk |
| 7 | **Update path** | Config preserved, eMMC hashes restored to `/etc/shadow` and `/etc/config/wireless` |
| 8 | **Fresh Start path** | New password replaces eMMC hashes, onboard disk wiped clean |
| 9 | **Auto-restore** | Simulate factory reset (wipe overlay), boot-time script restores both hashes without user interaction |
