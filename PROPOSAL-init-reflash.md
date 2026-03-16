# StartWRT Init & Reflash Proposal

## Context

StartWRT routers use **two separate passwords** set at different times:

| Password | Set by | When | Purpose |
|----------|--------|------|---------|
| **WiFi password** | Factory worker (from sticker) | Manufacturing via serial console | WPA2-PSK WiFi access |
| **Admin password** | End user | First GUI access after unboxing, factory reset, or reflash | Web UI authentication |

The WiFi password is a unique **12-character** string from a 67-char unambiguous charset, printed on a sticker on the bottom of the device. It provides **~72.3 bits of entropy**, making rainbow table attacks infeasible even with a static SSID.

The admin password is user-chosen (minimum 12 characters, any format) and stored in `/etc/shadow` on the overlay filesystem.

### Password lifecycle

| Stage | WiFi password | Admin password | Other settings |
|-------|--------------|----------------|----------------|
| **Manufacturing** | Password stored on eMMC | Not set | Default config |
| **End user unboxing** | Works via sticker | User creates via captive portal | User configures |
| **Factory reset** (no microSD) | Restored from eMMC | Cleared — user sets via captive portal on reboot | Wiped (overlay gone), firmware unchanged |
| **Reflash / Update** (microSD) | Restored from eMMC (or replaced by custom image) | User sets in wizard before reboot | **Preserved**, firmware replaced |
| **Fresh Start** (microSD) | Restored from eMMC (or replaced by custom image) | User sets in wizard before reboot | Wiped, firmware replaced |

### Storage Architecture

The BPI-F3 eMMC is partitioned for both firmware and persistent data. SPI NOR holds only the bootloader.

| Storage | Contents | Purpose |
|---------|----------|---------|
| **SPI NOR** (4MB) | U-Boot | Bootloader — selects eMMC or microSD boot |
| **eMMC — firmware partitions** | Kernel, SquashFS root, overlay | Main OS — factory reset wipes overlay only |
| **eMMC — `/key_backup` partition** | `wifi_password` (plaintext passphrase) | WiFi password — survives factory reset and reflash |
| **microSD** (removable) | Bootable reflash image | Recovery / reflash (Part 3) |

The `/key_backup` partition is excluded from factory reset wipes. It stores one file — the **WiFi password** (no admin credentials).

---

## Part 1: Manufacturing

### Initial Firmware Flash + WiFi Provisioning

U-Boot tries microSD first by default on the BPI-F3 (boot selection switch SW1). Manufacturing uses the same bootable microSD image as end-user reflash (Part 3), but driven via serial console instead of the WiFi wizard.

```
1. Factory worker inserts microSD with bootable StartWRT image
2. Powers on device
3. U-Boot boots from microSD
4. Serial dispatcher → login shell
5. Factory worker flashes firmware to eMMC partitions (e.g. startwrt-cli flash)
6. Factory worker runs startwrt-cli init → enters sticker password → password written to eMMC /key_backup
7. Factory worker removes microSD
8. Device reboots from eMMC — WiFi works immediately, no admin password set
```

> **Note**: Manufacturing and reflash share the same microSD image. Manufacturing uses serial; reflash uses the WiFi captive portal wizard. This avoids maintaining separate images.

### Init Tool

**`startwrt-cli init`** — a local-only subcommand. Provisions WiFi only.

| Design decision | Rationale |
|----------------|-----------|
| Subcommand of `startwrt-cli` | Zero deployment overhead. Handler skips RPC calls. |
| Rust (not shell) | Consistent with the rest of the Rust backend. |
| Smart serial dispatcher | Keeps serial useful for debugging the live system. |

### Serial Console Dispatcher

`/usr/sbin/startwrt-serial`:

```sh
#!/bin/sh
if grep -q "mmcblk1" /proc/cmdline; then
    exec /bin/login                    # microSD boot → login (wizard on web)
elif ! [ -f /key_backup/wifi_password ]; then
    exec startwrt-cli init             # no WiFi key → manufacturing init
else
    exec /bin/login                    # normal → login for debugging
fi
```

Inittab: `ttyS0::respawn:/sbin/agetty -L -l /usr/sbin/startwrt-serial ttyS0 115200 vt100`

### Init Tool Flow

```
 1. Check /key_backup mounted → if not, attempt mount → fail = error + exit
 2. /key_backup/wifi_password exists? → "Device already initialized." + exit
 3. Banner: "StartWRT Device Initialization"
 4. Prompt: "Enter device password: " (no echo)
 5. Validate: exactly 12 chars, unambiguous charset only
 6. Prompt: "Confirm device password: " (no echo)
 7. Mismatch → reprompt
 8. Write password → /key_backup/wifi_password (atomic)
 9. Write password → /etc/config/wireless
10. Enable WiFi: SSID "StartWRT", WPA2-PSK, dynamic_vlan = ALLOWED
11. "Device initialized successfully." + exit
```

### Password Character Set

| Category | Characters | Count |
|----------|-----------|-------|
| Uppercase | `A-Z` minus `I`, `O` | 24 |
| Lowercase | `a-z` minus `i`, `l`, `o` | 23 |
| Digits | `2-9` | 8 |
| Special | `! @ # $ % ^ & * - _ + =` | 12 |
| **Total** | | **67** |

12 chars × 67-char set ≈ **72.3 bits entropy**

---

## Captive Portal Mechanism

Both first-time setup (Part 2) and reflash (Part 3) use the same two-layer mechanism:

1. **DNS hijacking** — `dnsmasq` configured with `address=/#/<router IP>` makes all DNS queries resolve to the router.
2. **Captive portal detection** — Modern OSes probe known URLs on network join. When DNS hijacking redirects these probes, the OS detects a captive portal and auto-opens a browser pointed at the router's setup page.

Both scenarios use the same `StartWRT` SSID — they differ only in what the setup page does. Part 2 always uses the eMMC WiFi password; Part 3 resolves the password via precedence (baked-in first, then eMMC — see Part 3).

---

## Part 2: First-Time User Setup (Unboxing / Factory Reset)

After unboxing or factory reset, WiFi works immediately but no admin password is set. A captive portal on the main network forces setup before normal use.

```
1. User powers on router (or reboots after factory reset)
2. Connects to WiFi "StartWRT" using sticker password
3. No admin password → captive portal active (all DNS queries → router IP)
4. OS detects captive portal → auto-opens browser with setup page
5. GUI prompts: "Create your admin password"
6. User creates password (minimum 12 characters)
7. User confirms password
8. SHA-512 crypt → /etc/shadow
9. DNS hijacking disabled → normal browsing resumes
10. User is logged in
```

The captive portal ensures the admin password cannot be ignored — all internet access is blocked until setup is complete. If the user dismisses the captive portal popup, native apps and browsing remain broken until they open a browser and reach the setup page.

Implementation: `dnsmasq` `address=/#/<router IP>` when no root hash in `/etc/shadow`. Removed after password is set. Unlike the reflash captive portal, `max_num_sta` is not restricted — the device is already on a private network protected by the sticker password.

> **Note**: After a reflash, the admin password is set in the captive portal wizard (Part 3), so this flow only applies to unboxing and factory reset.

---

## Part 3: Reflash / Reset Flow

### Trigger

Boot from **microSD card** with StartWRT image. U-Boot prioritizes microSD.

### WiFi Password Precedence

The microSD image resolves the WiFi password with baked-in taking priority over eMMC:

```
Baked-in password in image (SWRTPWD magic in squashfs padding)?
├── Yes → Use baked-in password (custom image — user lost sticker)
└── No  → Read /key_backup/wifi_password from eMMC
            ├── Found → Use eMMC password (standard image — sticker password)
            └── Not found → Cannot start AP — custom image required
```

This precedence ensures a custom image user (who has lost their sticker password) can always connect.

### Captive Portal

The microSD image starts the `StartWRT` AP using the resolved password with a captive portal:

| Setting | Value |
|---------|-------|
| **SSID** | `StartWRT` |
| **Password** | Resolved password (baked-in or eMMC — see precedence above) |
| **Security** | WPA2-PSK |
| `max_num_sta` | `1` (single client limit during setup) |
| **DNS** | All queries resolve to router IP |

The user connects to `StartWRT` using their sticker password (or the password they chose when building a custom image). The captive portal redirects all requests to the wizard — the OS detects this and auto-opens a browser window.

> **Lost sticker password?** A separate image-building tool creates a custom microSD image with a user-chosen WiFi password baked in. The baked-in password is written to eMMC during flashing, permanently replacing the original sticker password.

### Detection Logic

| Onboard config? | eMMC password? | Standard image | Custom image (baked-in password) |
|:---:|:---:|---|---|
| Yes | Yes | **Update** or **Fresh Start** | **Update** or **Fresh Start** (replaces eMMC password) |
| Yes | No | Cannot start AP — custom image required | **Update** or **Fresh Start** (writes password to eMMC) |
| No | Yes | **Fresh Start** | **Fresh Start** (replaces eMMC password) |
| No | No | Cannot start AP — custom image required | **Fresh Start** (writes password to eMMC) |

### Path A: Update (keep settings, new admin password)

Physical access (microSD) = sufficient authorization.

```
 1. Detect onboard disk with config + WiFi password (resolved via precedence)
 2. User selects "Update"
 3. Create admin password (minimum 12 characters)
 4. Confirm admin password
 5. Backup config files (sysupgrade conffiles list)
 6. Flash new firmware (replace squashfs base, wipe overlay)
 7. Restore config files EXCEPT /etc/shadow
 8. Write admin hash → /etc/shadow
 9. If custom image: write baked-in password to eMMC (replaces old)
10. Restore WiFi password → /etc/config/wireless
11. "Update complete. Remove microSD and reboot."
```

On first boot: WiFi works immediately (eMMC password, SSID `"StartWRT"`), admin login works immediately. Config files (firewall rules, WiFi profiles, SSH keys, etc.) are preserved via `sysupgrade` conffiles. User-installed package binaries are wiped — users must reinstall packages, though their config files are retained. If a custom image was used, the sticker password is permanently replaced by the baked-in password.

### Path B: Fresh Start (full wipe, new admin password)

```
 1. User selects "Fresh Start"
 2. Select Language, Country, Drive/Partition
 3. Create admin password (minimum 12 characters)
 4. Confirm admin password
 5. Wipe onboard disk config overlay entirely
 6. Flash fresh firmware
 7. If custom image: write baked-in password to eMMC (replaces old)
 8. Restore WiFi password from eMMC → /etc/config/wireless
 9. Write admin hash → /etc/shadow
10. "Setup complete. Remove microSD and reboot."
```

On first boot: WiFi works immediately (eMMC password, SSID `"StartWRT"`), admin login works immediately. All settings start fresh. If a custom image was used, the sticker password is permanently replaced by the baked-in password.

### Package Management

All packages required by StartWRT are included in the firmware image. Both Update and Fresh Start wipe the overlay, so user-installed package binaries are always removed to avoid conflicts with the new firmware or UCI config files. The Update path preserves config files via `sysupgrade` conffiles — so package configs survive even though binaries are wiped. Users will need to reinstall any additional packages after reflash.

### Web UI Architecture

| Component | Details |
|-----------|---------|
| **Frontend** | Angular app from microSD image |
| **Backend** | `startwrt-ctrld` in "setup mode" |
| **Access** | Captive portal (DNS hijack → any URL reaches wizard) |

### Boot Detection

```
microSD boot?
├── Yes → Resolve WiFi password (baked-in first, then eMMC) → Start "StartWRT" AP
│         → Captive portal (DNS hijack) → Serve setup wizard
└── No  → Normal boot
          1. WiFi in /etc/config/wireless?
          │   ├── Yes → skip
          │   └── No  → eMMC password? → Auto-restore WiFi → continue
          2. Admin password set?
              ├── Yes → Normal operation
              └── No  → Captive portal active (forces admin setup before normal browsing)
```

Auto-restore: factory reset wipes overlay → boot script restores WiFi password from eMMC. Admin password not restored — captive portal forces user to set it before normal browsing works.

---

## Design Notes

### WiFi PSK and Security Profiles

Passphrase as `iface.key` is fully compatible with identity PSK. The code in `wifi.rs` treats the key as an opaque string (read/write pass-through). Hostapd derives the PMK internally from the passphrase. Per-profile PSKs in `wpa_psk_file` are processed independently.

```
WiFi Interface (iface.key = sticker password)
├── dynamic_vlan = ALLOWED
├── WifiStation { key: "profile1_pass", vid: 101 }
└── Default: sticker password → no VLAN → main LAN
```

### Why Plaintext, Not PMK

The sticker password is physically printed on the device — storing it as plaintext on the eMMC is no less secure than the sticker itself. Hostapd derives the PMK internally from the passphrase, so there is no need for the backend to perform PBKDF2-SHA1 derivation. Storing plaintext simplifies the codebase (no `pbkdf2`/`sha1`/`hmac` dependencies) and makes the stored value directly usable as a WPA2 passphrase without conversion.

### WiFi Key in UI

The WiFi passphrase is never displayed in the admin interface. The sticker is the source of truth — unless a custom image has replaced the eMMC password, in which case the user's chosen password supersedes the sticker permanently.

---

## Implementation

### Files

| File | Action | Purpose |
|------|--------|---------|
| `ctrl/src/init.rs` | **Create** | Init subcommand: password prompt, validation, eMMC write |
| `ctrl/src/emmc.rs` | **Create** | eMMC helpers: read/write password, mount check (shared by init + reflash + boot restore) |
| `ctrl/src/lib.rs` | **Modify** | Add modules, wire `init` into CLI |
| `firstboot_config/inittab` | **Create** | agetty serial config |
| `firstboot_config/startwrt-serial` | **Create** | Serial dispatcher script |
| `ctrl/src/setup.rs` | **Create** | Reflash mode: disk detection, flash, eMMC management |
| `ctrl/src/bin/startwrt-ctrld.rs` | **Modify** | Boot-time WiFi auto-restore + setup mode detection |
| `web/` (setup wizard) | **Create** | Angular wizard UI |

### Reusable Code

| Source | Reuse |
|--------|-------|
| `auth.rs:432` — `rpassword::prompt_password()` | No-echo prompting |
| `auth.rs` — atomic write pattern | temp → write → fsync → rename |
| `uciedit` crate | UCI config writing |
| `wifi.rs` | WiFi PSK + dynamic VLAN structure |
| `start-os/web/projects/setup-wizard/` | Angular wizard reference |

### New Dependencies

None — plaintext password storage eliminates the need for `pbkdf2`, `hmac`, `sha1`.

---

## Verification

| # | Test | Verify |
|---|------|--------|
| 1 | Charset validation | 12-char valid passes; ambiguous/wrong-length rejected |
| 2 | eMMC write | `wifi_password` contains plaintext passphrase |
| 3 | WiFi connectivity | Passphrase works with hostapd WPA2-PSK |
| 4 | Serial dispatcher | Correct mode per state |
| 5 | Manual serial test | Full agetty → init flow, WiFi works with sticker password |
| 6 | First-time admin | GUI prompts for admin password when unset |
| 7 | Factory reset | WiFi restores from eMMC password, admin unset, settings wiped |
| 8 | Update path | Settings preserved, `/etc/shadow` cleared, WiFi restored, admin re-prompted |
| 9 | Fresh Start | Everything wiped, WiFi password preserved (or replaced by custom image), admin re-prompted |
| 10 | Identity PSK | After reset, sticker WiFi works; profiles can be recreated |
| 11 | Custom image | Baked-in password used for AP, written to eMMC, survives reboot, old sticker invalid |
| 12 | Standard image + no eMMC password | AP fails to start, error displayed on serial console |
