# StartWRT Init & Reflash Proposal

## Context

StartWRT routers use **two separate passwords** set at different times:

| Password | Set by | When | Purpose |
|----------|--------|------|---------|
| **WiFi password** | Factory worker (from sticker) | Manufacturing via serial console | WPA2-PSK WiFi access |
| **Admin password** | End user | First GUI access after unboxing, factory reset, or reflash | Web UI authentication |

The WiFi password is a unique **12-character** string from a 68-char unambiguous charset, printed on a sticker on the bottom of the device. It provides **~73 bits of entropy**, making rainbow table attacks infeasible even with a static SSID.

The admin password is user-chosen (minimum 12 characters, any format) and stored in `/etc/shadow` on the overlay filesystem.

### Password lifecycle

| Stage | WiFi password | Admin password | Other settings |
|-------|--------------|----------------|----------------|
| **Manufacturing** | PMK stored on eMMC | Not set | Default config |
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
| **eMMC — `/persistent` partition** | `wifi_pmk` (64 hex chars, PBKDF2-SHA1) | WPA2-PSK key — survives factory reset and reflash |
| **microSD** (removable) | Bootable reflash image | Recovery / reflash (Part 3) |

The `/persistent` partition is excluded from factory reset wipes. It stores one file — the **WPA-PSK PMK** (no plaintext, no admin credentials).

---

## Part 1: Manufacturing

### Initial Firmware Flash

U-Boot auto-flashes firmware from microSD to the eMMC firmware partitions at the bootloader level — no OS is booted from the microSD.

```
1. Factory worker inserts microSD containing firmware image file
2. Powers on device
3. U-Boot detects firmware image on microSD → writes to eMMC firmware partitions
4. Flash complete (LED/serial confirmation)
5. Factory worker removes microSD
6. Device reboots from eMMC
7. Serial dispatcher sees no /persistent/wifi_pmk → launches startwrt-cli init
```

> **Note**: This is distinct from the Part 3 reflash flow, where the microSD contains a bootable image that runs a setup wizard. Here, U-Boot copies a raw image to eMMC firmware partitions without booting it. The `/persistent` partition is untouched.

### Init Tool

**`startwrt-cli init`** — a local-only subcommand. Provisions WiFi only.

| Design decision | Rationale |
|----------------|-----------|
| Subcommand of `startwrt-cli` | Zero deployment overhead. Handler skips RPC calls. |
| Rust (not shell) | PBKDF2-SHA1 requires crypto libs not guaranteed on minimal OpenWrt. |
| Smart serial dispatcher | Keeps serial useful for debugging the live system. |

### Serial Console Dispatcher

`/usr/sbin/startwrt-serial`:

```sh
#!/bin/sh
if grep -q "mmcblk1" /proc/cmdline; then
    exec /bin/login                    # microSD boot → login (wizard on web)
elif ! [ -f /persistent/wifi_pmk ]; then
    exec startwrt-cli init             # no WiFi key → manufacturing init
else
    exec /bin/login                    # normal → login for debugging
fi
```

Inittab: `ttyS0::respawn:/sbin/agetty -L -l /usr/sbin/startwrt-serial ttyS0 115200 vt100`

### Init Tool Flow

```
 1. Check /persistent mounted → if not, attempt mount → fail = error + exit
 2. /persistent/wifi_pmk exists? → "Device already initialized." + exit
 3. Banner: "StartWRT Device Initialization"
 4. Prompt: "Enter device password: " (no echo)
 5. Validate: exactly 12 chars, unambiguous charset only
 6. Prompt: "Confirm device password: " (no echo)
 7. Mismatch → reprompt
 8. Compute PBKDF2-SHA1(password, "StartWRT", 4096, 32) → 64 hex chars
 9. Write /persistent/wifi_pmk (atomic)
10. Write PMK → /etc/config/wireless
11. Enable WiFi: SSID "StartWRT", WPA2-PSK, dynamic_vlan = ALLOWED
12. "Device initialized successfully." + exit
```

### Password Character Set

| Category | Characters | Count |
|----------|-----------|-------|
| Uppercase | `A-Z` minus `I`, `O` | 24 |
| Lowercase | `a-z` minus `l`, `o` | 24 |
| Digits | `2-9` | 8 |
| Special | `! @ # $ % ^ & * - _ + =` | 12 |
| **Total** | | **68** |

12 chars × 68-char set ≈ **73 bits entropy**

---

## Captive Portal Mechanism

Both first-time setup (Part 2) and reflash (Part 3) use the same two-layer mechanism:

1. **DNS hijacking** — `dnsmasq` configured with `address=/#/<router IP>` makes all DNS queries resolve to the router.
2. **Captive portal detection** — Modern OSes probe known URLs on network join. When DNS hijacking redirects these probes, the OS detects a captive portal and auto-opens a browser pointed at the router's setup page.

Both scenarios use the same `StartWRT` SSID — they differ only in what the setup page does. Part 2 always uses the eMMC WiFi PMK; Part 3 resolves the PMK via precedence (baked-in first, then eMMC — see Part 3).

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

### WiFi PMK Precedence

The microSD image resolves the WiFi PMK with baked-in taking priority over eMMC:

```
Baked-in PMK in image (e.g. /image/wifi_pmk)?
├── Yes → Use baked-in PMK (custom image — user lost sticker)
└── No  → Read /persistent/wifi_pmk from eMMC
            ├── Found → Use eMMC PMK (standard image — sticker password)
            └── Not found → Cannot start AP — custom image required
```

This precedence ensures a custom image user (who has lost their sticker password) can always connect.

### Captive Portal

The microSD image starts the `StartWRT` AP using the resolved PMK with a captive portal:

| Setting | Value |
|---------|-------|
| **SSID** | `StartWRT` |
| **Password** | Resolved PMK (baked-in or eMMC — see precedence above) |
| **Security** | WPA2-PSK |
| `max_num_sta` | `1` (single client limit during setup) |
| **DNS** | All queries resolve to router IP |

The user connects to `StartWRT` using their sticker password (or the password they chose when building a custom image). The captive portal redirects all requests to the wizard — the OS detects this and auto-opens a browser window.

> **Lost sticker password?** A separate image-building tool creates a custom microSD image with a user-chosen WiFi password baked in. The baked-in PMK is written to eMMC during flashing, permanently replacing the original sticker password.

### Detection Logic

| Onboard config? | eMMC PMK? | Standard image | Custom image (baked-in password) |
|:---:|:---:|---|---|
| Yes | Yes | **Update** or **Fresh Start** | **Update** or **Fresh Start** (replaces eMMC PMK) |
| Yes | No | Cannot start AP — custom image required | **Update** or **Fresh Start** (writes PMK to eMMC) |
| No | Yes | **Fresh Start** | **Fresh Start** (replaces eMMC PMK) |
| No | No | Cannot start AP — custom image required | **Fresh Start** (writes PMK to eMMC) |

### Path A: Update (keep settings, new admin password)

Physical access (microSD) = sufficient authorization.

```
 1. Detect onboard disk with config + WiFi PMK (resolved via precedence)
 2. User selects "Update"
 3. Create admin password (minimum 12 characters)
 4. Confirm admin password
 5. Backup config files (sysupgrade conffiles list)
 6. Flash new firmware (replace squashfs base, wipe overlay)
 7. Restore config files EXCEPT /etc/shadow
 8. Write admin hash → /etc/shadow
 9. If custom image: write baked-in PMK to eMMC (replaces old)
10. Restore WiFi PMK → /etc/config/wireless
11. "Update complete. Remove microSD and reboot."
```

On first boot: WiFi works immediately (eMMC PMK, SSID `"StartWRT"`), admin login works immediately. Config files (firewall rules, WiFi profiles, SSH keys, etc.) are preserved via `sysupgrade` conffiles. User-installed package binaries are wiped — users must reinstall packages, though their config files are retained. If a custom image was used, the sticker password is permanently replaced by the baked-in password.

### Path B: Fresh Start (full wipe, new admin password)

```
 1. User selects "Fresh Start"
 2. Select Language, Country, Drive/Partition
 3. Create admin password (minimum 12 characters)
 4. Confirm admin password
 5. Wipe onboard disk config overlay entirely
 6. Flash fresh firmware
 7. If custom image: write baked-in PMK to eMMC (replaces old)
 8. Restore WiFi PMK from eMMC → /etc/config/wireless
 9. Write admin hash → /etc/shadow
10. "Setup complete. Remove microSD and reboot."
```

On first boot: WiFi works immediately (eMMC PMK, SSID `"StartWRT"`), admin login works immediately. All settings start fresh. If a custom image was used, the sticker password is permanently replaced by the baked-in password.

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
├── Yes → Resolve WiFi PMK (baked-in first, then eMMC) → Start "StartWRT" AP
│         → Captive portal (DNS hijack) → Serve setup wizard
└── No  → Normal boot
          1. WiFi in /etc/config/wireless?
          │   ├── Yes → skip
          │   └── No  → eMMC PMK? → Auto-restore WiFi → continue
          2. Admin password set?
              ├── Yes → Normal operation
              └── No  → Captive portal active (forces admin setup before normal browsing)
```

Auto-restore: factory reset wipes overlay → boot script restores WiFi PMK from eMMC. Admin password not restored — captive portal forces user to set it before normal browsing works.

---

## Design Notes

### WiFi PSK and Security Profiles

PMK as `iface.key` is fully compatible with identity PSK. The code in `wifi.rs` treats the key as an opaque string (read/write pass-through). Hostapd handles both passphrases and hex PMKs in `option key`. Per-profile PSKs in `wpa_psk_file` are processed independently.

```
WiFi Interface (iface.key = sticker PMK)
├── dynamic_vlan = ALLOWED
├── WifiStation { key: "profile1_pass", vid: 101 }
└── Default: sticker PMK → no VLAN → main LAN
```

### Why PMK, Not Plaintext

eMMC survives factory resets. PMK is one-way — if malware exfiltrates it, the attacker gains WiFi access but cannot recover the plaintext password (which the user may reuse elsewhere). Storing plaintext would expose the actual password from the sticker.

### WiFi Key in UI

The WiFi key (whether PMK or passphrase) is never displayed in the admin interface. The sticker is the source of truth — unless a custom image has replaced the eMMC PMK, in which case the user's chosen password supersedes the sticker permanently.

---

## Implementation

### Files

| File | Action | Purpose |
|------|--------|---------|
| `ctrl/src/init.rs` | **Create** | Init subcommand: password prompt, validation, PMK derivation, eMMC write |
| `ctrl/src/emmc.rs` | **Create** | eMMC helpers: read/write PMK, mount check (shared by init + reflash + boot restore) |
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

`pbkdf2`, `hmac`, `sha1` — for WPA-PSK PMK derivation.

---

## Verification

| # | Test | Verify |
|---|------|--------|
| 1 | Charset validation | 12-char valid passes; ambiguous/wrong-length rejected |
| 2 | eMMC write | `wifi_pmk` is 64 hex chars |
| 3 | PMK compatibility | Matches `wpa_passphrase "StartWRT" <password>` |
| 4 | Serial dispatcher | Correct mode per state |
| 5 | Manual serial test | Full agetty → init flow, WiFi works with sticker password |
| 6 | First-time admin | GUI prompts for admin password when unset |
| 7 | Factory reset | WiFi restores from eMMC, admin unset, settings wiped |
| 8 | Update path | Settings preserved, `/etc/shadow` cleared, WiFi restored, admin re-prompted |
| 9 | Fresh Start | Everything wiped, WiFi PMK preserved (or replaced by custom image), admin re-prompted |
| 10 | Identity PSK | After reset, sticker WiFi works; profiles can be recreated |
| 11 | Custom image | Baked-in PMK used for AP, written to eMMC, survives reboot, old sticker invalid |
| 12 | Standard image + no eMMC PMK | AP fails to start, error displayed on serial console |
