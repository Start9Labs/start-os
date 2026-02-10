# StartWRT Init & Reflash Proposal

## Context

StartWRT routers use **two separate passwords** set at different times:

| Password | Set by | When | Purpose |
|----------|--------|------|---------|
| **WiFi password** | Factory worker (from sticker) | Manufacturing via serial console | WPA2-PSK WiFi access |
| **Admin password** | End user | First GUI access after unboxing, factory reset, or reflash | Web UI authentication |

The WiFi password is a unique **12-character** string from a 68-char unambiguous charset, printed on a sticker on the bottom of the device. It provides **~73 bits of entropy**, making rainbow table attacks infeasible even with a static SSID.

The admin password is user-chosen (any length/format) and stored in `/etc/shadow` on the overlay filesystem.

### Password lifecycle

| Stage | WiFi password | Admin password | Other settings |
|-------|--------------|----------------|----------------|
| **Manufacturing** | PMK stored on eMMC | Not set | Default config |
| **End user unboxing** | Works via sticker | User creates on first `router.local` visit | User configures |
| **Factory reset** | Restored from eMMC | Cleared — user sets via `router.local` | Wiped (overlay gone) |
| **Reflash / Update** | Restored from eMMC | User sets in captive portal wizard | **Preserved** |
| **Fresh Start** | User sets new in wizard (replaces eMMC) | User sets in captive portal wizard | Wiped |

### eMMC persistent partition

Mounted at **`/persistent`** — a dedicated partition on the same eMMC chip, excluded from factory reset wipes.

One file — the **WPA-PSK PMK** (no plaintext, no admin credentials):

| File | Contents | Purpose |
|------|----------|---------|
| `/persistent/wifi_pmk` | 64 hex chars (PBKDF2-SHA1 output) | WPA2-PSK key for `/etc/config/wireless` |

---

## Part 1: Manufacturing Init Tool

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

## Part 2: First-Time User Setup (Unboxing / Factory Reset)

After unboxing or factory reset, WiFi works immediately but no admin password is set. DNS hijacking forces setup before normal use.

```
1. User powers on router (or reboots after factory reset)
2. Connects to WiFi "StartWRT" using sticker password
3. No admin password → DNS hijacking active (all queries → router IP)
4. OS detects captive portal → auto-opens browser with setup page
5. GUI prompts: "Create your admin password"
6. User creates password (any length/format) → SHA-512 crypt → /etc/shadow
7. DNS hijacking disabled → normal browsing resumes
8. User is logged in
```

DNS hijacking ensures the admin password cannot be ignored — all internet access is blocked until setup is complete. If the user dismisses the captive portal popup, native apps and browsing remain broken until they open a browser and reach the setup page.

Implementation: `dnsmasq` config `address=/#/<router IP>` when no root hash in `/etc/shadow`. Removed after password is set.

> **Note**: After a reflash, the admin password is set in the captive portal wizard (Part 3), so this flow only applies to unboxing and factory reset.

---

## Part 3: Reflash / Reset Flow

### Trigger

Boot from **microSD card** with StartWRT image. U-Boot prioritizes microSD.

### Captive Portal

The microSD image starts a captive portal for the setup wizard:

| Setting | Value |
|---------|-------|
| **SSID** | `StartWRT-Setup` |
| **Password** | Default, documented (not printed on device) |
| **Security** | WPA2-PSK |
| `max_num_sta` | `1` (single client limit) |
| **Timeout** | AP shuts down after inactivity; re-insert microSD to restart |
| **DNS** | All queries resolve to router IP |

The user connects to `StartWRT-Setup`. DNS hijacking redirects all requests to the wizard. Modern OSes detect the captive portal and auto-open a browser window.

### Detection Logic

| Onboard config? | eMMC PMK? | Options |
|:---:|:---:|---|
| Yes | Yes | **Update** or **Fresh Start** |
| Yes | No | **Fresh Start** only |
| No | Yes | **Fresh Start** (WiFi auto-configured) |
| No | No | **Fresh Start** only |

### Path A: Update (keep settings, new admin password)

Physical access (microSD) = sufficient authorization.

```
 1. Detect onboard disk with config + eMMC WiFi PMK
 2. User selects "Update"
 3. Create admin password (any length/format)
 4. Confirm admin password
 5. Flash new firmware (replace squashfs base)
 6. Preserve config overlay EXCEPT /etc/shadow
 7. Write admin hash → /etc/shadow
 8. Restore eMMC WiFi PMK → /etc/config/wireless
 9. "Update complete. Remove microSD and reboot."
```

On first boot: WiFi works immediately (eMMC PMK, SSID `"StartWRT"`), admin login works immediately. All other settings (profiles, firewall, SSH keys, etc.) are preserved.

### Path B: Fresh Start (full wipe, new WiFi + admin passwords)

```
 1. User selects "Fresh Start"
 2. Select Language, Country, Drive
 3. Enter new WiFi password (any format/length)
 4. Confirm WiFi password
 5. Create admin password (any length/format)
 6. Confirm admin password
 7. PBKDF2-SHA1(WiFi password, "StartWRT", 4096) → new PMK
 8. Write new PMK to eMMC (replaces old)
 9. Wipe onboard disk config overlay entirely
10. Flash fresh firmware
11. Write PMK → /etc/config/wireless
12. Write admin hash → /etc/shadow
13. "Setup complete. Remove microSD and reboot."
```

On first boot: WiFi works immediately (new PMK, SSID `"StartWRT"`), admin login works immediately. All settings start fresh.

### Package Management

> **Open question**: User-installed packages (via `opkg`) live in the overlay. The Update path preserves the overlay, but package binaries may be incompatible with the new base firmware (kernel modules, ABI changes, renamed packages). Strategy TBD — options include: saving package list for manual reinstall, best-effort auto-reinstall, or documenting as a known limitation.

### Web UI Architecture

| Component | Details |
|-----------|---------|
| **Frontend** | Angular app from microSD image |
| **Backend** | `startwrt-ctrld` in "setup mode" |
| **Access** | Captive portal (DNS hijack → any URL reaches wizard) |

### Boot Detection

```
microSD boot?
├── Yes → Start captive portal ("StartWRT-Setup" AP, DNS hijack)
│         → Serve setup wizard
└── No  → Normal boot
          ├── Admin password set?
          │   ├── Yes → Normal operation
          │   └── No  → DNS hijack active (forces admin setup via captive portal)
          ├── WiFi in /etc/config/wireless? → Normal operation
          └── No WiFi + eMMC PMK? → Auto-restore WiFi → Normal operation
```

Auto-restore: factory reset wipes overlay → boot script restores WiFi PMK from eMMC. Admin password not restored — DNS hijacking forces user to set it before normal browsing works.

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

eMMC survives factory resets. Storing plaintext creates a persistent attack surface — malware that exfiltrates the password retains access even after the device is wiped. PMK is one-way and SSID-bound: changing the SSID invalidates a stolen PMK.

### WiFi Key in UI

The WiFi key (whether PMK or passphrase) is never displayed in the admin interface. The sticker is the source of truth.

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
| 9 | Fresh Start | New PMK on eMMC, everything wiped, admin re-prompted |
| 10 | Identity PSK | After reset, sticker WiFi works; profiles can be recreated |
