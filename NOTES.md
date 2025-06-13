# Architecture

Currently the ui folder is setup as basically dumb angular starter code,
since Sam doesn't know anything about writing angular frontends. Only
the ui/src/libluci and ui/src/controllers folders have anything of note.
Please look at the README.md files in each directory.

# Identity PSK

The basic pre-installed packages do not suport wpa_psk_file keyids.

- `opkg remove wpad-basic-mbedtls`
- `opkg install wpad-wolfssl`
- `opkg install hostapd-utils`

## /etc/config/wireless
- under `wifi-iface`
```
        option encryption 'psk2'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
```

## /etc/hostapd.wpa_psk (example)
```
00:00:00:00:00:00 password
keyid=guest 00:00:00:00:00:00 guestpassword
```

## check key of client
- `hostapd_cli all_sta`

# Security Profiles with VLANs

Every security profile is simply VLAN with its own subnet. That is basically
what VLANs are built to do. The config_experiments folder has the UCI configs
we came up with when manually setting up security profiles on a test device.

The source of truth for all of this is:
https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required

I have also gotten a lot of good info from:
https://fabianlee.org/2023/01/22/openwrt-bridge-vlan-filtering-for-openwrt-21-x-with-dsa-isolated-guest-wi-fi

There are also markdown exports of those articles in the `notes/` folder, useful
for plugging into LLMs like gemini 2.5 in order to get a bit of a Q&A session.

# Security Profiles with secprofd (Old Idea)

The idea here was to monitor all ip<->mac parings via addrwatch or by
participating in dhcp and also monitor keid<->mac pairings via the hostapd unix
socket (which is used by hostapd_cli under the hood). Then generate nftables
rules to open and close doors between ip addresses as quickly as possible
based on their associated mac addresss and keyids. This fundementally could not
work because a lot of the routing happens at layer 2 in-hardware, without ever
consulting the operating system.

# Wifi Modules

- `opkg install pciutils`
- `lspci`

If you see "Qualcomm Atheros AR93XX", then you need:
- `opkg install kmod-ath9k`

Not all wifi drivers support the VLAN features we would like to use. Notably the
ath11k chipset reqiures a driver patch which is not merged as of v24.10.0:
https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/204

The Raspberry Pi 4B shows the same "hostapd: Failed to create interface: -95
(Not supported)" error, but does not have a patch that I'm aware of. Probably
the hardware does not support VLAN tagging.

The notes/dynamic_vlan_in_drivers.md file has more information on why different
drivers do/don't support dynamic VLAN.

# Compiling OpenWRT

The best way I've found to compile OpenWRT is https://github.com/mwarning/docker-openwrt-build-env

Trying to build it on Arch Linux results in some silent undebuggable errors
