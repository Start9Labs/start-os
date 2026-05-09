import { InjectionToken, signal } from '@angular/core'
import devicesDevice from './devices/device.html?raw'

import devices from './devices/index.html?raw'
import ethernetDialog from './ethernet/dialog.html?raw'
import ethernet from './ethernet/index.html?raw'
import inboundClientAdd from './inbound/client-dialog-add.html?raw'
import inboundClientConfig from './inbound/client-dialog-config.html?raw'
import inboundClientRename from './inbound/client-dialog-rename.html?raw'
import inboundClient from './inbound/client.html?raw'
import inboundDialog from './inbound/dialog.html?raw'
import inbound from './inbound/index.html?raw'
import lanIpv4 from './lan/ipv4.html?raw'
import lanIpv6 from './lan/ipv6.html?raw'
import outboundDialog from './outbound/dialog.html?raw'
import outbound from './outbound/index.html?raw'
import outboundVpn from './outbound/vpn.html?raw'
import profiles from './profiles/index.html?raw'
import profilesDialog from './profiles/dialog.html?raw'
import profilesSchedule from './profiles/schedule.html?raw'
import publishedPortsDialog from './published-ports/dialog.html?raw'
import publishedPorts from './published-ports/index.html?raw'
import settingsActivity from './settings/activity.html?raw'
import settingsAdvanced from './settings/advanced.html?raw'
import settingsBackup from './settings/backup.html?raw'
import settingsGeneral from './settings/general.html?raw'
import settingsLogs from './settings/logs.html?raw'
import settingsPassword from './settings/password.html?raw'
import settingsSshDialog from './settings/ssh-dialog.html?raw'
import settingsSsh from './settings/ssh.html?raw'
import wanDdns from './wan/ddns.html?raw'
import wanDns from './wan/dns.html?raw'
import wanIpv4 from './wan/ipv4.html?raw'
import wanIpv6 from './wan/ipv6.html?raw'
import wanMac from './wan/mac.html?raw'
import wifiBlackoutDialog from './wifi/blackout-dialog.html?raw'
import wifiBlackout from './wifi/blackout.html?raw'
import wifiPasswordsDialog from './wifi/passwords-dialog.html?raw'
import wifiPasswords from './wifi/passwords.html?raw'
import wifiSettings from './wifi/settings.html?raw'

export const HELP_OPEN = new InjectionToken('Help sidebar open status', {
  factory: () => signal(false),
})

export const HELP_URL = new InjectionToken<string>('Help URL')
export const HELP = new InjectionToken<Record<string, string>>(
  'Help dictionary',
  {
    factory: () => ({
      '/devices': devices,
      '/devices/device': devicesDevice,
      '/ethernet': ethernet,
      '/ethernet/dialog': ethernetDialog,
      '/inbound': inbound,
      '/inbound/dialog': inboundDialog,
      '/inbound/client': inboundClient,
      '/inbound/client/dialog-add': inboundClientAdd,
      '/inbound/client/dialog-config': inboundClientConfig,
      '/inbound/client/dialog-rename': inboundClientRename,
      '/lan/ipv4': lanIpv4,
      '/lan/ipv6': lanIpv6,
      '/outbound': outbound,
      '/outbound/vpn': outboundVpn,
      '/outbound/dialog': outboundDialog,
      '/profiles': profiles,
      '/profiles/dialog': profilesDialog,
      '/profiles/schedule': profilesSchedule,
      '/published-ports': publishedPorts,
      '/published-ports/dialog': publishedPortsDialog,
      '/wan/ipv4': wanIpv4,
      '/wan/ipv6': wanIpv6,
      '/wan/mac-address': wanMac,
      '/wan/dns': wanDns,
      '/wan/dynamic-dns': wanDdns,
      '/settings/activity': settingsActivity,
      '/settings/advanced': settingsAdvanced,
      '/settings/backup': settingsBackup,
      '/settings/general': settingsGeneral,
      '/settings/logs': settingsLogs,
      '/settings/password': settingsPassword,
      '/settings/ssh-keys': settingsSsh,
      '/settings/ssh-keys/dialog': settingsSshDialog,
      '/wifi/blackout-schedule': wifiBlackout,
      '/wifi/blackout-schedule/dialog': wifiBlackoutDialog,
      '/wifi/passwords': wifiPasswords,
      '/wifi/passwords/dialog': wifiPasswordsDialog,
      '/wifi/settings': wifiSettings,
    }),
  },
)

export function provideHelp(useValue: string) {
  return { provide: HELP_URL, useValue }
}
