import { Injectable } from '@angular/core'
import { pauseFor } from '../../utils/pauseFor'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  LoginReq,
  SystemInfoRes,
  SetFileReq,
  SetUciReq,
  SetUciRes,
  VersionInfo,
  SetPasswordReq,
  SetPreferencesReq,
  VpnServer,
  VpnServerDeleteArgs,
  VpnServerPeerAddArgs,
  VpnServerPeerAddResponse,
  VpnServerPeerDeleteArgs,
  VpnServerSetArgs,
  VpnServers,
  WifiConfig,
  BlackoutWindow,
} from './api.service'
import {
  DhcpSection,
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from './types'
import { wanIpv4Dhcp } from 'src/app/routes/home/routes/wan/routes/ipv4/uci/mocks'
import { wanIpv6Slaac } from 'src/app/routes/home/routes/wan/routes/ipv6/uci/mock'
import { ddnsDyndns } from 'src/app/routes/home/routes/wan/routes/ddns/uci/mocks'
import { macRouterDevice } from 'src/app/routes/home/routes/wan/routes/mac/uci/mocks'
import { lanDefault } from 'src/app/routes/home/routes/lan/routes/ipv4/uci/mocks'
import { dhcpLanSlaacDhcpv6 } from 'src/app/routes/home/routes/lan/routes/ipv6/uci/mocks'
import { mockWireGuardSections } from 'src/app/routes/home/routes/outbound/uci/mocks'
import {
  generateMockDataUsage,
  getMockArpOutput,
  mockBlockedDevices,
  mockDhcpHosts,
  mockDhcpLeasesOutput,
} from 'src/app/routes/home/routes/devices/uci/mocks'
import { mockPublishedPorts } from 'src/app/routes/home/routes/published-ports/uci/mocks'
import {
  mockBrLan,
  mockWanDevice,
} from 'src/app/routes/home/routes/ethernet/uci/mocks'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  async login(params: LoginReq): Promise<null> {
    await pauseFor(250)

    return null
  }

  async logout(): Promise<null> {
    await pauseFor(250)

    return null
  }

  async exec(params: ExecReq): Promise<ExecRes> {
    await pauseFor(250)

    // Handle specific commands for device discovery
    if (params.command === 'ip' && params.args[0] === 'neigh') {
      // Check WAN and LAN IPv6 status to determine which addresses to show
      const wanIpv6Enabled = this.isWanIpv6Enabled()
      const lanIpv6Enabled = this.isLanIpv6Enabled()

      return {
        exitCode: 0,
        stdout: getMockArpOutput(wanIpv6Enabled, lanIpv6Enabled),
        stderr: '',
      }
    }

    if (params.command === 'cat' && params.args[0] === '/tmp/dhcp.leases') {
      return {
        exitCode: 0,
        stdout: mockDhcpLeasesOutput,
        stderr: '',
      }
    }

    // Mock Start9 DDNS hostname file
    if (params.command === 'cat' && params.args[0] === '/etc/start9/hostname') {
      return {
        exitCode: 0,
        stdout: 'abc123xyz.start9.net',
        stderr: '',
      }
    }

    // Handle service restart commands
    if (
      params.command === '/etc/init.d/firewall' ||
      params.command === '/etc/init.d/dnsmasq' ||
      params.command === '/etc/init.d/network' ||
      params.command === '/etc/init.d/ddns'
    ) {
      return {
        exitCode: 0,
        stdout: '',
        stderr: '',
      }
    }

    // Handle ubus calls for WAN status
    if (
      params.command === 'ubus' &&
      params.args[0] === 'call' &&
      params.args[1] === 'network.interface.wan'
    ) {
      return {
        exitCode: 0,
        stdout: JSON.stringify({
          'ipv4-address': [{ address: '203.0.113.45', mask: 24 }],
          'ipv6-address': [],
        }),
        stderr: '',
      }
    }

    // Handle nlbwmon queries for data usage
    if (params.command === 'nlbw') {
      // Parse the date from args (-t flag)
      const dateIndex = params.args.indexOf('-t')
      const startDate =
        dateIndex !== -1 ? params.args[dateIndex + 1] : undefined

      // Determine period based on date range
      let period: 'day' | 'week' | 'month' | '3months' = 'week'
      if (startDate) {
        const start = new Date(startDate)
        const now = new Date()
        const daysDiff = Math.floor(
          (now.getTime() - start.getTime()) / (24 * 60 * 60 * 1000),
        )
        if (daysDiff <= 1) period = 'day'
        else if (daysDiff <= 7) period = 'week'
        else if (daysDiff <= 30) period = 'month'
        else period = '3months'
      }

      // Generate mock data for all known MACs
      const macs = [
        '00:1a:2b:3c:4d:5e',
        '00:1a:2b:3c:4d:5f',
        'de:ad:be:ef:ca:fe',
        'de:ad:be:ef:ca:ff',
      ]

      const mockData: any[] = []
      for (const mac of macs) {
        const points = generateMockDataUsage(mac, period)
        for (const point of points) {
          mockData.push([mac, point.download, point.upload, point.timestamp])
        }
      }

      return {
        exitCode: 0,
        stdout: JSON.stringify({
          columns: ['mac', 'rx_bytes', 'tx_bytes', 'interval_start'],
          data: mockData,
        }),
        stderr: '',
      }
    }

    return {
      exitCode: 0,
      stdout: 'success',
      stderr: '',
    }
  }

  private mockAuthorizedKeys = `ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl matt@macbook
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJf3LQXK5m7dZtQgkVwMYxPragThKvOHPrLwfCfMR7fa lucy@desktop`

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(250)

    if (params.path === '/root/.ssh/authorized_keys') {
      return {
        modified: new Date().toISOString(),
        contents: this.mockAuthorizedKeys,
      }
    }

    return {
      modified: new Date().toISOString(),
      contents: '',
    }
  }

  async setFile(params: SetFileReq): Promise<null> {
    await pauseFor(250)

    if (params.path === '/root/.ssh/authorized_keys') {
      this.mockAuthorizedKeys = params.contents
    }

    return null
  }

  async getUci<T extends Record<string, UciFile<any>>>(
    params: GetUciReq,
  ): Promise<T> {
    await pauseFor(250)

    return params.names.reduce(
      (obj, name) => ({
        ...obj,
        [name]: mockUci[name],
      }),
      {} as T,
    )
  }

  async setUci<T extends string[]>(params: SetUciReq): Promise<SetUciRes<T>> {
    await pauseFor(250)

    const isoString = new Date().toISOString()

    // Actually update the mock data
    for (const name of Object.keys(params)) {
      if (mockUci[name]) {
        mockUci[name] = {
          ...params[name],
          modified: isoString,
        }
      }
    }

    return Object.keys(params).reduce(
      (obj, name) => ({
        ...obj,
        [name]: isoString,
      }),
      {} as SetUciRes<T>,
    )
  }

  async systemInfo(): Promise<SystemInfoRes> {
    await pauseFor(250)

    return {
      version: '1.0.0',
      language: 'English',
      date: new Date().toISOString(),
      theme: 'system',
    }
  }

  async systemNewerVersions(): Promise<VersionInfo[]> {
    await pauseFor(250)

    return [
      {
        version: '1.0.1',
        releaseNotes: `## Bug Fixes

- Resolved issue with DHCP lease renewals
- Fixed port forwarding rules not persisting after reboot
- Corrected timezone display in system logs

## Security Updates

- Updated OpenSSL to latest version
- Patched CVE-2024-1234 vulnerability`,
      },
      {
        version: '1.0.2',
        releaseNotes: `## What's New

- Improved Wi-Fi stability and range
- Fixed intermittent connection drops on 5GHz band
- Added support for WPA3 security protocol
- Performance improvements for VPN connections`,
      },
    ]
  }

  async systemRestart(): Promise<null> {
    await pauseFor(250)
    return null
  }

  async setPassword(params: SetPasswordReq): Promise<null> {
    await pauseFor(250)
    // Mock validation - in real implementation, backend validates old password
    if (params.oldPassword === '') {
      throw new Error('Invalid old password')
    }
    return null
  }

  async setPreferences(params: SetPreferencesReq): Promise<null> {
    await pauseFor(250)
    return null
  }

  private mockVpnServers: VpnServer[] = this.loadVpnServers()
  private mockNextIpCounter = this.loadIpCounter()

  private loadVpnServers(): VpnServer[] {
    const stored = localStorage.getItem('mock:vpn-servers')
    if (stored) {
      try {
        return JSON.parse(stored)
      } catch {}
    }
    return [
      {
        profile: 'guest',
        label: 'Family',
        enabled: true,
        listen_port: 51820,
        endpoint: '100.65.227.234',
        public_key: 'aB3dE5fG7hI9jK1lM3nO5pQ7rS9tU1vW3xY5zA7bC9d=',
        server_address: '192.168.1.1',
        peers: [],
      },
      {
        profile: 'lan',
        label: 'Matt',
        enabled: false,
        listen_port: 51821,
        endpoint: 'agf5d.start9.me',
        public_key: 'xY5zA7bC9daB3dE5fG7hI9jK1lM3nO5pQ7rS9tU1vW3=',
        server_address: '192.168.1.1',
        peers: [
          {
            name: 'tablet',
            ip: '192.168.1.2',
            public_key: '2JIBoK+Bxe7MJzX9zV+lFjqHxLTvehLp3piEROaNJjw=',
          },
          {
            name: 'Smartphone',
            ip: '192.168.1.3',
            public_key: 'bZIOgRYRGTX9x0OQsN1K+R63EhT2Pgo0WzYatTmzdDU=',
          },
        ],
      },
    ]
  }

  private loadIpCounter(): number {
    const stored = localStorage.getItem('mock:vpn-ip-counter')
    return stored ? parseInt(stored, 10) : 4
  }

  private persistVpnServers(): void {
    localStorage.setItem(
      'mock:vpn-servers',
      JSON.stringify(this.mockVpnServers),
    )
  }

  private persistIpCounter(): void {
    localStorage.setItem('mock:vpn-ip-counter', String(this.mockNextIpCounter))
  }

  async vpnServerList(): Promise<VpnServers> {
    await pauseFor(250)
    return { servers: this.mockVpnServers }
  }

  async vpnServerSet(params: VpnServerSetArgs): Promise<null> {
    await pauseFor(250)
    const existingIndex = this.mockVpnServers.findIndex(
      s => s.profile === params.profile,
    )
    if (existingIndex >= 0) {
      this.mockVpnServers = this.mockVpnServers.map((s, i) =>
        i === existingIndex
          ? { ...s, ...params.config, public_key: s.public_key }
          : s,
      )
    } else {
      this.mockVpnServers = [
        ...this.mockVpnServers,
        {
          profile: params.profile,
          ...params.config,
          public_key: 'mock' + btoa(String(Date.now())).slice(0, 40) + '=',
          server_address: '192.168.1.1',
          peers: [],
        },
      ]
    }
    this.persistVpnServers()
    return null
  }

  async vpnServerDelete(params: VpnServerDeleteArgs): Promise<null> {
    await pauseFor(250)
    this.mockVpnServers = this.mockVpnServers.filter(
      s => s.profile !== params.profile,
    )
    this.persistVpnServers()
    return null
  }

  async vpnServerPeerAdd(
    params: VpnServerPeerAddArgs,
  ): Promise<VpnServerPeerAddResponse> {
    await pauseFor(250)
    const server = this.mockVpnServers.find(s => s.profile === params.profile)
    const generatedKey =
      params.peer.public_key || btoa(String(Date.now())).slice(0, 40) + '='
    const ip =
      params.peer.ip ||
      `${server?.server_address.replace(/\.\d+$/, '')}.${this.mockNextIpCounter++}`
    this.persistIpCounter()
    const peer = { ...params.peer, public_key: generatedKey, ip }
    this.mockVpnServers = this.mockVpnServers.map(s =>
      s.profile === params.profile ? { ...s, peers: [...s.peers, peer] } : s,
    )
    this.persistVpnServers()
    return {
      public_key: generatedKey,
      ip,
      client_config: params.peer.public_key
        ? undefined
        : `[Interface]\nPrivateKey = MOCK_PRIVATE_KEY\nAddress = ${ip}/32\n\n[Peer]\nPublicKey = ${server?.public_key ?? 'mockServerKey'}\nEndpoint = ${server?.endpoint ?? 'mock'}:${server?.listen_port ?? 51820}\nAllowedIPs = 0.0.0.0/0`,
    }
  }

  async vpnServerPeerDelete(params: VpnServerPeerDeleteArgs): Promise<null> {
    await pauseFor(250)
    this.mockVpnServers = this.mockVpnServers.map(s =>
      s.profile === params.profile
        ? {
            ...s,
            peers: s.peers.filter(p => p.public_key !== params.public_key),
          }
        : s,
    )
    this.persistVpnServers()
    return null
  }

  private mockWifi: WifiConfig = {
    ssid: 'StartOS',
    radios: {
      default_radio0: {
        band: '2g',
        channel: 'auto',
        enabled: true,
        broadcast: true,
      },
      default_radio1: {
        band: '5g',
        channel: 'auto',
        enabled: true,
        broadcast: true,
      },
    },
    passwords: [
      {
        label: 'Home Network',
        profile: null,
        password: 'password123',
      },
      {
        label: 'Guest Wi-Fi',
        profile: {
          fullname: 'Guest',
          interface: 'guest',
          vlan_tag: 100,
        },
        password: 'guestpass456',
      },
    ],
  }

  async wifiGet(): Promise<WifiConfig> {
    await pauseFor(250)
    return structuredClone(this.mockWifi)
  }

  async wifiSet(params: WifiConfig): Promise<null> {
    await pauseFor(250)
    this.mockWifi = structuredClone(params)
    return null
  }

  private mockBlackoutWindows: BlackoutWindow[] = [
    {
      startTime: '22:00',
      endTime: '23:00',
      days: [false, true, true, true, true, true, false],
    },
  ]

  async wifiBlackoutGet(): Promise<BlackoutWindow[]> {
    await pauseFor(250)
    return structuredClone(this.mockBlackoutWindows)
  }

  async wifiBlackoutSet(params: BlackoutWindow[]): Promise<null> {
    await pauseFor(250)
    this.mockBlackoutWindows = structuredClone(params)
    return null
  }

  /**
   * Check if WAN IPv6 is enabled in mock UCI data
   */
  private isWanIpv6Enabled(): boolean {
    const network = mockUci['network']
    if (!network) return false

    const wan6 = network.sections.find(
      s => s.type === 'interface' && s.name === 'wan6',
    ) as NetworkInterfaceSection | undefined
    if (!wan6) return false

    // Check if proto is not 'none' or disabled
    const proto = wan6.options?.proto
    return !!proto && proto !== 'none'
  }

  /**
   * Check if LAN IPv6 is enabled in mock UCI data
   */
  private isLanIpv6Enabled(): boolean {
    const dhcp = mockUci['dhcp']
    if (!dhcp) return false

    const lan = dhcp.sections.find(
      s => s.type === 'dhcp' && s.name === 'lan',
    ) as DhcpSection | undefined
    if (!lan) return false

    // Check ra (router advertisement) setting
    const ra = lan.options?.ra
    return !!ra && ra !== 'disabled'
  }
}

export const mockUci: Record<string, UciFile<UciSection>> = {
  network: {
    sections: [
      wanIpv4Dhcp,
      wanIpv6Slaac,
      macRouterDevice,
      lanDefault,
      mockBrLan,
      mockWanDevice,
      ...mockWireGuardSections,
    ],
    modified: new Date().toISOString(),
  },
  ddns: {
    sections: [ddnsDyndns],
    modified: new Date().toISOString(),
  },
  dhcp: {
    sections: [dhcpLanSlaacDhcpv6, ...mockDhcpHosts],
    modified: new Date().toISOString(),
  },
  firewall: {
    sections: [...mockBlockedDevices, ...mockPublishedPorts],
    modified: new Date().toISOString(),
  },
}
