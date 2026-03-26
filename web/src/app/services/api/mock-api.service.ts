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
  ScheduleWindow,
  ProfileId,
  ProfileIdOpt,
  SecurityProfile,
  ProfileCreateInput,
  ProfileUpdateInput,
  CheckInitializedRes,
  SetInitialPasswordReq,
  SetupStatusRes,
  LogEntry,
  LogsResponse,
  DeviceFromApi,
  DeviceUpdateReq,
  DeviceDataUsageReq,
  DataUsagePointFromApi,
  LanIpv4Response,
  LanIpv4SetRequest,
  LanIpv6Response,
  LanIpv6SetRequest,
  WanIpv4Response,
  WanIpv4SetRequest,
  WanIpv6Response,
  WanIpv6SetRequest,
  WanMacResponse,
  WanMacSetRequest,
  WanDnsResponse,
  WanDnsSetRequest,
  WanDdnsResponse,
  WanDdnsSetRequest,
  PublishedPortFromApi,
  PublishedPortsSetRequest,
  OutboundVpn,
  OutboundVpnCreateRequest,
  OutboundVpnCreateResponse,
  OutboundVpnUpdateRequest,
  OutboundVpnDeleteRequest,
  OutboundVpnSetEnabledRequest,
  EthernetConfig,
  EthernetSetConfig,
  SshKeyFromApi,
  SshKeysAddRequest,
  SshKeysDeleteRequest,
  ActivityEntry,
  ActivityListParams,
  ActivityListResponse,
  BackupCreateRes,
  BackupRestoreRes,
  DiagnosticsCreateRes,
} from './api.service'
import {
  DhcpSection,
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from './types'
import { dhcpLanSlaacDhcpv6 } from 'src/app/routes/lan/routes/ipv6/uci/mocks'
import {
  generateMockDataUsage,
  getMockArpOutput,
  mockBlockedDevices,
  mockConnectionTypes,
  mockDataUsageTotals,
  mockDeviceSpeeds,
  mockDhcpHosts,
  mockDhcpLeasesOutput,
} from 'src/app/routes/devices/uci/mocks'

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

  async setTimezone(_params: {
    timezone: string
    posixTz: string
  }): Promise<null> {
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

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(250)
    return {
      modified: new Date().toISOString(),
      contents: '',
    }
  }

  async setFile(params: SetFileReq): Promise<null> {
    await pauseFor(250)
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
      language: 'en_US',
      date: new Date().toISOString(),
      theme: 'system',
      remoteAccess: 'default',
      timezone: Intl.DateTimeFormat().resolvedOptions().timeZone,
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
    broadcastSeparately: false,
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
      startTime: '20:00',
      endTime: '23:00',
      days: [false, true, true, true, true, true, false],
    },
    {
      startTime: '6:00',
      endTime: '10:00',
      days: [true, false, false, false, false, true, true],
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

  private mockProfiles: SecurityProfile[] = [
    {
      fullname: 'Admin',
      interface: 'lan',
      vlan_tag: 1,
      gateway_ip: '192.168.1.1',
      outbound: 'wan',
      lan_access: 'ALL',
      wan_access: 'ALL',
      access_to_new_profiles: true,
      owns_lan: true,
      dns_source: 'system',
    },
    {
      fullname: 'Guest',
      interface: 'guest',
      vlan_tag: 100,
      gateway_ip: '192.168.2.1',
      outbound: 'wan',
      lan_access: 'SAME_PROFILE',
      wan_access: { whitelist: ['1.1.1.1', '8.8.8.8', '9.9.9.9'] },
      access_to_new_profiles: false,
      owns_lan: false,
      dns_override: [
        { address: '1.1.1.1', ssl: true },
        { address: '8.8.8.8', ssl: false },
      ],
      dns_source: 'custom',
    },
    {
      fullname: 'IoT',
      interface: 'iot',
      vlan_tag: 101,
      gateway_ip: '192.168.3.1',
      outbound: 'wan',
      lan_access: 'SAME_PROFILE',
      wan_access: { blacklist: ['192.0.2.0/24', '198.51.100.0/24'] },
      access_to_new_profiles: false,
      owns_lan: false,
      dns_source: 'system',
    },
  ]

  async profilesList(): Promise<ProfileId[]> {
    await pauseFor(250)
    return this.mockProfiles.map(p => ({
      fullname: p.fullname,
      interface: p.interface,
      vlan_tag: p.vlan_tag,
    }))
  }

  async profileGet(params: ProfileIdOpt): Promise<SecurityProfile> {
    await pauseFor(250)
    const profile = this.mockProfiles.find(
      p =>
        (!params.fullname || p.fullname === params.fullname) &&
        (!params.interface || p.interface === params.interface) &&
        (params.vlan_tag === undefined || p.vlan_tag === params.vlan_tag),
    )
    if (!profile) {
      throw new Error('Profile not found')
    }
    return structuredClone(profile)
  }

  async profileCreate(params: ProfileCreateInput): Promise<ProfileId> {
    await pauseFor(250)

    // Generate interface name from fullname (first 5 chars, lowercase)
    const interface_name =
      params.interface ||
      params.fullname
        ?.toLowerCase()
        .replace(/[^a-z0-9]/g, '')
        .slice(0, 5) ||
      'prof'

    // Auto-assign vlan_tag if not provided
    const existing_tags = this.mockProfiles.map(p => p.vlan_tag)
    const vlan_tag =
      params.vlan_tag ||
      Array.from({ length: 3995 }, (_, i) => i + 101).find(
        t => !existing_tags.includes(t),
      ) ||
      101

    const newProfile: SecurityProfile = {
      fullname: params.fullname || 'Untitled',
      interface: interface_name,
      vlan_tag,
      gateway_ip: params.gateway_ip,
      outbound: params.outbound,
      lan_access: params.lan_access as any, // Cast needed for ProfileIdOpt -> ProfileId
      wan_access: params.wan_access,
      access_to_new_profiles: params.access_to_new_profiles,
      owns_lan: params.owns_lan,
      dns_source: params.dns_override?.length
        ? 'custom'
        : params.outbound !== 'wan'
          ? 'vpn'
          : 'system',
    }

    this.mockProfiles = [...this.mockProfiles, newProfile]

    return {
      fullname: newProfile.fullname,
      interface: newProfile.interface,
      vlan_tag: newProfile.vlan_tag,
    }
  }

  async profileUpdate(params: ProfileUpdateInput): Promise<ProfileId> {
    await pauseFor(250)

    this.mockProfiles = this.mockProfiles.map(p =>
      p.interface === params.interface && p.vlan_tag === params.vlan_tag
        ? {
            ...p,
            fullname: params.fullname || p.fullname,
            gateway_ip: params.gateway_ip,
            outbound: params.outbound,
            lan_access: params.lan_access as any,
            wan_access: params.wan_access,
            access_to_new_profiles: params.access_to_new_profiles,
          }
        : p,
    )

    return {
      fullname: params.fullname || '',
      interface: params.interface,
      vlan_tag: params.vlan_tag,
    }
  }

  async profileDelete(params: ProfileIdOpt): Promise<null> {
    await pauseFor(250)

    this.mockProfiles = this.mockProfiles.filter(
      p =>
        !(
          (!params.fullname || p.fullname === params.fullname) &&
          (!params.interface || p.interface === params.interface) &&
          (params.vlan_tag === undefined || p.vlan_tag === params.vlan_tag)
        ),
    )
    return null
  }

  private mockSchedules: Record<string, ScheduleWindow[]> = {}

  async profileScheduleGet(params: {
    interface: string
  }): Promise<ScheduleWindow[]> {
    await pauseFor(100)
    return this.mockSchedules[params.interface] || []
  }

  async profileScheduleSet(params: {
    interface: string
    windows: ScheduleWindow[]
  }): Promise<null> {
    await pauseFor(250)
    this.mockSchedules[params.interface] = params.windows
    return null
  }

  private mockInitialized = true

  async checkInitialized(): Promise<CheckInitializedRes> {
    await pauseFor(250)
    return { initialized: this.mockInitialized }
  }

  async setInitialPassword(params: SetInitialPasswordReq): Promise<null> {
    await pauseFor(250)
    this.mockInitialized = true
    return null
  }

  async setupStatus(): Promise<SetupStatusRes> {
    await pauseFor(250)
    return { setupMode: false, disk: { emmcFound: true, hasFirmware: true } }
  }

  async systemFactoryReset(): Promise<null> {
    await pauseFor(2000)
    return null
  }

  async systemLogs(): Promise<LogsResponse> {
    await pauseFor(250)
    const messages = [
      'daemon.info dnsmasq[1]: read /etc/hosts - 3 names',
      'kern.info kernel: [12345.678] br-lan: port 1(eth0) entered forwarding state',
      'daemon.notice odhcpd[1]: Got a DHCPv6 request',
      'daemon.info dnsmasq-dhcp[1]: DHCPACK(br-lan) 192.168.1.100 aa:bb:cc:dd:ee:ff laptop',
      'daemon.info hostapd: wlan0: STA aa:bb:cc:dd:ee:ff IEEE 802.11: authenticated',
      'daemon.info dnsmasq[1]: query[A] example.com from 192.168.1.100',
      'daemon.info dnsmasq[1]: forwarded example.com to 1.1.1.1',
      'daemon.info dnsmasq[1]: reply example.com is 93.184.216.34',
      'kern.warn kernel: [12400.123] nf_conntrack: table full, dropping packet',
      'daemon.notice netifd: Interface "wan" is now up',
    ]
    const entries: LogEntry[] = []
    for (let i = 0; i < 50; i++) {
      const hour = String(8 + Math.floor(i / 6)).padStart(2, '0')
      const min = String((i * 10) % 60).padStart(2, '0')
      entries.push({
        timestamp: `Mon Mar  3 ${hour}:${min}:00 2026`,
        message: messages[i % messages.length],
      })
    }
    return { entries }
  }

  // --- Device smart endpoint mocks ---

  private mockDeviceHosts = structuredClone(mockDhcpHosts)
  private mockDeviceBlocked = structuredClone(mockBlockedDevices)

  async devicesList(): Promise<DeviceFromApi[]> {
    await pauseFor(250)

    const wanIpv6 = this.isWanIpv6Enabled()
    const lanIpv6 = this.isLanIpv6Enabled()
    const arpOutput = getMockArpOutput(wanIpv6, lanIpv6)

    // Parse ARP entries
    const arpByMac = new Map<
      string,
      { ip: string; iface: string; state: string }[]
    >()
    for (const line of arpOutput.split('\n')) {
      const match = line.match(
        /^(\S+)\s+dev\s+(\S+)\s+lladdr\s+([0-9a-fA-F:]+)\s+(\S+)/,
      )
      if (match && match[2].startsWith('br-lan')) {
        const mac = match[3].toUpperCase()
        if (!arpByMac.has(mac)) arpByMac.set(mac, [])
        arpByMac
          .get(mac)!
          .push({ ip: match[1], iface: match[2], state: match[4] })
      }
    }

    // Parse DHCP leases
    const leaseByMac = new Map<string, { ip: string; hostname: string }>()
    for (const line of mockDhcpLeasesOutput.split('\n')) {
      const parts = line.split(/\s+/)
      if (parts.length >= 4) {
        leaseByMac.set(parts[1].toUpperCase(), {
          ip: parts[2],
          hostname: parts[3],
        })
      }
    }

    // Build host map
    const hostByMac = new Map(
      this.mockDeviceHosts.map(h => [h.options.mac!.toUpperCase(), h]),
    )

    // Blocked MACs
    const blockedMacs = new Set(
      this.mockDeviceBlocked
        .filter(
          r => r.options.target === 'REJECT' || r.options.target === 'DROP',
        )
        .map(r => r.options.src_mac!.toUpperCase()),
    )

    // All MACs
    const allMacs = new Set([
      ...arpByMac.keys(),
      ...leaseByMac.keys(),
      ...hostByMac.keys(),
      ...blockedMacs,
    ])

    const devices: DeviceFromApi[] = []
    for (const mac of allMacs) {
      const arpList = arpByMac.get(mac) || []
      const lease = leaseByMac.get(mac)
      const host = hostByMac.get(mac)
      const isBlocked = blockedMacs.has(mac)

      const status: DeviceFromApi['status'] = isBlocked
        ? 'blocked'
        : arpList.some(e => e.state === 'REACHABLE' || e.state === 'STALE')
          ? 'online'
          : 'offline'

      const ipv4 =
        arpList.find(e => !e.ip.includes(':'))?.ip || lease?.ip || null
      const ipv6 =
        arpList.find(e => e.ip.includes(':') && !e.ip.startsWith('fe80:'))
          ?.ip ||
        arpList.find(e => e.ip.startsWith('fe80:'))?.ip ||
        null

      devices.push({
        mac,
        name: host?.options.name || null,
        hostname: lease?.hostname || null,
        status,
        connection:
          status === 'online' ? (mockConnectionTypes[mac] ?? 'Ethernet') : null,
        ipv4,
        ipv6,
        ipv4_static: !!host?.options.ip,
        ipv6_static: !!host?.options.hostid,
        security_profile: 'Admin',
        speed: status === 'online' ? (mockDeviceSpeeds[mac] ?? null) : null,
        data_usage: mockDataUsageTotals[mac] ?? null,
      })
    }
    return devices
  }

  async devicesUpdate(params: DeviceUpdateReq): Promise<null> {
    await pauseFor(250)
    const macUpper = params.mac.toUpperCase()
    const existing = this.mockDeviceHosts.find(
      h => h.options.mac?.toUpperCase() === macUpper,
    )
    if (existing) {
      existing.options.name = params.name
      existing.options.ip = params.ipv4_static ? params.ipv4 : undefined
      existing.options.hostid = params.ipv6_static
        ? params.ipv6.split(':').slice(-4).join(':')
        : undefined
    } else {
      this.mockDeviceHosts.push({
        type: 'host',
        name: `host_${params.mac.replace(/:/g, '').toLowerCase()}`,
        options: {
          mac: params.mac,
          name: params.name,
          ip: params.ipv4_static ? params.ipv4 : undefined,
          hostid: params.ipv6_static
            ? params.ipv6.split(':').slice(-4).join(':')
            : undefined,
          dns: '1',
        },
        lists: {},
      })
    }
    return null
  }

  async devicesBlock(params: { mac: string }): Promise<null> {
    await pauseFor(250)
    const macUpper = params.mac.toUpperCase()
    if (
      !this.mockDeviceBlocked.some(
        r => r.options.src_mac?.toUpperCase() === macUpper,
      )
    ) {
      this.mockDeviceBlocked.push({
        type: 'rule',
        name: `block_${params.mac.replace(/:/g, '').toLowerCase()}`,
        options: {
          src: 'lan',
          dest: 'wan',
          src_mac: params.mac,
          target: 'REJECT',
          enabled: '1',
          name: `Block ${params.mac}`,
        },
        lists: {},
      })
    }
    // Remove static IPs
    const host = this.mockDeviceHosts.find(
      h => h.options.mac?.toUpperCase() === macUpper,
    )
    if (host) {
      delete host.options.ip
      delete host.options.hostid
    }
    return null
  }

  async devicesUnblock(params: { mac: string }): Promise<null> {
    await pauseFor(250)
    const macUpper = params.mac.toUpperCase()
    this.mockDeviceBlocked = this.mockDeviceBlocked.filter(
      r =>
        !(
          (r.options.target === 'REJECT' || r.options.target === 'DROP') &&
          r.options.src_mac?.toUpperCase() === macUpper
        ),
    )
    return null
  }

  async devicesForget(params: { mac: string }): Promise<null> {
    await pauseFor(250)
    const macUpper = params.mac.toUpperCase()
    this.mockDeviceHosts = this.mockDeviceHosts.filter(
      h => h.options.mac?.toUpperCase() !== macUpper,
    )
    this.mockDeviceBlocked = this.mockDeviceBlocked.filter(
      r =>
        !(
          (r.options.target === 'REJECT' || r.options.target === 'DROP') &&
          r.options.src_mac?.toUpperCase() === macUpper
        ),
    )
    return null
  }

  async devicesDataUsage(
    params: DeviceDataUsageReq,
  ): Promise<DataUsagePointFromApi[]> {
    await pauseFor(250)
    return generateMockDataUsage(params.mac, params.period)
  }

  private mockLanIpv4: LanIpv4Response = {
    address: '192.168.0.1',
    netmask: '255.255.0.0',
  }

  async lanIpv4Get(): Promise<LanIpv4Response> {
    await pauseFor(250)
    return structuredClone(this.mockLanIpv4)
  }

  async lanIpv4Set(params: LanIpv4SetRequest): Promise<null> {
    await pauseFor(250)
    this.mockLanIpv4 = { ...this.mockLanIpv4, address: params.address }
    return null
  }

  private mockLanIpv6: LanIpv6Response = {
    slaac: true,
    dhcpv6: true,
    prefix: 64,
    ip6addr: 'fd00::1',
    wan_prefix: 48,
  }

  async lanIpv6Get(): Promise<LanIpv6Response> {
    await pauseFor(250)
    return structuredClone(this.mockLanIpv6)
  }

  async lanIpv6Set(params: LanIpv6SetRequest): Promise<null> {
    await pauseFor(250)
    this.mockLanIpv6 = {
      ...this.mockLanIpv6,
      slaac: params.slaac,
      dhcpv6: params.dhcpv6,
      prefix: params.prefix,
    }
    return null
  }

  // --- WAN smart endpoint mocks ---

  private mockWanIpv4: WanIpv4Response = {
    mode: 'dhcp',
    assigned_ip: '203.0.113.45',
    address: null,
    netmask: null,
    gateway: null,
    username: null,
    password: null,
    device: 'eth1',
  }

  async wanIpv4Get(): Promise<WanIpv4Response> {
    await pauseFor(250)
    return structuredClone(this.mockWanIpv4)
  }

  async wanIpv4Set(params: WanIpv4SetRequest): Promise<null> {
    await pauseFor(250)
    this.mockWanIpv4 = {
      ...this.mockWanIpv4,
      mode: params.mode,
      address: params.address ?? null,
      netmask: params.netmask ?? null,
      gateway: params.gateway ?? null,
      username: params.username ?? null,
      password: params.password ?? null,
      device: params.device ?? this.mockWanIpv4.device,
    }
    return null
  }

  private mockWanIpv6: WanIpv6Response = {
    mode: 'slaac',
    address: null,
    prefix: null,
    gateway: null,
    ip6prefix: null,
    ip6prefixlen: null,
    ip4prefixlen: null,
    assigned_ipv6: '2001:db8::1',
    border_relay: null,
  }

  async wanIpv6Get(): Promise<WanIpv6Response> {
    await pauseFor(250)
    return structuredClone(this.mockWanIpv6)
  }

  async wanIpv6Set(params: WanIpv6SetRequest): Promise<null> {
    await pauseFor(250)
    this.mockWanIpv6 = {
      mode: params.mode,
      address: params.address ?? null,
      prefix: params.prefix ?? null,
      gateway: params.gateway ?? null,
      ip6prefix: params.ip6prefix ?? null,
      ip6prefixlen: params.ip6prefixlen ?? null,
      ip4prefixlen: params.ip4prefixlen ?? null,
      border_relay: params.border_relay ?? null,
    }
    return null
  }

  private mockWanMac: WanMacResponse = {
    strategy: 'router',
    mac: 'AA:BB:CC:DD:EE:01',
    default_mac: 'AA:BB:CC:DD:EE:01',
  }

  async wanMacGet(): Promise<WanMacResponse> {
    await pauseFor(250)
    return structuredClone(this.mockWanMac)
  }

  async wanMacSet(params: WanMacSetRequest): Promise<null> {
    await pauseFor(250)
    this.mockWanMac = {
      ...this.mockWanMac,
      strategy: params.strategy,
      mac:
        params.strategy === 'custom'
          ? (params.mac ?? this.mockWanMac.mac)
          : this.mockWanMac.default_mac,
    }
    return null
  }

  private mockWanDns: WanDnsResponse = {
    mode: 'isp',
    servers: [],
  }

  async wanDnsGet(): Promise<WanDnsResponse> {
    await pauseFor(250)
    return structuredClone(this.mockWanDns)
  }

  async wanDnsSet(params: WanDnsSetRequest): Promise<null> {
    await pauseFor(250)
    this.mockWanDns = {
      mode: params.mode,
      servers: params.servers ?? [],
    }
    return null
  }

  private mockWanDdns: WanDdnsResponse = {
    enabled: true,
    provider: 'dyndns',
    hostname: 'myhost.dyndns.org',
    username: 'myuser',
    password: 'mypass',
    token: null,
    zone: null,
  }

  async wanDdnsGet(): Promise<WanDdnsResponse> {
    await pauseFor(250)
    return structuredClone(this.mockWanDdns)
  }

  async wanDdnsSet(params: WanDdnsSetRequest): Promise<null> {
    await pauseFor(250)
    this.mockWanDdns = {
      enabled: params.enabled,
      provider: params.provider,
      hostname: params.hostname ?? null,
      username: params.username ?? null,
      password: params.password ?? null,
      token: params.token ?? null,
      zone: params.zone ?? null,
    }
    return null
  }

  // --- Published Ports smart endpoint mocks ---

  private mockPublishedPorts: PublishedPortFromApi[] = [
    {
      id: 'home_assistant',
      enabled: true,
      label: 'Home Assistant',
      device_mac: '00:1A:2B:3C:4D:5E',
      ports: '8123',
      protocol: 'tcp',
      ipv4: true,
      ipv6: true,
      ipv4_public_port: null,
      source: 'any',
      status: 'active',
      status_reason: null,
      device_name: 'Home Server',
      device_ipv4: '192.168.1.100',
      device_ipv6: '2001:db8:abcd:1::100',
    },
    {
      id: 'minecraft',
      enabled: true,
      label: 'Minecraft Server',
      device_mac: '00:1A:2B:3C:4D:5F',
      ports: '25565',
      protocol: 'tcp+udp',
      ipv4: true,
      ipv6: false,
      ipv4_public_port: null,
      source: 'any',
      status: 'active',
      status_reason: null,
      device_name: 'Gaming PC',
      device_ipv4: '192.168.1.101',
      device_ipv6: null,
    },
    {
      id: 'ssh_access',
      enabled: false,
      label: 'SSH Access',
      device_mac: 'DE:AD:BE:EF:CA:FF',
      ports: '22',
      protocol: 'tcp',
      ipv4: true,
      ipv6: true,
      ipv4_public_port: '2222',
      source: '203.0.113.0/24',
      status: 'disabled',
      status_reason: null,
      device_name: null,
      device_ipv4: '192.168.1.103',
      device_ipv6: '2001:db8:abcd:1::103',
    },
  ]

  async publishedPortsList(): Promise<PublishedPortFromApi[]> {
    await pauseFor(250)
    return structuredClone(this.mockPublishedPorts)
  }

  async publishedPortsSet(params: PublishedPortsSetRequest): Promise<null> {
    await pauseFor(250)
    // Update the mock data — enrich inputs with existing display data
    this.mockPublishedPorts = params.ports.map(input => {
      const existing = this.mockPublishedPorts.find(p => p.id === input.id)
      return {
        ...input,
        ipv4_public_port: input.ipv4_public_port ?? null,
        status: input.enabled
          ? (existing?.status ?? 'active')
          : ('disabled' as const),
        status_reason: input.enabled ? null : null,
        device_name: existing?.device_name ?? null,
        device_ipv4: existing?.device_ipv4 ?? null,
        device_ipv6: existing?.device_ipv6 ?? null,
      }
    })
    return null
  }

  // --- Outbound VPN (WireGuard Client) smart endpoint mocks ---

  private mockVpnClients: OutboundVpn[] = [
    {
      id: 'wg_proton',
      label: 'Proton',
      target: 'Internet',
      enabled: true,
      used_by: [],
    },
    {
      id: 'wg_mullvad',
      label: 'Mullvad',
      target: 'Proton',
      enabled: true,
      used_by: [],
    },
  ]

  async vpnClientList(): Promise<OutboundVpn[]> {
    await pauseFor(250)
    return structuredClone(this.mockVpnClients)
  }

  async vpnClientCreate(
    params: OutboundVpnCreateRequest,
  ): Promise<OutboundVpnCreateResponse> {
    await pauseFor(250)
    const id = `wg_${params.label.toLowerCase().replace(/[^a-z0-9]/g, '_')}`
    this.mockVpnClients = [
      ...this.mockVpnClients,
      {
        id,
        label: params.label,
        target: params.target,
        enabled: true,
        used_by: [],
      },
    ]
    return { id }
  }

  async vpnClientUpdate(params: OutboundVpnUpdateRequest): Promise<null> {
    await pauseFor(250)
    this.mockVpnClients = this.mockVpnClients.map(c =>
      c.id === params.id
        ? { ...c, label: params.label, target: params.target }
        : c,
    )
    return null
  }

  async vpnClientDelete(params: OutboundVpnDeleteRequest): Promise<null> {
    await pauseFor(250)
    this.mockVpnClients = this.mockVpnClients.filter(c => c.id !== params.id)
    return null
  }

  async vpnClientSetEnabled(
    params: OutboundVpnSetEnabledRequest,
  ): Promise<null> {
    await pauseFor(250)
    this.mockVpnClients = this.mockVpnClients.map(c =>
      c.id === params.id ? { ...c, enabled: params.enabled } : c,
    )
    return null
  }

  // --- Ethernet smart endpoint mocks ---

  private mockEthernet: EthernetConfig = {
    wan_ipv6: true,
    wan_port: 'eth0',
    ports: {
      eth0: { profile: null },
      eth1: {
        profile: { fullname: 'Admin', interface: 'lan', vlan_tag: 1 },
      },
      eth2: {
        profile: { fullname: 'Admin', interface: 'lan', vlan_tag: 1 },
      },
      eth3: {
        profile: { fullname: 'Guest', interface: 'guest', vlan_tag: 100 },
      },
    },
  }

  async ethernetGet(): Promise<EthernetConfig> {
    await pauseFor(250)
    return structuredClone(this.mockEthernet)
  }

  async ethernetSet(params: EthernetSetConfig): Promise<null> {
    await pauseFor(250)
    this.mockEthernet = {
      wan_ipv6: params.wan_ipv6,
      wan_port: params.wan_port,
      ports: Object.fromEntries(
        Object.entries(params.ports).map(([name, port]) => [
          name,
          {
            profile: port.profile
              ? (this.mockProfiles.find(
                  p =>
                    (port.profile!.fullname === undefined ||
                      p.fullname === port.profile!.fullname) &&
                    (port.profile!.interface === undefined ||
                      p.interface === port.profile!.interface) &&
                    (port.profile!.vlan_tag === undefined ||
                      p.vlan_tag === port.profile!.vlan_tag),
                ) ?? null)
              : null,
          },
        ]),
      ),
    }
    return null
  }

  // --- SSH Keys smart endpoint mocks ---

  private mockSshKeys: SshKeyFromApi[] = [
    {
      algorithm: 'ssh-ed25519',
      fingerprint: 'ab:cd:ef:01:23:45:67:89:ab:cd:ef:01:23:45:67:89',
      hostname: 'matt@macbook',
    },
    {
      algorithm: 'ssh-ed25519',
      fingerprint: '12:34:56:78:9a:bc:de:f0:12:34:56:78:9a:bc:de:f0',
      hostname: 'lucy@desktop',
    },
  ]

  async sshKeysList(): Promise<SshKeyFromApi[]> {
    await pauseFor(250)
    return structuredClone(this.mockSshKeys)
  }

  async sshKeysAdd(params: SshKeysAddRequest): Promise<SshKeyFromApi> {
    await pauseFor(250)
    const parts = params.key.trim().split(/\s+/)
    const newKey: SshKeyFromApi = {
      algorithm: parts[0] || 'ssh-ed25519',
      fingerprint: Array.from({ length: 16 }, () =>
        Math.floor(Math.random() * 256)
          .toString(16)
          .padStart(2, '0'),
      ).join(':'),
      hostname: parts.slice(2).join(' ') || '',
    }
    this.mockSshKeys = [...this.mockSshKeys, newKey]
    return newKey
  }

  async sshKeysDelete(params: SshKeysDeleteRequest): Promise<null> {
    await pauseFor(250)
    this.mockSshKeys = this.mockSshKeys.filter(
      k => k.fingerprint !== params.fingerprint,
    )
    return null
  }

  private mockActivity: ActivityEntry[] = Array.from(
    { length: 25 },
    (_, i) => ({
      id: i,
      timestamp: new Date(Date.now() - i * 1000 * 60 * 60 * 24).toISOString(),
      category: ['profile', 'wifi', 'vpn-client', 'device', 'auth'][i % 5],
      action: ['created', 'updated', 'deleted', 'enabled', 'login'][i % 5],
      success: i !== 3,
      summary: [
        "Created profile 'Guest'",
        "Updated WiFi settings (SSID: 'MyNetwork')",
        "Deleted outbound VPN 'Mullvad US'",
        "Failed to enable outbound VPN 'NordVPN'",
        'Logged in',
      ][i % 5],
      error: i === 3 ? 'VpnChainCycle: would create a loop' : null,
    }),
  )

  async activityList(
    params: ActivityListParams = {},
  ): Promise<ActivityListResponse> {
    await pauseFor(100)
    const offset = params.offset ?? 0
    const limit = params.limit ?? 50
    return {
      entries: this.mockActivity.slice(offset, offset + limit),
      total: this.mockActivity.length,
    }
  }

  async activityDelete(params: { id: number }): Promise<null> {
    await pauseFor(100)
    this.mockActivity = this.mockActivity.filter(e => e.id !== params.id)
    return null
  }

  async activityClear(): Promise<null> {
    await pauseFor(100)
    this.mockActivity = []
    return null
  }

  async backupCreate(): Promise<BackupCreateRes> {
    await pauseFor(500)
    return {
      guid: 'mock-backup-guid',
      filename: 'backup-startwrt-2026-03-19.tar.gz',
    }
  }

  async backupRestore(): Promise<BackupRestoreRes> {
    await pauseFor(250)
    return { upload: 'mock-restore-guid' }
  }

  async diagnosticsCreate(): Promise<DiagnosticsCreateRes> {
    await pauseFor(500)
    return {
      guid: 'mock-diag-guid',
      filename: 'diagnostics-startwrt-2026-03-19.log',
    }
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
      {
        type: 'interface',
        name: 'wan',
        options: {
          proto: 'dhcp',
          device: 'eth0.2',
          peerdns: '1',
        },
        lists: { dns: [] },
      },
      {
        type: 'interface',
        name: 'wan6',
        options: {
          proto: 'dhcpv6',
          reqaddress: 'try',
          reqprefix: 'auto',
          device: '@wan',
        },
        lists: {},
      },
      {
        type: 'device',
        name: null,
        options: {
          name: 'eth0.2',
          macaddr: 'AA:BB:CC:DD:EE:01',
        },
        lists: {},
      },
      {
        type: 'interface',
        name: 'lan',
        options: {
          proto: 'static',
          device: 'br-lan',
          ipaddr: '192.168.0.1',
          netmask: '255.255.0.0',
          ip6assign: '64',
          ip6addr: 'fd00::1/64',
        },
        lists: {},
      },
      {
        type: 'device',
        name: 'br_lan',
        options: {
          name: 'br-lan',
          type: 'bridge',
        },
        lists: {
          ports: ['eth1', 'eth2', 'eth3'],
        },
      },
      {
        type: 'device',
        name: 'wan_eth0',
        options: {
          name: 'eth0',
        },
        lists: {},
      },
    ],
    modified: new Date().toISOString(),
  },
  ddns: {
    sections: [
      {
        type: 'service',
        name: 'wan',
        options: {
          enabled: '1',
          service_name: 'dyndns.org',
          username: 'myuser',
          password: 'mypass',
          domain: 'myhost.dyndns.org',
          lookup_host: 'myhost.dyndns.org',
          ip_source: 'network',
          ip_network: 'wan',
        },
        lists: {},
      },
    ],
    modified: new Date().toISOString(),
  },
  dhcp: {
    sections: [dhcpLanSlaacDhcpv6, ...mockDhcpHosts],
    modified: new Date().toISOString(),
  },
  firewall: {
    sections: [...mockBlockedDevices],
    modified: new Date().toISOString(),
  },
}
