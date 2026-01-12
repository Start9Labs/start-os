import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  DhcpHostSection,
  FirewallRuleSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'
import {
  DataUsagePeriod,
  DataUsagePoint,
  Device,
  DeviceStatus,
  DeviceUpdateData,
} from '../utils'
import {
  mockConnectionTypes,
  mockDataUsageTotals,
  mockDeviceSpeeds,
} from './mocks'

type UciFiles = {
  dhcp: UciFile<UciSection>
  firewall: UciFile<UciSection>
}

interface ArpEntry {
  ip: string
  mac: string
  interface: string
  state: string
}

interface DhcpLease {
  expiry: number
  mac: string
  ip: string
  hostname: string
}

@Injectable({
  providedIn: 'root',
})
export class DevicesUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<Device[]> {
    // Fetch UCI config files
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['dhcp', 'firewall'],
    })

    // Get current ARP table (online devices)
    const arpEntries = await this.getArpTable()

    // Get DHCP leases (all known devices)
    const dhcpLeases = await this.getDhcpLeases()

    // Get known hosts from DHCP config (custom names, static IPs)
    const dhcpHosts = this._uciFiles.dhcp.sections.filter(
      (s): s is DhcpHostSection => s.type === 'host',
    )

    // Get blocked MACs from firewall rules
    const blockedMacs = this.getBlockedMacs(this._uciFiles.firewall.sections)

    // Build device list
    const devices = this.buildDeviceList(
      arpEntries,
      dhcpLeases,
      dhcpHosts,
      blockedMacs,
    )

    return devices
  }

  async update(mac: string, data: DeviceUpdateData): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles
    const macUpper = mac.toUpperCase()

    // Find or create host section for this MAC
    let hostSection = uciFiles.dhcp.sections.find(
      (s): s is DhcpHostSection =>
        s.type === 'host' && s.options.mac?.toUpperCase() === macUpper,
    )

    if (!hostSection) {
      // Create new host section
      const hostId = `host_${mac.replace(/:/g, '').toLowerCase()}`
      hostSection = {
        type: 'host',
        name: hostId,
        options: {
          mac: mac,
          dns: '1',
        },
        lists: {},
      }
      uciFiles.dhcp.sections.push(hostSection)
    }

    // Update name
    if (data.name) {
      hostSection.options.name = data.name
    }

    // Update static IPv4
    if (data.ipv4Static && data.ipv4) {
      hostSection.options.ip = data.ipv4
    } else {
      delete hostSection.options.ip
    }

    // Update static IPv6 (using hostid for SLAAC suffix)
    if (data.ipv6Static && data.ipv6) {
      // Extract the interface identifier from the IPv6 address
      // For simplicity, store the full address - in real implementation
      // we'd extract just the host portion
      hostSection.options.hostid = this.extractIPv6HostId(data.ipv6)
    } else {
      delete hostSection.options.hostid
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Update local cache
    this._uciFiles = uciFiles

    // Restart dnsmasq to apply changes
    await this.api.exec({
      command: '/etc/init.d/dnsmasq',
      args: ['restart'],
      timeout: 10000,
    })
  }

  async block(mac: string): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles
    const macUpper = mac.toUpperCase()

    // Check if already blocked
    const existingRule = uciFiles.firewall.sections.find(
      (s): s is FirewallRuleSection =>
        this.isBlockRule(s) && this.ruleBlocksMac(s, macUpper),
    )

    if (!existingRule) {
      // Create new firewall rule to block the MAC
      const ruleId = `block_${mac.replace(/:/g, '').toLowerCase()}`
      const blockRule: FirewallRuleSection = {
        type: 'rule',
        name: ruleId,
        options: {
          src: 'lan',
          dest: 'wan',
          src_mac: mac,
          target: 'REJECT',
          enabled: '1',
          name: `Block ${mac}`,
        },
        lists: {},
      }
      uciFiles.firewall.sections.push(blockRule)
    }

    // Remove static IP settings from DHCP host but keep custom name
    const hostSection = uciFiles.dhcp.sections.find(
      (s): s is DhcpHostSection =>
        s.type === 'host' && s.options.mac?.toUpperCase() === macUpper,
    )

    if (hostSection) {
      // Remove static IP settings
      delete hostSection.options.ip
      delete hostSection.options.hostid
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Update local cache
    this._uciFiles = uciFiles

    // Restart firewall and dnsmasq to apply changes
    await this.api.exec({
      command: '/etc/init.d/firewall',
      args: ['restart'],
      timeout: 10000,
    })

    await this.api.exec({
      command: '/etc/init.d/dnsmasq',
      args: ['restart'],
      timeout: 10000,
    })
  }

  async unblock(mac: string): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles
    const macUpper = mac.toUpperCase()

    // Remove firewall rule(s) that block this MAC
    uciFiles.firewall.sections = uciFiles.firewall.sections.filter(s => {
      if (!this.isBlockRule(s)) return true
      return !this.ruleBlocksMac(s, macUpper)
    })

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Update local cache
    this._uciFiles = uciFiles

    // Restart firewall to apply changes
    await this.api.exec({
      command: '/etc/init.d/firewall',
      args: ['restart'],
      timeout: 10000,
    })
  }

  async forget(mac: string): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles
    const macUpper = mac.toUpperCase()

    // Remove DHCP host entry for this MAC
    uciFiles.dhcp.sections = uciFiles.dhcp.sections.filter(s => {
      if (s.type !== 'host') return true
      return (s as DhcpHostSection).options.mac?.toUpperCase() !== macUpper
    })

    // Remove any firewall block rules for this MAC
    uciFiles.firewall.sections = uciFiles.firewall.sections.filter(s => {
      if (!this.isBlockRule(s)) return true
      return !this.ruleBlocksMac(s, macUpper)
    })

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Update local cache
    this._uciFiles = uciFiles

    // Restart services to apply changes
    await this.api.exec({
      command: '/etc/init.d/firewall',
      args: ['restart'],
      timeout: 10000,
    })

    await this.api.exec({
      command: '/etc/init.d/dnsmasq',
      args: ['restart'],
      timeout: 10000,
    })
  }

  private ruleBlocksMac(rule: FirewallRuleSection, mac: string): boolean {
    // Check single MAC in options
    if (rule.options.src_mac?.toUpperCase() === mac) {
      return true
    }
    // Check MAC list
    if (rule.lists.src_mac?.some(m => m.toUpperCase() === mac)) {
      return true
    }
    return false
  }

  private extractIPv6HostId(ipv6: string): string {
    // Extract the last 64 bits (interface identifier) from IPv6
    // This is a simplified implementation
    const parts = ipv6.split(':')
    if (parts.length >= 4) {
      return parts.slice(-4).join(':')
    }
    return ipv6
  }

  private async getArpTable(): Promise<ArpEntry[]> {
    const result = await this.api.exec({
      command: 'ip',
      args: ['neigh', 'show'],
      timeout: 5000,
    })

    if (result.exitCode !== 0) {
      console.error('Failed to get ARP table:', result.stderr)
      return []
    }

    return this.parseArpOutput(result.stdout)
  }

  private async getDhcpLeases(): Promise<DhcpLease[]> {
    const result = await this.api.exec({
      command: 'cat',
      args: ['/tmp/dhcp.leases'],
      timeout: 5000,
    })

    if (result.exitCode !== 0) {
      // File might not exist if no leases
      return []
    }

    return this.parseDhcpLeases(result.stdout)
  }

  private parseArpOutput(stdout: string): ArpEntry[] {
    const entries: ArpEntry[] = []
    const lines = stdout.trim().split('\n')

    for (const line of lines) {
      if (!line.trim()) continue

      // Format: IP dev INTERFACE lladdr MAC STATE
      // Example: 192.168.1.100 dev br-lan lladdr 00:1a:2b:3c:4d:5e REACHABLE
      const match = line.match(
        /^(\S+)\s+dev\s+(\S+)\s+lladdr\s+([0-9a-fA-F:]+)\s+(\S+)/,
      )
      if (match) {
        entries.push({
          ip: match[1],
          interface: match[2],
          mac: match[3].toUpperCase(),
          state: match[4],
        })
      }
    }

    return entries
  }

  private parseDhcpLeases(stdout: string): DhcpLease[] {
    const leases: DhcpLease[] = []
    const lines = stdout.trim().split('\n')

    for (const line of lines) {
      if (!line.trim()) continue

      // Format: expiry_timestamp mac_address ip_address hostname client_id
      const parts = line.split(/\s+/)
      if (parts.length >= 4) {
        leases.push({
          expiry: parseInt(parts[0], 10),
          mac: parts[1].toUpperCase(),
          ip: parts[2],
          hostname: parts[3],
        })
      }
    }

    return leases
  }

  private getBlockedMacs(sections: UciSection[]): Set<string> {
    const blocked = new Set<string>()

    for (const section of sections) {
      if (!this.isBlockRule(section)) continue
      if (section.options.enabled === '0') continue

      // Check single MAC in options
      if (section.options.src_mac) {
        blocked.add(section.options.src_mac.toUpperCase())
      }

      // Check MAC list
      if (section.lists.src_mac) {
        for (const mac of section.lists.src_mac) {
          blocked.add(mac.toUpperCase())
        }
      }
    }

    return blocked
  }

  private buildDeviceList(
    arpEntries: ArpEntry[],
    dhcpLeases: DhcpLease[],
    dhcpHosts: DhcpHostSection[],
    blockedMacs: Set<string>,
  ): Device[] {
    const deviceMap = new Map<string, Device>()

    // Create lookup maps
    const arpByMac = new Map<string, ArpEntry[]>()
    for (const entry of arpEntries) {
      const mac = entry.mac.toUpperCase()
      if (!arpByMac.has(mac)) {
        arpByMac.set(mac, [])
      }
      arpByMac.get(mac)!.push(entry)
    }

    const leaseByMac = new Map<string, DhcpLease>()
    for (const lease of dhcpLeases) {
      leaseByMac.set(lease.mac.toUpperCase(), lease)
    }

    const hostByMac = new Map<string, DhcpHostSection>()
    for (const host of dhcpHosts) {
      if (host.options.mac) {
        hostByMac.set(host.options.mac.toUpperCase(), host)
      }
    }

    // Process all known MACs from various sources
    const allMacs = new Set<string>([
      ...arpByMac.keys(),
      ...leaseByMac.keys(),
      ...hostByMac.keys(),
      ...blockedMacs,
    ])

    for (const mac of allMacs) {
      const arpList = arpByMac.get(mac) || []
      const lease = leaseByMac.get(mac)
      const host = hostByMac.get(mac)
      const isBlocked = blockedMacs.has(mac)

      // Determine status
      let status: DeviceStatus
      if (isBlocked) {
        status = 'blocked'
      } else if (
        arpList.some(e => e.state === 'REACHABLE' || e.state === 'STALE')
      ) {
        status = 'online'
      } else {
        status = 'offline'
      }

      // Get IPs from ARP entries
      const ipv4Entry = arpList.find(e => !e.ip.includes(':'))
      const ipv6Entry = arpList.find(e => e.ip.includes(':'))

      // Determine IPv6 address: prefer dynamic from ARP, fall back to static hostid
      let ipv6: string | undefined = ipv6Entry?.ip
      if (!ipv6 && host?.options.hostid) {
        // Format hostid as IPv6 suffix (it's the interface identifier)
        ipv6 = `::${host.options.hostid}`
      }

      // Determine name (prefer host config, then lease hostname, then generate from MAC)
      const name =
        host?.options.name || lease?.hostname || this.generateNameFromMac(mac)

      // Determine connection type from interface or mock
      const connection =
        mockConnectionTypes[mac] ||
        this.getConnectionType(arpList[0]?.interface)

      // Get speed data (only for online devices)
      const speed = status === 'online' ? mockDeviceSpeeds[mac] : undefined

      // Get data usage total
      const dataUsage = mockDataUsageTotals[mac]

      const device: Device = {
        mac,
        name,
        hostname: lease?.hostname || '',
        status,
        connection: status === 'online' ? connection : undefined,
        ipv4: ipv4Entry?.ip || lease?.ip,
        ipv6,
        ipv4Static: !!host?.options.ip,
        ipv6Static: !!host?.options.hostid,
        // Placeholder values - will be populated later
        securityProfile: 'Default',
        speed,
        dataUsage,
      }

      deviceMap.set(mac, device)
    }

    return Array.from(deviceMap.values())
  }

  private generateNameFromMac(mac: string): string {
    // Generate a readable name from the last 6 chars of MAC
    const suffix = mac.replace(/:/g, '').slice(-6).toLowerCase()
    return `device-${suffix}`
  }

  private getConnectionType(iface?: string): string | undefined {
    if (!iface) return undefined

    // Map interface names to connection types
    if (iface.startsWith('eth') || iface === 'br-lan') {
      return 'Ethernet'
    }
    if (iface.startsWith('wlan') || iface.includes('wifi')) {
      // In a real implementation, we'd check the specific radio
      return 'Wi-Fi'
    }
    return iface
  }

  private isBlockRule(section: UciSection): section is FirewallRuleSection {
    return (
      section.type === 'rule' &&
      'target' in section.options &&
      (section.options.target === 'REJECT' || section.options.target === 'DROP')
    )
  }

  /**
   * Get historical data usage for a device from nlbwmon
   */
  async getDataUsage(
    mac: string,
    period: DataUsagePeriod,
  ): Promise<DataUsagePoint[]> {
    // Calculate date range based on period
    const now = new Date()
    let startDate: Date

    switch (period) {
      case 'day':
        startDate = new Date(now.getTime() - 24 * 60 * 60 * 1000)
        break
      case 'week':
        startDate = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000)
        break
      case 'month':
        startDate = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000)
        break
      case '3months':
        startDate = new Date(now.getTime() - 90 * 24 * 60 * 60 * 1000)
        break
    }

    // Format date for nlbwmon query (YYYY-MM-DD)
    const formatDate = (d: Date) => d.toISOString().split('T')[0]

    try {
      // Query nlbwmon for the date range
      // nlbw -c json -g mac -t <start_date>
      const result = await this.api.exec({
        command: 'nlbw',
        args: ['-c', 'json', '-g', 'mac', '-t', formatDate(startDate)],
        timeout: 10000,
      })

      // Parse the JSON response
      const data = JSON.parse(result.stdout)

      // Find entries for this MAC and convert to our format
      return this.parseNlbwmonData(data, mac.toLowerCase(), period)
    } catch (error) {
      console.error('Failed to get data usage from nlbwmon:', error)
      return []
    }
  }

  private parseNlbwmonData(
    data: any,
    mac: string,
    period: DataUsagePeriod,
  ): DataUsagePoint[] {
    // nlbwmon returns data grouped by time period
    // Structure varies but generally:
    // { columns: [...], data: [[values], ...] }

    const points: DataUsagePoint[] = []

    if (!data?.data) return points

    // Find MAC column index and byte columns
    const columns = data.columns || []
    const macIndex = columns.indexOf('mac')
    const rxIndex = columns.indexOf('rx_bytes')
    const txIndex = columns.indexOf('tx_bytes')
    const timeIndex = columns.indexOf('interval_start') // or similar

    if (macIndex === -1 || rxIndex === -1 || txIndex === -1) {
      return points
    }

    // Filter and map data for this MAC
    for (const row of data.data) {
      if (row[macIndex]?.toLowerCase() === mac) {
        points.push({
          timestamp: row[timeIndex] || Date.now() / 1000,
          download: row[rxIndex] || 0,
          upload: row[txIndex] || 0,
        })
      }
    }

    return points
  }
}
