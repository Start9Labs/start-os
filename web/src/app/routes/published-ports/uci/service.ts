import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  FirewallRedirectSection,
  FirewallRuleSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'
import { Protocol, PublishedPort } from '../types'

type UciFiles = {
  firewall: UciFile<UciSection>
}

/**
 * Service for managing published ports via OpenWRT UCI firewall configuration.
 *
 * Published ports are stored as standard OpenWRT firewall sections:
 * - IPv4: `config redirect` with target DNAT
 * - IPv6: `config rule` with family ipv6 and target ACCEPT
 *
 * We use custom options (_pp_id, _pp_mac) to link related entries and
 * store device information. These are ignored by the firewall daemon.
 *
 * Section naming convention:
 * - pp_<uuid>: IPv4 redirect
 * - pp_<uuid>_v6: IPv6 rule
 */
@Injectable({
  providedIn: 'root',
})
export class PublishedPortsUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles
  private _cachedPorts?: PublishedPort[]

  async get(): Promise<PublishedPort[]> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['firewall'],
    })

    // Get all redirects (IPv4 port forwards)
    const redirects = this._uciFiles.firewall.sections.filter(
      (s): s is FirewallRedirectSection =>
        s.type === 'redirect' && s.options.target === 'DNAT',
    )

    // Get all IPv6 allow rules (IPv6 port forwards)
    const ipv6Rules = this._uciFiles.firewall.sections.filter(
      (s): s is FirewallRuleSection =>
        s.type === 'rule' &&
        s.options.family === 'ipv6' &&
        s.options.target === 'ACCEPT' &&
        s.options.src === 'wan' &&
        !!s.options.dest_port,
    )

    // Build a map of _pp_id -> sections for matching
    const redirectsByPpId = new Map<string, FirewallRedirectSection>()
    const rulesByPpId = new Map<string, FirewallRuleSection>()
    const unmatchedRedirects: FirewallRedirectSection[] = []
    const unmatchedRules: FirewallRuleSection[] = []

    for (const redirect of redirects) {
      const ppId = redirect.options._pp_id
      if (ppId) {
        redirectsByPpId.set(ppId, redirect)
      } else {
        unmatchedRedirects.push(redirect)
      }
    }

    for (const rule of ipv6Rules) {
      const ppId = rule.options._pp_id
      if (ppId) {
        rulesByPpId.set(ppId, rule)
      } else {
        unmatchedRules.push(rule)
      }
    }

    const ports: PublishedPort[] = []

    // Process matched pairs (have _pp_id)
    const processedPpIds = new Set<string>()

    for (const [ppId, redirect] of redirectsByPpId) {
      const rule = rulesByPpId.get(ppId) ?? null
      ports.push(this.sectionsToPublishedPort(redirect, rule))
      processedPpIds.add(ppId)
    }

    // Process IPv6-only rules with _pp_id (no matching redirect)
    for (const [ppId, rule] of rulesByPpId) {
      if (!processedPpIds.has(ppId)) {
        ports.push(this.sectionsToPublishedPort(null, rule))
      }
    }

    // Process unmatched redirects (no _pp_id - possibly created via LuCI)
    for (const redirect of unmatchedRedirects) {
      ports.push(this.sectionsToPublishedPort(redirect, null))
    }

    // Process unmatched IPv6 rules (no _pp_id - possibly created via LuCI)
    for (const rule of unmatchedRules) {
      ports.push(this.sectionsToPublishedPort(null, rule))
    }

    this._cachedPorts = ports
    return ports
  }

  /**
   * Get cached ports or load them if not available
   */
  async getPorts(): Promise<PublishedPort[]> {
    if (this._cachedPorts) return this._cachedPorts
    return this.get()
  }

  /**
   * Check if a device has any published port rules using a specific IP version
   */
  async getDevicePortUsage(
    mac: string,
  ): Promise<{ usesIpv4: boolean; usesIpv6: boolean }> {
    const ports = await this.getPorts()
    const macUpper = mac.toUpperCase()

    const devicePorts = ports.filter(
      p => p.deviceMac.toUpperCase() === macUpper && p.enabled,
    )

    return {
      usesIpv4: devicePorts.some(p => p.ipv4),
      usesIpv6: devicePorts.some(p => p.ipv6),
    }
  }

  /**
   * Check if any published port uses IPv6
   */
  async hasIpv6Ports(): Promise<boolean> {
    const ports = await this.getPorts()
    return ports.some(p => p.ipv6 && p.enabled)
  }

  async set(items: PublishedPort[]): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('UCI files not loaded')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Get all _pp_ids from our items to know what to keep/remove
    const ourPpIds = new Set(items.map(item => item.id))

    // Remove existing sections that belong to us (have _pp_id in our set or section name starts with pp_)
    uciFiles.firewall.sections = uciFiles.firewall.sections.filter(s => {
      if (s.type === 'redirect') {
        const redirect = s as FirewallRedirectSection
        // Remove if it has our _pp_id or section name starts with pp_
        if (redirect.options._pp_id && ourPpIds.has(redirect.options._pp_id)) {
          return false
        }
        if (redirect.name?.startsWith('pp_')) {
          return false
        }
      }
      if (s.type === 'rule') {
        const rule = s as FirewallRuleSection
        // Remove IPv6 port forward rules with our _pp_id
        if (
          rule.options.family === 'ipv6' &&
          rule.options.target === 'ACCEPT' &&
          rule.options._pp_id &&
          ourPpIds.has(rule.options._pp_id)
        ) {
          return false
        }
        if (rule.name?.startsWith('pp_')) {
          return false
        }
      }
      return true
    })

    // Add new sections for each published port
    for (const item of items) {
      const sections = this.publishedPortToSections(item)
      uciFiles.firewall.sections.push(...sections)
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Update cache
    this._cachedPorts = items

    // Restart firewall
    await this.api.exec({
      command: '/etc/init.d/firewall',
      args: ['restart'],
      timeout: 30000,
    })
  }

  /**
   * Convert UCI sections to a PublishedPort model
   */
  private sectionsToPublishedPort(
    redirect: FirewallRedirectSection | null,
    rule: FirewallRuleSection | null,
  ): PublishedPort {
    // Determine the primary section for common fields
    const primary = redirect || rule
    if (!primary) {
      throw new Error('At least one section required')
    }

    // Get protocol
    const proto = primary.options.proto || 'tcp'
    let protocol: Protocol
    if (proto === 'tcp udp') {
      protocol = 'tcp+udp'
    } else if (proto === 'tcp') {
      protocol = 'tcp'
    } else {
      protocol = 'udp'
    }

    // Get common options from whichever section has them
    const ppId =
      redirect?.options._pp_id ||
      rule?.options._pp_id ||
      primary.name ||
      crypto.randomUUID()
    const ppMac = redirect?.options._pp_mac || rule?.options._pp_mac || ''
    const enabled = primary.options.enabled !== '0'
    const label = primary.options.name || ''

    // Get port info
    const destPort =
      redirect?.options.dest_port || rule?.options.dest_port || ''
    const srcDport = redirect?.options.src_dport

    // Get source restriction
    const srcIp = redirect?.options.src_ip || rule?.options.src_ip
    const source = srcIp || 'any'

    return {
      id: ppId,
      enabled,
      label,
      deviceMac: ppMac,
      ports: destPort,
      protocol,
      ipv4: !!redirect,
      ipv6: !!rule,
      ipv4PublicPort: srcDport !== destPort ? srcDport : undefined,
      source,
    }
  }

  /**
   * Convert a PublishedPort to UCI sections
   */
  private publishedPortToSections(item: PublishedPort): UciSection[] {
    const sections: UciSection[] = []

    // Convert protocol
    let proto: 'tcp' | 'udp' | 'tcp udp'
    if (item.protocol === 'tcp+udp') {
      proto = 'tcp udp'
    } else {
      proto = item.protocol
    }

    // IPv4 redirect (DNAT)
    if (item.ipv4) {
      const redirect: FirewallRedirectSection = {
        type: 'redirect',
        name: `pp_${item.id}`,
        options: {
          name: item.label,
          src: 'wan',
          dest: 'lan',
          target: 'DNAT',
          proto,
          src_dport: item.ipv4PublicPort || item.ports,
          dest_ip: '', // Resolved at apply time from device lookup
          dest_port: item.ports,
          enabled: item.enabled ? '1' : '0',
          // Our metadata
          _pp_id: item.id,
          _pp_mac: item.deviceMac,
        },
        lists: {},
      }

      // Add source restriction if not 'any'
      if (item.source !== 'any') {
        redirect.options.src_ip = item.source
      }

      sections.push(redirect)
    }

    // IPv6 rule (ACCEPT)
    if (item.ipv6) {
      const rule: FirewallRuleSection = {
        type: 'rule',
        name: `pp_${item.id}_v6`,
        options: {
          name: item.label,
          src: 'wan',
          dest: 'lan',
          target: 'ACCEPT',
          proto,
          dest_ip: '', // Resolved at apply time from device lookup
          dest_port: item.ports,
          family: 'ipv6',
          enabled: item.enabled ? '1' : '0',
          // Our metadata
          _pp_id: item.id,
          _pp_mac: item.deviceMac,
        },
        lists: {},
      }

      // Add source restriction if not 'any'
      if (item.source !== 'any') {
        rule.options.src_ip = item.source
      }

      sections.push(rule)
    }

    return sections
  }
}
