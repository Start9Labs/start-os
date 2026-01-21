import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  FirewallRedirectSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'
import { Protocol, PublishedPort } from '../types'

// Custom UCI section for published port metadata
export type PublishedPortSection = {
  type: 'published_port'
  name: string | null
  options: {
    id?: string
    enabled?: '0' | '1'
    label?: string
    device_mac?: string
    ports?: string
    protocol?: 'tcp' | 'udp' | 'tcp+udp'
    ipv4?: '0' | '1'
    ipv6?: '0' | '1'
    ipv4_public_port?: string
    source?: string
  }
  lists: {}
}

type UciFiles = {
  firewall: UciFile<UciSection>
  // In a real implementation, we'd have a custom config file:
  // published_ports: UciFile<PublishedPortSection>
}

@Injectable({
  providedIn: 'root',
})
export class PublishedPortsUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<PublishedPort[]> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['firewall'],
    })

    // In a real implementation, we'd read from a custom config file
    // For now, we parse from firewall redirects with extended options
    const redirects = this._uciFiles.firewall.sections.filter(
      (s): s is FirewallRedirectSection => s.type === 'redirect',
    )

    return redirects.map(section => this.sectionToPublishedPort(section))
  }

  async set(items: PublishedPort[]): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('UCI files not loaded')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Remove existing redirects managed by us
    uciFiles.firewall.sections = uciFiles.firewall.sections.filter(
      s => s.type !== 'redirect',
    )

    // Add updated redirects
    const newSections = items.flatMap(item =>
      this.publishedPortToSections(item),
    )
    uciFiles.firewall.sections.push(...newSections)

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart firewall
    await this.api.exec({
      command: '/etc/init.d/firewall',
      args: ['restart'],
      timeout: 30000,
    })
  }

  private sectionToPublishedPort(
    section: FirewallRedirectSection,
  ): PublishedPort {
    const proto = section.options.proto || 'tcp'
    let protocol: Protocol
    if (proto === 'tcp udp') {
      protocol = 'tcp+udp'
    } else if (proto === 'tcp') {
      protocol = 'tcp'
    } else {
      protocol = 'udp'
    }

    // Parse extended options from name field (temporary hack)
    // Format: "label|mac|ipv4|ipv6|source"
    const nameParts = (section.options.name || '').split('|')
    const label = nameParts[0] || ''
    const deviceMac = nameParts[1] || ''
    const ipv4Enabled = nameParts[2] !== '0'
    const ipv6Enabled = nameParts[3] === '1'
    const source = nameParts[4] || 'any'

    return {
      id: section.name || crypto.randomUUID(),
      enabled: section.options.enabled !== '0',
      label,
      deviceMac,
      ports: section.options.dest_port || '',
      protocol,
      ipv4: ipv4Enabled,
      ipv6: ipv6Enabled,
      ipv4PublicPort: section.options.src_dport,
      source,
    }
  }

  private publishedPortToSections(
    item: PublishedPort,
  ): FirewallRedirectSection[] {
    const sections: FirewallRedirectSection[] = []

    let proto: 'tcp' | 'udp' | 'tcp udp'
    if (item.protocol === 'tcp+udp') {
      proto = 'tcp udp'
    } else {
      proto = item.protocol
    }

    // Encode metadata in name field (temporary hack)
    // In production, this would be stored in a separate config
    const encodedName = `${item.label}|${item.deviceMac}|${item.ipv4 ? '1' : '0'}|${item.ipv6 ? '1' : '0'}|${item.source}`

    // IPv4 redirect (DNAT)
    if (item.ipv4) {
      sections.push({
        type: 'redirect',
        name: item.id,
        options: {
          name: encodedName,
          src: 'wan',
          dest: 'lan',
          target: 'DNAT',
          proto,
          src_dport: item.ipv4PublicPort || item.ports,
          dest_ip: '', // Will be resolved from MAC at apply time
          dest_port: item.ports,
          enabled: item.enabled ? '1' : '0',
          // Source restriction
          ...(item.source !== 'any' && { src_ip: item.source }),
        },
        lists: {},
      })
    }

    // IPv6 would use firewall rules instead of redirects
    // (not implemented in this version)

    return sections
  }
}
