import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  FirewallRedirectSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'
import { Forwarding } from '../service'

type UciFiles = {
  firewall: UciFile<UciSection>
}

@Injectable({
  providedIn: 'root',
})
export class ForwardingUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<Forwarding[]> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['firewall'],
    })

    const redirects = this._uciFiles.firewall.sections.filter(
      (s): s is FirewallRedirectSection => s.type === 'redirect',
    )

    return redirects.map(section => this.sectionToForwarding(section))
  }

  async set(items: Forwarding[]): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('UCI files not loaded')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Remove existing redirects
    uciFiles.firewall.sections = uciFiles.firewall.sections.filter(
      s => s.type !== 'redirect',
    )

    // Add updated redirects
    const newSections = items.map(item => this.forwardingToSection(item))
    uciFiles.firewall.sections.push(...newSections)

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart firewall
    await this.api.exec({
      command: '/etc/init.d/firewall',
      args: ['restart'],
      timeout: 30000,
    })
  }

  private sectionToForwarding(section: FirewallRedirectSection): Forwarding {
    const proto = section.options.proto || 'tcp udp'
    let protocol: string
    if (proto === 'tcp udp') {
      protocol = 'TCP/UDP'
    } else if (proto === 'tcp') {
      protocol = 'TCP'
    } else {
      protocol = 'UDP'
    }

    return {
      enabled: section.options.enabled !== '0',
      purpose: section.options.name || '',
      protocol,
      ip: section.options.dest_ip || '',
      external: section.options.src_dport || '',
      internal: section.options.dest_port || '',
    }
  }

  private forwardingToSection(item: Forwarding): FirewallRedirectSection {
    let proto: 'tcp' | 'udp' | 'tcp udp'
    if (item.protocol === 'TCP/UDP') {
      proto = 'tcp udp'
    } else if (item.protocol === 'TCP') {
      proto = 'tcp'
    } else {
      proto = 'udp'
    }

    return {
      type: 'redirect',
      name: null,
      options: {
        name: item.purpose,
        src: 'wan',
        dest: 'lan',
        target: 'DNAT',
        proto,
        src_dport: item.external,
        dest_ip: item.ip,
        dest_port: item.internal,
        enabled: item.enabled ? '1' : '0',
      },
      lists: {},
    }
  }
}
