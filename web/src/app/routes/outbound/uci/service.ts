import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  UciFile,
  UciSection,
  WireGuardInterfaceSection,
  WireGuardPeerSection,
} from 'src/app/services/api/types'
import { OutboundVpnTableItem } from '../utils'

type UciFiles = {
  network: UciFile<UciSection>
}

@Injectable({
  providedIn: 'root',
})
export class OutboundUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<OutboundVpnTableItem[]> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    return this._uciFiles.network.sections
      .filter(
        (s): s is WireGuardInterfaceSection =>
          this.isWireGuardInterface(s) && !!s.name,
      )
      .map(section => ({
        id: section.name!,
        label: section.options.label || section.name!,
        target: section.options.target || 'Internet',
        enabled: section.options.disabled !== '1',
        usedBy: '', // Stub - will come from security profiles later
      }))
  }

  async update(
    id: string,
    data: { label: string; target: string },
  ): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    const iface = uciFiles.network.sections.find(
      (s): s is WireGuardInterfaceSection =>
        this.isWireGuardInterface(s) && s.name === id,
    )

    if (!iface) {
      throw new Error(`VPN interface ${id} not found`)
    }

    iface.options.label = data.label
    iface.options.target = data.target

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }

  async create(data: {
    label: string
    target: string
    config: File
  }): Promise<string> {
    if (!this._uciFiles) {
      this._uciFiles = await this.api.getUci<UciFiles>({
        names: ['network'],
      })
    }

    const configText = await data.config.text()
    const parsed = this.parseWireGuardConfig(configText)

    const id = `wg_${data.label.toLowerCase().replace(/[^a-z0-9]/g, '_')}`
    const peerId = `${id.replace('wg_', '')}_peer0`

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Create interface section
    const interfaceSection: WireGuardInterfaceSection = {
      type: 'interface',
      name: id,
      options: {
        proto: 'wireguard',
        private_key: parsed.interface.privateKey,
        disabled: '0',
        label: data.label,
        target: data.target,
      },
      lists: {
        addresses: parsed.interface.addresses,
        dns: parsed.interface.dns,
      },
    }

    // Create peer section
    const peerSection: WireGuardPeerSection = {
      type: 'wireguard_peer',
      name: peerId,
      options: {
        public_key: parsed.peer.publicKey,
        endpoint_host: parsed.peer.endpointHost,
        endpoint_port: parsed.peer.endpointPort,
        persistent_keepalive: parsed.peer.persistentKeepalive,
        route_allowed_ips: '1',
      },
      lists: {
        allowed_ips: parsed.peer.allowedIps,
      },
    }

    if (parsed.peer.presharedKey) {
      peerSection.options.preshared_key = parsed.peer.presharedKey
    }

    uciFiles.network.sections.push(interfaceSection, peerSection)

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })

    return id
  }

  async delete(id: string): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    uciFiles.network.sections = uciFiles.network.sections.filter(s => {
      if (this.isWireGuardInterface(s) && s.name === id) return false
      if (
        this.isWireGuardPeer(s) &&
        s.name?.startsWith(id.replace('wg_', '')) === true
      )
        return false
      return true
    })

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }

  async setEnabled(id: string, enabled: boolean): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    const iface = uciFiles.network.sections.find(
      (s): s is WireGuardInterfaceSection =>
        this.isWireGuardInterface(s) && s.name === id,
    )

    if (!iface) {
      throw new Error(`VPN interface ${id} not found`)
    }

    iface.options.disabled = enabled ? '0' : '1'

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }

  private isWireGuardInterface(
    section: UciSection,
  ): section is WireGuardInterfaceSection {
    return (
      section.type === 'interface' &&
      'proto' in section.options &&
      section.options.proto === 'wireguard'
    )
  }

  private isWireGuardPeer(
    section: UciSection,
  ): section is WireGuardPeerSection {
    return section.type === 'wireguard_peer'
  }

  private parseWireGuardConfig(configText: string): {
    interface: {
      privateKey: string
      addresses: string[]
      dns: string[]
    }
    peer: {
      publicKey: string
      presharedKey?: string
      endpointHost?: string
      endpointPort?: string
      allowedIps: string[]
      persistentKeepalive?: string
    }
  } {
    const lines = configText.split('\n').map(line => line.trim())
    let currentSection = ''

    const result = {
      interface: {
        privateKey: '',
        addresses: [] as string[],
        dns: [] as string[],
      },
      peer: {
        publicKey: '',
        presharedKey: undefined as string | undefined,
        endpointHost: undefined as string | undefined,
        endpointPort: undefined as string | undefined,
        allowedIps: [] as string[],
        persistentKeepalive: undefined as string | undefined,
      },
    }

    for (const line of lines) {
      // Skip empty lines and comments
      if (!line || line.startsWith('#')) continue

      // Section headers
      if (line.toLowerCase() === '[interface]') {
        currentSection = 'interface'
        continue
      }
      if (line.toLowerCase() === '[peer]') {
        currentSection = 'peer'
        continue
      }

      // Key-value pairs
      const match = line.match(/^(\w+)\s*=\s*(.+)$/)
      if (!match) continue

      const [, key, value] = match
      const keyLower = key.toLowerCase()

      if (currentSection === 'interface') {
        switch (keyLower) {
          case 'privatekey':
            result.interface.privateKey = value.trim()
            break
          case 'address':
            result.interface.addresses = value.split(',').map(s => s.trim())
            break
          case 'dns':
            result.interface.dns = value.split(',').map(s => s.trim())
            break
        }
      } else if (currentSection === 'peer') {
        switch (keyLower) {
          case 'publickey':
            result.peer.publicKey = value.trim()
            break
          case 'presharedkey':
            result.peer.presharedKey = value.trim()
            break
          case 'endpoint': {
            const endpoint = value.trim()
            const colonIndex = endpoint.lastIndexOf(':')
            if (colonIndex !== -1) {
              result.peer.endpointHost = endpoint.substring(0, colonIndex)
              result.peer.endpointPort = endpoint.substring(colonIndex + 1)
            }
            break
          }
          case 'allowedips':
            result.peer.allowedIps = value.split(',').map(s => s.trim())
            break
          case 'persistentkeepalive':
            result.peer.persistentKeepalive = value.trim()
            break
        }
      }
    }

    return result
  }
}
