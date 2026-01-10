import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { DdnsForm, DdnsProvider } from '../utils'
import { DdnsSection, UciFile } from 'src/app/services/api/types'

type UciFiles = {
  ddns: UciFile<DdnsSection>
}

// Map our provider keys to ddns-scripts service names
const PROVIDER_SERVICE_MAP: Record<DdnsProvider, string> = {
  start9: 'start9',
  dyndns: 'dyndns.org',
  noip: 'no-ip.com',
  cloudflare: 'cloudflare.com-v4',
  duckdns: 'duckdns.org',
  freedns: 'freedns.afraid.org',
}

const SERVICE_PROVIDER_MAP = Object.fromEntries(
  Object.entries(PROVIDER_SERVICE_MAP).map(([k, v]) => [v, k]),
) as Record<string, DdnsProvider>

@Injectable({
  providedIn: 'root',
})
export class DdnsUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<DdnsForm> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['ddns'],
    })

    // Find the WAN DDNS service
    const ddnsService = this._uciFiles.ddns.sections.find(
      (s): s is DdnsSection => s.type === 'service' && s.name === 'wan',
    )

    if (!ddnsService) {
      return {
        enabled: false,
        provider: 'start9',
        fields: {
          username: '',
          password: '',
          hostname: '',
          token: '',
          zone: '',
        },
      }
    }

    const serviceName = ddnsService.options.service_name || ''
    const provider = SERVICE_PROVIDER_MAP[serviceName] || 'start9'

    return {
      enabled: ddnsService.options.enabled === '1',
      provider,
      fields: {
        username: ddnsService.options.username || '',
        password: ddnsService.options.password || '',
        hostname:
          ddnsService.options.domain || ddnsService.options.lookup_host || '',
        token: ddnsService.options.password || '', // Some providers use password field for token
        zone: '', // Cloudflare-specific, stored differently
      },
    }
  }

  async set({ enabled, provider, fields }: DdnsForm): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    // Find or create WAN DDNS service
    let ddnsService = uciFiles.ddns.sections.find(
      (s): s is DdnsSection => s.type === 'service' && s.name === 'wan',
    )

    if (!ddnsService) {
      ddnsService = {
        type: 'service',
        name: 'wan',
        options: {},
        lists: {},
      }
      uciFiles.ddns.sections.push(ddnsService)
    }

    ddnsService.options.enabled = enabled ? '1' : '0'
    ddnsService.options.service_name = PROVIDER_SERVICE_MAP[provider]
    ddnsService.options.ip_source = 'network'
    ddnsService.options.ip_network = 'wan'

    // Clear all auth fields first
    delete ddnsService.options.username
    delete ddnsService.options.password
    delete ddnsService.options.domain
    delete ddnsService.options.lookup_host

    if (enabled && provider !== 'start9') {
      // Set fields based on provider
      if (fields.username) {
        ddnsService.options.username = fields.username
      }
      if (fields.password) {
        ddnsService.options.password = fields.password
      }
      if (fields.token) {
        // Most providers use password field for tokens
        ddnsService.options.password = fields.token
      }
      if (fields.hostname) {
        ddnsService.options.domain = fields.hostname
        ddnsService.options.lookup_host = fields.hostname
      }
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart DDNS service
    await this.api.exec({
      command: '/etc/init.d/ddns',
      args: [enabled ? 'restart' : 'stop'],
      timeout: 10000,
    })
  }
}
