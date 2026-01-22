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
  private _cachedData?: DdnsForm

  /**
   * Check if DDNS is enabled
   */
  async isEnabled(): Promise<boolean> {
    const data = await this.get()
    return data.enabled
  }

  /**
   * Get the DDNS hostname if enabled
   * Always fetches fresh data to ensure accuracy
   * For Start9: queries the auto-assigned hostname from the system
   * For other providers: returns the user-configured hostname
   */
  async getHostname(): Promise<string | null> {
    const data = await this.get()
    if (!data.enabled) return null

    // Start9 auto-assigns a hostname - query it from the system
    if (data.provider === 'start9') {
      return this.getStart9Hostname()
    }

    return data.fields.hostname || null
  }

  /**
   * Get the Start9 auto-assigned DDNS hostname
   * Start9 stores the assigned hostname in /etc/start9/hostname
   */
  private async getStart9Hostname(): Promise<string | null> {
    try {
      const result = await this.api.exec({
        command: 'cat',
        args: ['/etc/start9/hostname'],
        timeout: 5000,
      })
      if (result.exitCode === 0 && result.stdout.trim()) {
        return result.stdout.trim()
      }
    } catch {
      // File doesn't exist or not accessible
    }
    return null
  }

  /**
   * Get cached data or load if not available
   */
  async getData(): Promise<DdnsForm> {
    if (this._cachedData) return this._cachedData
    return this.get()
  }

  async get(): Promise<DdnsForm> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['ddns'],
    })

    // Find the WAN DDNS service
    const ddnsService = this._uciFiles.ddns.sections.find(
      (s): s is DdnsSection => s.type === 'service' && s.name === 'wan',
    )

    if (!ddnsService) {
      this._cachedData = {
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
      return this._cachedData
    }

    const serviceName = ddnsService.options.service_name || ''
    const provider = SERVICE_PROVIDER_MAP[serviceName] || 'start9'

    this._cachedData = {
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

    return this._cachedData
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

    // Clear cache
    this._cachedData = undefined

    // Restart DDNS service
    await this.api.exec({
      command: '/etc/init.d/ddns',
      args: [enabled ? 'restart' : 'stop'],
      timeout: 10000,
    })
  }
}
