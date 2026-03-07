import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/utils/pauseFor'
import { NETWORK_RESTART_TIMEOUT_MS } from 'src/app/services/network-restart.service'
import { DnsForm, parseDnsServer } from './utils'

@Injectable()
export class DnsService extends FormService<DnsForm> {
  private readonly api = inject(ApiService)

  async load(): Promise<DnsForm> {
    const res = await this.api.wanDnsGet()

    if (res.mode === 'custom' && res.servers.length > 0) {
      const s1 = parseDnsServer(res.servers[0])
      const s2 = res.servers[1]
        ? parseDnsServer(res.servers[1])
        : { ip: '', tls: false }
      const s3 = res.servers[2]
        ? parseDnsServer(res.servers[2])
        : { ip: '', tls: false }

      return {
        mode: 'custom',
        custom1: s1.ip,
        custom2: s2.ip,
        custom3: s3.ip,
        custom1Tls: s1.tls,
        custom2Tls: s2.tls,
        custom3Tls: s3.tls,
      }
    }

    return {
      mode: 'isp',
      custom1: '',
      custom2: '',
      custom3: '',
      custom1Tls: false,
      custom2Tls: false,
      custom3Tls: false,
    }
  }

  async store(data: DnsForm): Promise<void> {
    if (data.mode === 'isp') {
      await this.api.wanDnsSet({ mode: 'isp' })
    } else {
      const servers: string[] = []
      if (data.custom1) {
        servers.push(data.custom1Tls ? `${data.custom1}@853` : data.custom1)
      }
      if (data.custom2) {
        servers.push(data.custom2Tls ? `${data.custom2}@853` : data.custom2)
      }
      if (data.custom3) {
        servers.push(data.custom3Tls ? `${data.custom3}@853` : data.custom3)
      }
      await this.api.wanDnsSet({ mode: 'custom', servers })
    }
  }

  override async save(data: DnsForm): Promise<boolean> {
    return this.actions.run(
      async () => {
        await this.store(data)
        await pauseFor(NETWORK_RESTART_TIMEOUT_MS)
        await this.refreshAndWait()
      },
      {
        loading: 'Applying WAN settings...',
        success: 'WAN settings applied',
        restart: true,
      },
    )
  }
}
