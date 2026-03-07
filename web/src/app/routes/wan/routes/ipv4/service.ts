import { inject, Injectable, signal } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/utils/pauseFor'
import { NETWORK_RESTART_TIMEOUT_MS } from 'src/app/services/network-restart.service'
import { prefixFromNetmask, netmaskFromPrefix, WanIpv4Form } from './utils'

@Injectable()
export class WanIpv4Service extends FormService<WanIpv4Form> {
  private readonly api = inject(ApiService)
  readonly assignedIp = signal<string | null>(null)

  async load(): Promise<WanIpv4Form> {
    const res = await this.api.wanIpv4Get()
    this.assignedIp.set(res.assigned_ip)
    return {
      ip: {
        mode: res.mode,
        wan: res.address || '',
        prefix: res.netmask ? prefixFromNetmask(res.netmask) : '',
        gateway: res.gateway || '',
        username: res.username || '',
        password: res.password || '',
        device: res.device || '',
      },
    }
  }

  async store(data: WanIpv4Form): Promise<void> {
    const { ip } = data
    await this.api.wanIpv4Set({
      mode: ip.mode,
      address: ip.mode === 'static' ? ip.wan : undefined,
      netmask: ip.mode === 'static' ? netmaskFromPrefix(ip.prefix) : undefined,
      gateway: ip.mode === 'static' ? ip.gateway : undefined,
      username: ip.mode === 'pppoe' ? ip.username : undefined,
      password: ip.mode === 'pppoe' ? ip.password : undefined,
      device: ip.mode === 'pppoe' && ip.device ? ip.device : undefined,
    })
  }

  override async save(data: WanIpv4Form): Promise<boolean> {
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
