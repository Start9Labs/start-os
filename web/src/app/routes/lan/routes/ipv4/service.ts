import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ApiService } from 'src/app/services/api/api.service'
import { buildRouterIp, LanIpv4Form, parseIpToForm } from './utils'

@Injectable()
export class LanIpv4Service extends FormService<LanIpv4Form> {
  private readonly api = inject(ApiService)

  async load(): Promise<LanIpv4Form> {
    const res = await this.api.lanIpv4Get()
    return parseIpToForm(res.address)
  }

  async store(data: LanIpv4Form): Promise<void> {
    await this.api.lanIpv4Set({ address: buildRouterIp(data.ip) })
  }

  override async save(data: LanIpv4Form): Promise<boolean> {
    return this.actions.run(() => this.store(data), {
      loading: 'Applying LAN settings...',
      success: 'LAN settings applied',
      restart: true,
    })
  }

  saveForIpChange(data: LanIpv4Form): Promise<boolean> {
    this.networkRestart.suppress()
    return this.api.lanIpv4Set({ address: buildRouterIp(data.ip) }).then(
      () => true,
      () => false,
    )
  }
}
