import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/utils/pauseFor'
import { NETWORK_RESTART_TIMEOUT_MS } from 'src/app/services/network-restart.service'
import { MacForm } from './utils'

@Injectable()
export class MacService extends FormService<MacForm> {
  private readonly api = inject(ApiService)

  async load(): Promise<MacForm> {
    const res = await this.api.wanMacGet()
    return {
      strategy: res.strategy,
      address: {
        mac: res.mac,
      },
    }
  }

  async store(data: MacForm): Promise<void> {
    await this.api.wanMacSet({
      strategy: data.strategy,
      mac: data.strategy === 'custom' ? data.address.mac : undefined,
    })
  }

  override async save(data: MacForm): Promise<boolean> {
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
