import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { WanIpv4Form } from './utils'
import { WanIpv4UciService } from './uci/service'

@Injectable()
export class WanIpv4Service extends FormService<WanIpv4Form> {
  private readonly uci = inject(WanIpv4UciService)

  async store(data: WanIpv4Form) {
    await this.uci.set(data)
  }

  async load(): Promise<WanIpv4Form> {
    return this.uci.get()
  }
}
