import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { WanIpv4Form } from './utils'
import { Ipv4UciService } from './uci/service'

@Injectable()
export class Ipv4Service extends FormService<WanIpv4Form> {
  private readonly uci = inject(Ipv4UciService)

  async store(data: WanIpv4Form) {
    await this.uci.set(data)
  }

  async load(): Promise<WanIpv4Form> {
    return this.uci.get()
  }
}
