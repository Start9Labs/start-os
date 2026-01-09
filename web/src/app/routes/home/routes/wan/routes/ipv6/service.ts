import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { WanIpv6Form } from './utils'
import { Ipv6UciService } from './uci/service'

@Injectable()
export class Ipv6Service extends FormService<WanIpv6Form> {
  private readonly uci = inject(Ipv6UciService)

  async store(data: WanIpv6Form) {
    await this.uci.set(data)
  }

  async load(): Promise<WanIpv6Form> {
    return this.uci.get()
  }
}
