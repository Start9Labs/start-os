import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { LanIpv6UciService, LanIpv6Data } from './uci/service'

@Injectable()
export class LanIpv6Service extends FormService<LanIpv6Data> {
  private readonly uci = inject(LanIpv6UciService)

  load() {
    return this.uci.get()
  }

  store(data: LanIpv6Data) {
    return this.uci.set(data)
  }
}
