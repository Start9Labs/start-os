import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { LanIpv4Form } from './utils'
import { LanIpv4UciService } from './uci/service'

@Injectable()
export class LanIpv4Service extends FormService<LanIpv4Form> {
  private readonly uci = inject(LanIpv4UciService)

  load() {
    return this.uci.get()
  }

  store(data: LanIpv4Form) {
    return this.uci.set(data)
  }
}
