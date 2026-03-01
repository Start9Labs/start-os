import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { DnsForm } from './utils'
import { DnsUciService } from './uci/service'

@Injectable()
export class DnsService extends FormService<DnsForm> {
  private readonly uci = inject(DnsUciService)

  load() {
    return this.uci.get()
  }

  store(data: DnsForm) {
    return this.uci.set(data)
  }
}
