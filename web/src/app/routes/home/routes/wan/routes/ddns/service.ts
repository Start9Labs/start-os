import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { DdnsForm } from './utils'
import { DdnsUciService } from './uci/service'

@Injectable()
export class DdnsService extends FormService<DdnsForm> {
  private readonly uci = inject(DdnsUciService)

  load() {
    return this.uci.get()
  }

  store(data: DdnsForm) {
    return this.uci.set(data)
  }
}
