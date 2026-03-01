import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { MacForm } from './utils'
import { MacUciService } from './uci/service'

@Injectable()
export class MacService extends FormService<MacForm> {
  private readonly uci = inject(MacUciService)

  load() {
    return this.uci.get()
  }

  store(data: MacForm) {
    return this.uci.set(data)
  }
}
