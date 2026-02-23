import { inject, Injectable } from '@angular/core'
import { ApiService, BlackoutWindow } from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

export type { BlackoutWindow }

@Injectable({ providedIn: 'root' })
export class BlackoutService extends FormService<BlackoutWindow[]> {
  private readonly api = inject(ApiService)

  async load() {
    return this.api.wifiBlackoutGet()
  }

  async store(data: BlackoutWindow[]) {
    await this.api.wifiBlackoutSet(data)
  }
}
