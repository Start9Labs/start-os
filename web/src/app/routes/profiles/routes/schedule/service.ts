import { inject, Injectable } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService, ScheduleWindow } from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

export type { ScheduleWindow }

@Injectable()
export class ProfileScheduleService extends FormService<ScheduleWindow[]> {
  private readonly api = inject(ApiService)
  private readonly iface = inject(ActivatedRoute).snapshot.params['interface']

  async load() {
    return this.api.profileScheduleGet({ interface: this.iface })
  }

  async store(data: ScheduleWindow[]) {
    await this.api.profileScheduleSet({
      interface: this.iface,
      windows: data,
    })
  }
}
