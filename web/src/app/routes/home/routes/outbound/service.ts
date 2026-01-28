import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { OutboundUciService } from './uci/service'
import { OutboundVpnTableItem } from './utils'

@Injectable()
export class OutboundService extends FormService<OutboundVpnTableItem[]> {
  private readonly uci = inject(OutboundUciService)

  async load(): Promise<OutboundVpnTableItem[]> {
    return this.uci.get()
  }

  async store(): Promise<void> {
    // List doesn't have a single store operation
  }

  update(id: string, data: { label: string; target: string }) {
    return this.actions.run(async () => await this.uci.update(id, data))
  }

  create(data: { label: string; target: string; config: File }) {
    return this.actions.run(async () => await this.uci.create(data), {
      loading: 'Creating',
    })
  }

  remove(id: string) {
    return this.actions.run(async () => await this.uci.delete(id), {
      loading: 'Deleting',
    })
  }
}
