import { inject, Injectable } from '@angular/core'
import { TuiAlertService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { FormService } from 'src/app/services/form.service'
import { OutboundUciService } from './uci/service'
import { OutboundVpnTableItem } from './utils'

@Injectable()
export class OutboundService extends FormService<OutboundVpnTableItem[]> {
  private readonly uci = inject(OutboundUciService)
  private readonly alertService = inject(TuiAlertService)
  private readonly notifications = inject(TuiNotificationMiddleService)

  async load(): Promise<OutboundVpnTableItem[]> {
    return this.uci.get()
  }

  async store(): Promise<void> {
    // List doesn't have a single store operation
  }

  async update(
    id: string,
    data: { label: string; target: string },
  ): Promise<boolean> {
    const saving = this.notifications.open('Saving').subscribe()

    try {
      await this.uci.update(id, data)
      return true
    } catch (e: any) {
      this.alertService.open(e, { appearance: 'negative' }).subscribe()
      return false
    } finally {
      saving.unsubscribe()
    }
  }

  async create(data: {
    label: string
    target: string
    config: File
  }): Promise<string | null> {
    const saving = this.notifications.open('Creating').subscribe()

    try {
      return await this.uci.create(data)
    } catch (e: any) {
      this.alertService.open(e, { appearance: 'negative' }).subscribe()
      return null
    } finally {
      saving.unsubscribe()
    }
  }

  async remove(id: string): Promise<boolean> {
    const deleting = this.notifications.open('Deleting').subscribe()

    try {
      await this.uci.delete(id)
      return true
    } catch (e: any) {
      this.alertService.open(e, { appearance: 'negative' }).subscribe()
      return false
    } finally {
      deleting.unsubscribe()
    }
  }
}
