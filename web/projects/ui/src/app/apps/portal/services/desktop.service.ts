import { inject, Injectable } from '@angular/core'
import { TuiAlertService } from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class DesktopService {
  private readonly alerts = inject(TuiAlertService)
  private readonly api = inject(ApiService)

  order = new Map()
  items: readonly string[] = []

  add(id: string) {
    if (this.items.includes(id)) return

    this.items = this.items.concat(id)
    this.save(this.items)
  }

  remove(id: string) {
    if (!this.items.includes(id)) return

    this.items = this.items.filter(x => x !== id)
    this.save(this.items)
  }

  save(ids: readonly string[] = []) {
    this.api
      .setDbValue(['desktop'], Array.from(new Set(ids)))
      .catch(() =>
        this.alerts
          .open(
            'Desktop might be out of sync. Please refresh the page to fix it.',
            { status: 'warning' },
          )
          .subscribe(),
      )
  }

  reorder(order: Map<number, number>) {
    this.order = order

    const items: string[] = [...this.items]

    Array.from(this.order.entries()).forEach(([index, order]) => {
      items[order] = this.items[index]
    })

    this.save(items)
  }
}
