import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { tap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { DesktopService } from './desktop.service'

@Component({
  templateUrl: 'desktop.component.html',
  styleUrls: ['desktop.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DesktopComponent {
  private readonly desktop = inject(DesktopService)

  readonly desktop$ = this.desktop.desktop$.pipe(
    tap(() => (this.order = new Map())),
  )

  readonly packages$ =
    inject<PatchDB<DataModel>>(PatchDB).watch$('package-data')

  order = new Map()

  onReorder(order: Map<number, number>, desktop: readonly string[]) {
    this.order = order

    const items: string[] = []

    Array.from(this.order.entries()).forEach(([index, order]) => {
      items[order] = desktop[index]
    })

    this.desktop.save(items)
  }
}
