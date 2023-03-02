import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  ElementRef,
  Inject,
  Optional,
  Type,
} from '@angular/core'
import { TuiDestroyService, TuiResizeService, tuiWatch } from '@taiga-ui/cdk'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import {
  PolymorpheusComponent,
  POLYMORPHEUS_CONTEXT,
} from '@tinkoff/ng-polymorpheus'
import { distinctUntilChanged, map, startWith, takeUntil } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel, Widget } from '../../services/patch-db/data-model'
import { ApiService } from '../../services/api/embassy-api.service'
import { ADD_WIDGET } from './built-in/add/add.component'
import { FavoritesComponent } from './built-in/favorites/favorites.component'
import { HealthComponent } from './built-in/health/health.component'
import { NetworkComponent } from './built-in/network/network.component'
import { MetricsComponent } from './built-in/metrics/metrics.component'
import { UptimeComponent } from './built-in/uptime/uptime.component'
import { WidgetsService } from './built-in/widgets.service'
import { take } from 'rxjs/operators'

@Component({
  selector: 'widgets',
  templateUrl: 'widgets.page.html',
  styleUrls: ['widgets.page.scss'],
  providers: [TuiDestroyService, TuiResizeService],
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: {
    '[class.dialog]': 'context',
  },
})
export class WidgetsPage {
  edit = false

  order = new Map<number, number>()

  items: readonly Widget[] = []

  pending = true

  readonly isMobile$ = this.resize$.pipe(
    startWith(null),
    map(() => this.elementRef.nativeElement.clientWidth < 600),
    distinctUntilChanged(),
  )

  readonly components: Record<string, Type<any>> = {
    health: HealthComponent,
    favorites: FavoritesComponent,
    metrics: MetricsComponent,
    network: NetworkComponent,
    uptime: UptimeComponent,
  }

  constructor(
    @Optional()
    @Inject(POLYMORPHEUS_CONTEXT)
    readonly context: TuiDialogContext | null,
    private readonly elementRef: ElementRef<HTMLElement>,
    private readonly resize$: TuiResizeService,
    private readonly dialog: TuiDialogService,
    private readonly patch: PatchDB<DataModel>,
    private readonly destroy$: TuiDestroyService,
    private readonly cdr: ChangeDetectorRef,
    private readonly api: ApiService,
    private readonly service: WidgetsService,
  ) {
    this.patch
      .watch$('ui', 'widgets', 'widgets')
      .pipe(take(1))
      .subscribe(items => {
        this.updateItems(items)
        this.pending = false
      })
  }

  trackBy(_: number, { id }: Widget) {
    return id
  }

  close() {
    if (this.context) {
      this.context.$implicit.complete()
    } else {
      this.service.toggle(false)
    }
  }

  toggle() {
    if (this.edit) {
      this.updateItems(this.getReordered())
    }

    this.edit = !this.edit
  }

  add() {
    this.dialog.open(ADD_WIDGET, { label: 'Add widget' }).subscribe(widget => {
      this.addWidget(widget)
    })
  }

  remove(index: number) {
    this.removeWidget(index)
  }

  private removeWidget(index: number) {
    this.updateItems(
      this.getReordered().filter((_, i) => i !== this.order.get(index)),
    )
  }

  private addWidget(widget: Widget) {
    this.updateItems(this.getReordered().concat(widget))
  }

  private getReordered(): Widget[] {
    const items: Widget[] = []

    Array.from(this.order.entries()).forEach(([index, order]) => {
      items[order] = this.items[index]
    })

    return items
  }

  private updateItems(items: readonly Widget[]) {
    const previous = this.items

    if (!this.pending) {
      this.pending = true
      this.api
        .setDbValue(['widgets', 'widgets'], items)
        .catch(() => {
          this.updateItems(previous)
        })
        .finally(() => {
          this.pending = false
          this.cdr.markForCheck()
        })
    }

    this.items = items
    this.order = new Map(items.map((_, index) => [index, index]))
  }
}

export const WIDGETS_COMPONENT = new PolymorpheusComponent(WidgetsPage)
