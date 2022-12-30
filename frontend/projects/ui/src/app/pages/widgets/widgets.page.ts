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
import {
  distinctUntilChanged,
  filter,
  map,
  pairwise,
  startWith,
  takeUntil,
} from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel, Widget } from '../../services/patch-db/data-model'
import { ADD_WIDGET } from './built-in/add/add.component'
import { FavoritesComponent } from './built-in/favorites/favorites.component'
import { HealthComponent } from './built-in/health/health.component'
import { NetworkComponent } from './built-in/network/network.component'
import { MetricsComponent } from './built-in/metrics/metrics.component'
import { UptimeComponent } from './built-in/uptime/uptime.component'

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
    private readonly patchDb: PatchDB<DataModel>,
    private readonly destroy$: TuiDestroyService,
    private readonly cdr: ChangeDetectorRef,
  ) {
    this.patchDb
      .watch$('ui', 'widgets', 'widgets')
      .pipe(
        startWith([]),
        pairwise(),
        filter(([prev, { length }]) => prev.length !== length),
        tuiWatch(this.cdr),
        takeUntil(this.destroy$),
      )
      .subscribe(([, items]) => {
        this.items = items
        this.order = new Map(items.map((_, index) => [index, index]))
      })
  }

  close() {
    this.context?.$implicit?.complete()
    // TODO: close sidebar
  }

  toggle() {
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

  // TODO: waiting for the backend
  private removeWidget(index: number) {
    this.items = this.items.filter((_, i) => i !== index)
    this.order.delete(index)
  }

  // TODO: waiting for the backend
  private addWidget(widget: Widget) {
    this.items = this.items.concat(widget)
  }
}

export const WIDGETS_COMPONENT = new PolymorpheusComponent(WidgetsPage)
