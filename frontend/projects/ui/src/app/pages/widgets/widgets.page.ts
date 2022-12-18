import { Component, ElementRef, Inject, Optional } from '@angular/core'
import { TuiDestroyService, TuiResizeService } from '@taiga-ui/cdk'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import {
  PolymorpheusComponent,
  POLYMORPHEUS_CONTEXT,
} from '@tinkoff/ng-polymorpheus'
import { distinctUntilChanged, map, startWith } from 'rxjs'

@Component({
  selector: 'widgets',
  templateUrl: 'widgets.page.html',
  styleUrls: ['widgets.page.scss'],
  providers: [TuiDestroyService, TuiResizeService],
  host: {
    '[class.dialog]': 'context',
  },
})
export class WidgetsPage {
  edit = false

  order = new Map<number, number>()

  items = [
    { w: 2, h: 2, content: 'health' },
    { w: 2, h: 2, content: 'backup' },
    { w: 2, h: 2, content: 'achievements' },
    { w: 3, h: 2, content: 'rick' },
    { w: 3, h: 2, content: 'network' },
    { w: 6, h: 1, content: 'stats' },
  ]

  readonly isMobile$ = this.resize$.pipe(
    startWith(null),
    map(() => this.elementRef.nativeElement.clientWidth < 600),
    distinctUntilChanged(),
  )

  constructor(
    @Optional()
    @Inject(POLYMORPHEUS_CONTEXT)
    readonly context: TuiDialogContext | null,
    private readonly elementRef: ElementRef<HTMLElement>,
    private readonly resize$: TuiResizeService,
    private readonly dialog: TuiDialogService,
  ) {}

  toggle() {
    this.edit = !this.edit
  }

  add() {
    this.dialog
      .open('Here be widgets list and search', { label: 'Add widget' })
      .subscribe()
  }

  onRemove(index: number) {
    this.items.splice(index, 1)
    this.order.delete(index)
  }

  getHeight(isMobile: boolean, index: number): number {
    if (isMobile && this.items[index].content === 'network') {
      return 3
    }

    return isMobile ? 2 : this.items[index].h
  }
}

export const WIDGETS_COMPONENT = new PolymorpheusComponent(WidgetsPage)
