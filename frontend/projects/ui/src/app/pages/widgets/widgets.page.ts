import { Component, ElementRef } from '@angular/core'
import { TuiDestroyService, TuiResizeService } from '@taiga-ui/cdk'
import { distinctUntilChanged, map } from 'rxjs/operators'

@Component({
  selector: 'widgets',
  templateUrl: 'widgets.page.html',
  styleUrls: ['widgets.page.scss'],
  providers: [TuiDestroyService, TuiResizeService],
})
export class WidgetsPage {
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
    map(() => this.elementRef.nativeElement.clientWidth < 600),
    distinctUntilChanged(),
  )

  constructor(
    private readonly elementRef: ElementRef<HTMLElement>,
    private readonly resize$: TuiResizeService,
  ) {}

  getHeight(isMobile: boolean, index: number): number {
    if (isMobile && this.items[index].content === 'network') {
      return 3
    }

    return isMobile ? 2 : this.items[index].h
  }
}
