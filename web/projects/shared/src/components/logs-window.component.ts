import { TuiScrollbar } from '@taiga-ui/core'
import { AsyncPipe } from '@angular/common'
import { Component, ElementRef, inject } from '@angular/core'
import {
  WaIntersectionObserver,
  INTERSECTION_ROOT,
} from '@ng-web-apis/intersection-observer'
import { WaMutationObserver } from '@ng-web-apis/mutation-observer'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { SetupLogsService } from '../services/setup-logs.service'

@Component({
  standalone: true,
  selector: 'logs-window',
  template: `
    <tui-scrollbar childList subtree (waMutationObserver)="scrollTo(bottom)">
      @for (log of logs$ | async; track log) {
        <pre [innerHTML]="log | dompurify"></pre>
      }
      <section
        #bottom
        waIntersectionObserver
        [style.padding.rem]="1"
        (waIntersectionObservee)="onBottom($event)"
      ></section>
    </tui-scrollbar>
  `,
  imports: [
    AsyncPipe,
    WaMutationObserver,
    WaIntersectionObserver,
    NgDompurifyModule,
    TuiScrollbar,
  ],
  providers: [
    {
      provide: INTERSECTION_ROOT,
      useExisting: ElementRef,
    },
  ],
})
export class LogsWindowComponent {
  readonly logs$ = inject(SetupLogsService)
  scroll = true

  scrollTo(bottom: HTMLElement) {
    if (this.scroll) bottom.scrollIntoView()
  }

  onBottom(entries: readonly IntersectionObserverEntry[]) {
    this.scroll = !!entries[entries.length - 1]?.isIntersecting
  }
}
