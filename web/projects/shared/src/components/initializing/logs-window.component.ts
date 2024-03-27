import { AsyncPipe } from '@angular/common'
import { Component, ElementRef, inject } from '@angular/core'
import {
  IntersectionObserverModule,
  INTERSECTION_ROOT,
} from '@ng-web-apis/intersection-observer'
import { MutationObserverModule } from '@ng-web-apis/mutation-observer'
import { TuiScrollbarModule } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { SetupLogsService } from '../../services/setup-logs.service'

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
    MutationObserverModule,
    IntersectionObserverModule,
    NgDompurifyModule,
    TuiScrollbarModule,
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
    if (this.scroll) bottom.scrollIntoView({ behavior: 'smooth' })
  }

  onBottom([{ isIntersecting }]: readonly IntersectionObserverEntry[]) {
    this.scroll = isIntersecting
  }
}
