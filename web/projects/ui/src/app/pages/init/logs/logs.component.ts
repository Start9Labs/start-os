import { Component, ElementRef, inject } from '@angular/core'
import { INTERSECTION_ROOT } from '@ng-web-apis/intersection-observer'
import { LogsService } from 'src/app/pages/init/logs/logs.service'

@Component({
  selector: 'logs-window',
  templateUrl: 'logs.template.html',
  styles: [
    `
      pre {
        margin: 0;
      }
    `,
  ],
  providers: [
    {
      provide: INTERSECTION_ROOT,
      useExisting: ElementRef,
    },
  ],
})
export class LogsComponent {
  readonly logs$ = inject(LogsService)
  scroll = true

  scrollTo(bottom: HTMLElement) {
    if (this.scroll) bottom.scrollIntoView({ behavior: 'smooth' })
  }

  onBottom([{ isIntersecting }]: readonly IntersectionObserverEntry[]) {
    this.scroll = isIntersecting
  }
}
