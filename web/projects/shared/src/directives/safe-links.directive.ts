import { Directive, inject, DOCUMENT } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import {
  MutationObserverService,
  provideMutationObserverInit,
} from '@ng-web-apis/mutation-observer'
import { tuiInjectElement } from '@taiga-ui/cdk'

@Directive({
  selector: '[safeLinks]',
  providers: [
    MutationObserverService,
    provideMutationObserverInit({
      childList: true,
      subtree: true,
    }),
  ],
})
export class SafeLinksDirective {
  private readonly doc = inject(DOCUMENT)
  private readonly el = tuiInjectElement()
  private readonly sub = inject(MutationObserverService)
    .pipe(takeUntilDestroyed())
    .subscribe(() => {
      Array.from(this.doc.links)
        .filter(
          link =>
            link.hostname !== this.doc.location.hostname &&
            this.el.contains(link),
        )
        .forEach(link => {
          link.target = '_blank'
          link.setAttribute('rel', 'noreferrer')
          link.classList.add('g-external-link')
        })
    })
}
