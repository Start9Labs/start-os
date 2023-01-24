import { Directive, ElementRef, inject, Output } from '@angular/core'
import { ResizeObserverService } from '@ng-web-apis/resize-observer'
import {
  MUTATION_OBSERVER_INIT,
  MutationObserverService,
} from '@ng-web-apis/mutation-observer'
import { distinctUntilChanged, map, merge } from 'rxjs'

@Directive({
  selector: '[elasticContainer]',
  providers: [
    ResizeObserverService,
    MutationObserverService,
    {
      provide: MUTATION_OBSERVER_INIT,
      useValue: {
        childList: true,
        characterData: true,
        subtree: true,
      },
    },
  ],
})
export class ElasticContainerDirective {
  private readonly elementRef = inject(ElementRef)

  @Output()
  readonly elasticContainer = merge(
    inject(ResizeObserverService),
    inject(MutationObserverService),
  ).pipe(
    map(() => this.elementRef.nativeElement.clientHeight),
    distinctUntilChanged(),
  )
}
