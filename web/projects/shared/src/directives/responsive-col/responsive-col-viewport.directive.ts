import {
  Directive,
  ElementRef,
  Inject,
  InjectionToken,
  Input,
  NgZone,
} from '@angular/core'
import { ResizeObserverService } from '@ng-web-apis/resize-observer'
import { distinctUntilChanged, map, Observable } from 'rxjs'
import { tuiZonefree } from '@taiga-ui/cdk'

export type Step = 'xs' | 'sm' | 'md' | 'lg' | 'xl'

/**
 * Not exported:
 * https://github.com/ionic-team/ionic-framework/blob/main/core/src/utils/media.ts
 *
 * export const SIZE_TO_MEDIA: any = {
 *   xs: '(min-width: 0px)',
 *   sm: '(min-width: 576px)',
 *   md: '(min-width: 768px)',
 *   lg: '(min-width: 992px)',
 *   xl: '(min-width: 1200px)',
 * };
 */
export const BREAKPOINTS = new InjectionToken<readonly [number, Step][]>(
  'BREAKPOINTS',
  {
    factory: () => [
      [1200, 'xl'],
      [992, 'lg'],
      [768, 'md'],
      [576, 'sm'],
      [0, 'xs'],
    ],
  },
)

@Directive({
  selector: '[responsiveColViewport]',
  exportAs: 'viewport',
  providers: [ResizeObserverService],
})
export class ResponsiveColViewportDirective extends Observable<Step> {
  @Input()
  responsiveColViewport: Observable<Step> | '' = ''

  private readonly stream$ = this.resize$.pipe(
    map(() => this.elementRef.nativeElement.clientWidth),
    map(width => this.breakpoints.find(([step]) => width >= step)?.[1] || 'xs'),
    distinctUntilChanged(),
    tuiZonefree(this.zone),
  )

  constructor(
    @Inject(BREAKPOINTS)
    private readonly breakpoints: readonly [number, Step][],
    private readonly resize$: ResizeObserverService,
    private readonly elementRef: ElementRef<HTMLElement>,
    private readonly zone: NgZone,
  ) {
    super(subscriber =>
      (this.responsiveColViewport || this.stream$).subscribe(subscriber),
    )
  }
}
