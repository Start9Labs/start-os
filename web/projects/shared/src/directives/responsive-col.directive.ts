import {
  Directive,
  OnInit,
  Optional,
  ElementRef,
  Inject,
  InjectionToken,
  Input,
  NgZone,
} from '@angular/core'
import { ResizeObserverService } from '@ng-web-apis/resize-observer'
import { distinctUntilChanged, map, Observable } from 'rxjs'
import { tuiZonefree, TuiDestroyService } from '@taiga-ui/cdk'
import { IonCol } from '@ionic/angular'
import { takeUntil } from 'rxjs'

export type Step = 'xs' | 'sm' | 'md' | 'lg' | 'xl'

const SIZE: readonly Step[] = ['xl', 'lg', 'md', 'sm', 'xs']

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
  standalone: true,
})
export class ResponsiveColViewportDirective extends Observable<Step> {
  @Input()
  responsiveColViewport: Observable<Step> | '' = ''

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

  private readonly stream$ = this.resize$.pipe(
    map(() => this.elementRef.nativeElement.clientWidth),
    map(width => this.breakpoints.find(([step]) => width >= step)?.[1] || 'xs'),
    distinctUntilChanged(),
    tuiZonefree(this.zone),
  )
}

@Directive({
  selector: 'ion-col[responsiveCol]',
  providers: [TuiDestroyService],
  standalone: true,
})
export class ResponsiveColDirective implements OnInit {
  readonly size: Record<Step, string | undefined> = {
    xs: '12',
    sm: '6',
    md: '4',
    lg: '3',
    xl: '2',
  }

  constructor(
    @Optional()
    viewport$: ResponsiveColViewportDirective | null,
    destroy$: TuiDestroyService,
    private readonly col: IonCol,
  ) {
    viewport$?.pipe(takeUntil(destroy$)).subscribe(size => {
      const max = this.size[size] || this.findMax(size)

      this.col.sizeLg = max
      this.col.sizeMd = max
      this.col.sizeSm = max
      this.col.sizeXl = max
      this.col.sizeXs = max
    })
  }

  ngOnInit() {
    this.size.lg = this.col.sizeLg
    this.size.md = this.col.sizeMd
    this.size.sm = this.col.sizeSm
    this.size.xl = this.col.sizeXl
    this.size.xs = this.col.sizeXs
  }

  private findMax(current: Step): string | undefined {
    const start = SIZE.indexOf(current) - 1
    const max = SIZE.find((size, i) => i > start && this.size[size]) || current

    return this.size[max]
  }
}
