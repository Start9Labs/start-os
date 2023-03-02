import { Directive, OnInit, Optional } from '@angular/core'
import { TuiDestroyService } from '@taiga-ui/cdk'
import {
  ResponsiveColViewportDirective,
  Step,
} from './responsive-col-viewport.directive'
import { IonCol } from '@ionic/angular'
import { takeUntil } from 'rxjs'

const SIZE: readonly Step[] = ['xl', 'lg', 'md', 'sm', 'xs']

@Directive({
  selector: 'ion-col[responsiveCol]',
  providers: [TuiDestroyService],
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
