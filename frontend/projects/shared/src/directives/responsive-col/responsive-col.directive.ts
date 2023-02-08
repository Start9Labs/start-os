import { Directive, OnInit, Optional } from '@angular/core'
import { TuiDestroyService } from '@taiga-ui/cdk'
import {
  ResponsiveColViewportDirective,
  Step,
} from './responsive-col-viewport.directive'
import { IonCol } from '@ionic/angular'
import { takeUntil } from 'rxjs'

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
      this.col.sizeLg = this.size[size] || this.size.lg
      this.col.sizeMd = this.size[size] || this.size.md
      this.col.sizeSm = this.size[size] || this.size.sm
      this.col.sizeXl = this.size[size] || this.size.xl
      this.col.sizeXs = this.size[size] || this.size.xs
    })
  }

  ngOnInit() {
    this.size.lg = this.col.sizeLg
    this.size.md = this.col.sizeMd
    this.size.sm = this.col.sizeSm
    this.size.xl = this.col.sizeXl
    this.size.xs = this.col.sizeXs
  }
}
