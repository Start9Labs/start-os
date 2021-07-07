import { Component, Input } from '@angular/core'
import { combineLatest } from 'rxjs'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'status',
  templateUrl: './status.component.html',
  styleUrls: ['./status.component.scss'],
})
export class StatusComponent {
  @Input() pkgId: string
  @Input() size?: 'small' | 'medium' | 'large' = 'large'
  @Input() style?: string = 'regular'
  @Input() weight?: string = 'normal'
  display = ''
  color = ''
  showDots = false
  subs = []

  constructor (
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.subs = [
      combineLatest([
        this.patch.watch$('package-data', this.pkgId, 'state'),
        this.patch.watch$('package-data', this.pkgId, 'installed', 'status'),
      ])
      .subscribe(([state, status]) => {
        const { display, color, showDots } = renderPkgStatus(state, status)
        this.display = display
        this.color = color
        this.showDots = showDots
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }
}

