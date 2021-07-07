import { Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
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
      this.patch.sequence$.subscribe(_ => {
        this.render(this.patch.data['package-data'][this.pkgId])
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  private render (pkg: PackageDataEntry) {
    const { display, color, showDots } = renderPkgStatus(pkg.state, pkg.installed.status)
    this.display = display
    this.color = color
    this.showDots = showDots
  }
}

