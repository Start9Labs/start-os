import { Component, Input } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { PackageDataEntry, PackageMainStatus, PackageState } from 'src/app/services/patch-db/data-model'
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
  pkg: PackageDataEntry

  constructor (
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('package-data', this.pkgId, 'installed', 'status', 'main', 'status').subscribe(_ => {
        this.pkg = this.patch.data['package-data'][this.pkgId]
        this.render(this.pkg)
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

