import { Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { ConnectionStatus } from 'patch-db-client'

@Component({
  selector: 'status',
  templateUrl: './status.component.html',
  styleUrls: ['./status.component.scss'],
})
export class StatusComponent {
  @Input() pkg: PackageDataEntry
  @Input() connected: boolean
  @Input() size?: 'small' | 'medium' | 'large' = 'large'
  @Input() style?: string = 'regular'
  @Input() weight?: string = 'normal'
  display = ''
  color = ''
  showDots = false

  ngOnChanges () {
    const { display, color, showDots } = renderPkgStatus(this.pkg)
    this.display = display
    this.color = color
    this.showDots = showDots
  }
}

