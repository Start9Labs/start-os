import { Component, Input } from '@angular/core'
import { AppStatus } from 'src/app/models/app-model'
import { ServerStatus } from 'src/app/models/server-model'
import { ServerStatusRendering, AppStatusRendering } from '../../util/status-rendering'

@Component({
  selector: 'status',
  templateUrl: './status.component.html',
  styleUrls: ['./status.component.scss'],
})
export class StatusComponent {
  @Input() appStatus?: AppStatus
  @Input() serverStatus?: ServerStatus
  @Input() size: 'small' | 'medium' | 'large' | 'italics-small' | 'bold-large' = 'large'
  @Input() text: string = ''
  color: string
  display: string
  showDots: boolean
  style = ''

  ngOnChanges () {
    if (this.serverStatus) {
      this.handleServerStatus()
    } else if (this.appStatus) {
      this.handleAppStatus()
    }
  }

  handleServerStatus () {
    let res = ServerStatusRendering[this.serverStatus]
    if (!res) {
      console.warn(`Received invalid server status from the server: `, this.serverStatus)
      res = ServerStatusRendering[ServerStatus.UNKNOWN]
    }

    const { display, color, showDots } = res
    this.display = display
    this.color = color
    this.showDots = showDots
  }

  handleAppStatus () {
    let res = AppStatusRendering[this.appStatus]
    if (!res) {
      console.warn(`Received invalid app status from the server: `, this.appStatus)
      res = AppStatusRendering[AppStatus.UNKNOWN]
    }

    const { display, color, showDots, style } = res
    this.display = display + this.text
    this.color = color
    this.showDots = showDots
    this.style = style
  }
}

