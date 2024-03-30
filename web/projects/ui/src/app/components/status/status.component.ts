import { Component, Input } from '@angular/core'
import { ConnectionService } from 'src/app/services/connection.service'
import { InstallingInfo } from 'src/app/services/patch-db/data-model'
import {
  PrimaryRendering,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'status',
  templateUrl: './status.component.html',
  styleUrls: ['./status.component.scss'],
})
export class StatusComponent {
  PR = PrimaryRendering

  @Input() rendering!: StatusRendering
  @Input() size?: string
  @Input() style?: string = 'regular'
  @Input() weight?: string = 'normal'
  @Input() installingInfo?: InstallingInfo
  @Input() sigtermTimeout?: string | null = null

  readonly connected$ = this.connectionService.connected$

  constructor(private readonly connectionService: ConnectionService) {}
}
