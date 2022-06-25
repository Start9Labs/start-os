import { Component, Input } from '@angular/core'
import { ConnectionService } from 'src/app/services/connection.service'
import { InstallProgress } from 'src/app/services/patch-db/data-model'
import {
  PrimaryRendering,
  PrimaryStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'status',
  templateUrl: './status.component.html',
  styleUrls: ['./status.component.scss'],
})
export class StatusComponent {
  PS = PrimaryStatus
  PR = PrimaryRendering

  @Input() rendering: StatusRendering
  @Input() size?: string
  @Input() style?: string = 'regular'
  @Input() weight?: string = 'normal'
  @Input() installProgress?: InstallProgress
  @Input() sigtermTimeout?: string | null = null

  disconnected$ = this.connectionService.watchDisconnected$()

  constructor(private readonly connectionService: ConnectionService) {}
}
