import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ConnectionService } from 'src/app/services/connection.service'
import { PkgInfo } from 'src/app/types/pkg-info'

@Component({
  selector: 'app-list-icon',
  templateUrl: 'app-list-icon.component.html',
  styleUrls: ['app-list-icon.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListIconComponent {
  @Input({ required: true })
  pkg!: PkgInfo

  readonly connected$ = this.connectionService.connected$

  constructor(private readonly connectionService: ConnectionService) {}
}
