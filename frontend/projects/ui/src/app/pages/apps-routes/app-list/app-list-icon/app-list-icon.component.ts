import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { map } from 'rxjs/operators'
import { ConnectionService } from 'src/app/services/connection.service'
import { PkgInfo } from 'src/app/util/get-package-info'

@Component({
  selector: 'app-list-icon',
  templateUrl: 'app-list-icon.component.html',
  styleUrls: ['app-list-icon.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListIconComponent {
  @Input()
  pkg: PkgInfo

  color$ = this.connectionService.watchDisconnected$().pipe(
    map(disconnected => {
      return disconnected
        ? 'var(--ion-color-dark)'
        : 'var(--ion-color-' + this.pkg.primaryRendering.color + ')'
    }),
  )

  constructor(private readonly connectionService: ConnectionService) {}
}
