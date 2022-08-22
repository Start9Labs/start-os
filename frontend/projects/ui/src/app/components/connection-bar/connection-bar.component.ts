import { ChangeDetectionStrategy, Component } from '@angular/core'
import { combineLatest, map, Observable, startWith, tap } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'connection-bar',
  templateUrl: './connection-bar.component.html',
  styleUrls: ['./connection-bar.component.scss'],
  // changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ConnectionBarComponent {
  private readonly websocket$ = this.connectionService.websocketConnected$

  readonly connection$: Observable<{
    message: string
    icon: string
    color: string
    dots: boolean
  }> = combineLatest([
    this.connectionService.networkConnected$,
    this.websocket$,
  ]).pipe(
    map(([network, websocket]) => {
      if (!network)
        return {
          message: 'No Internet',
          icon: 'cloud-offline-outline',
          color: 'dark',
          dots: false,
        }
      if (!websocket)
        return {
          message: 'Connecting',
          icon: 'cloud-offline-outline',
          color: 'warning',
          dots: true,
        }

      return {
        message: 'Connected',
        icon: 'cloud-done',
        color: 'success',
        dots: false,
      }
    }),
  )

  constructor(
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDbService,
  ) {}
}
