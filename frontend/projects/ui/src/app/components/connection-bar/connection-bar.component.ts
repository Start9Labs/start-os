import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, startWith } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'connection-bar',
  templateUrl: './connection-bar.component.html',
  styleUrls: ['./connection-bar.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ConnectionBarComponent {
  private readonly websocket$ = this.connectionService.websocketConnected$

  readonly connection$: Observable<{
    message: string
    color: string
    icon: string
    dots: boolean
  }> = combineLatest([
    this.connectionService.networkConnected$,
    this.websocket$.pipe(startWith(false)),
    this.patch
      .watch$('server-info', 'status-info')
      .pipe(startWith({ restarting: false, 'shutting-down': false })),
  ]).pipe(
    map(([network, websocket, status]) => {
      if (!network)
        return {
          message: 'No Internet',
          color: 'danger',
          icon: 'cloud-offline-outline',
          dots: false,
        }
      if (!websocket)
        return {
          message: 'Connecting',
          color: 'warning',
          icon: 'cloud-offline-outline',
          dots: true,
        }
      if (status['shutting-down'])
        return {
          message: 'Shutting Down',
          color: 'dark',
          icon: 'power',
          dots: true,
        }
      if (status.restarting)
        return {
          message: 'Restarting',
          color: 'dark',
          icon: 'power',
          dots: true,
        }

      return {
        message: 'Connected',
        color: 'success',
        icon: 'cloud-done',
        dots: false,
      }
    }),
  )

  constructor(
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}
