import { ChangeDetectionStrategy, Component } from '@angular/core'
import { combineLatest, map, Observable, startWith } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'

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
  ]).pipe(
    map(([network, websocket]) => {
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

      return {
        message: 'Connected',
        color: 'success',
        icon: 'cloud-done',
        dots: false,
      }
    }),
  )

  constructor(private readonly connectionService: ConnectionService) {}
}
