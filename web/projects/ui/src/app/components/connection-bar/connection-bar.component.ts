import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, startWith } from 'rxjs'
import { NetworkService } from 'src/app/services/network.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'connection-bar',
  templateUrl: './connection-bar.component.html',
  styleUrls: ['./connection-bar.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ConnectionBarComponent {
  readonly connection$: Observable<{
    message: string
    color: string
    icon: string
    dots: boolean
  }> = combineLatest([
    this.network$,
    this.state$.pipe(map(Boolean)),
    this.patch
      .watch$('serverInfo', 'statusInfo')
      .pipe(startWith({ restarting: false, shuttingDown: false })),
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
      if (status.shuttingDown)
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
    private readonly network$: NetworkService,
    private readonly state$: StateService,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}
