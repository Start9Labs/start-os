import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiIconModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, startWith } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { AsyncPipe } from '@angular/common'

@Component({
  standalone: true,
  selector: 'header-connection',
  template: `
    @if (connection$ | async; as connection) {
      <tui-icon
        [title]="connection.message"
        [icon]="connection.icon"
        [style.color]="connection.color"
        [style.margin.rem]="0.5"
      ></tui-icon>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIconModule, AsyncPipe],
})
export class HeaderConnectionComponent {
  readonly connection$: Observable<{
    message: string
    color: string
    icon: string
  }> = combineLatest([
    inject(ConnectionService).networkConnected$,
    inject(ConnectionService).websocketConnected$.pipe(startWith(false)),
    inject(PatchDB<DataModel>)
      .watch$('server-info', 'status-info')
      .pipe(startWith({ restarting: false, 'shutting-down': false })),
  ]).pipe(
    map(([network, websocket, status]) => {
      if (!network)
        return {
          message: 'No Internet',
          color: 'var(--tui-error-fill)',
          icon: 'tuiIconCloudOff',
        }
      if (!websocket)
        return {
          message: 'Connecting',
          color: 'var(--tui-warning-fill)',
          icon: 'tuiIconCloudOff',
        }
      if (status['shutting-down'])
        return {
          message: 'Shutting Down',
          color: 'var(--tui-neutral-fill)',
          icon: 'tuiIconPower',
        }
      if (status.restarting)
        return {
          message: 'Restarting',
          color: 'var(--tui-neutral-fill)',
          icon: 'tuiIconPower',
        }

      return {
        message: 'Connected',
        color: 'var(--tui-success-fill)',
        icon: 'tuiIconCloud',
      }
    }),
  )
}
