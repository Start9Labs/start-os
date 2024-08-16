import { TuiIcon } from '@taiga-ui/core'
import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, startWith } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import { NetworkService } from 'src/app/services/network.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: 'header-connection',
  template: `
    <ng-content />
    @if (connection$ | async; as connection) {
      <!-- data-connection is used to display color indicator in the header through :has() -->
      <tui-icon
        [icon]="connection.icon"
        [style.color]="connection.color"
        [style.font-size.em]="1.5"
        [attr.data-connection]="connection.status"
      />
      {{ connection.message }}
    }
  `,
  styles: [
    `
      :host {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        padding: 0 2rem;
      }

      :host-context(tui-root._mobile) {
        display: none;
        font-size: 1rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, AsyncPipe],
})
export class HeaderConnectionComponent {
  readonly connection$: Observable<{
    message: string
    color: string
    icon: string
    status: string
  }> = combineLatest([
    inject(NetworkService),
    inject(ConnectionService),
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'statusInfo')
      .pipe(startWith({ restarting: false, shuttingDown: false })),
  ]).pipe(
    map(([network, websocket, status]) => {
      if (!network)
        return {
          message: 'No Internet',
          color: 'var(--tui-status-negative)',
          icon: '@tui.cloud-off',
          status: 'error',
        }
      if (!websocket)
        return {
          message: 'Connecting',
          color: 'var(--tui-status-warning)',
          icon: '@tui.cloud-off',
          status: 'warning',
        }
      if (status.shuttingDown)
        return {
          message: 'Shutting Down',
          color: 'var(--tui-status-neutral)',
          icon: '@tui.power',
          status: 'neutral',
        }
      if (status.restarting)
        return {
          message: 'Restarting',
          color: 'var(--tui-status-neutral)',
          icon: '@tui.power',
          status: 'neutral',
        }

      return {
        message: 'Connected',
        color: 'var(--tui-status-positive)',
        icon: '@tui.cloud',
        status: 'success',
      }
    }),
  )
}
