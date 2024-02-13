import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiIconModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, Observable, startWith } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
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
      ></tui-icon>
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
  imports: [TuiIconModule, AsyncPipe],
})
export class HeaderConnectionComponent {
  readonly connection$: Observable<{
    message: string
    color: string
    icon: string
    status: string
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
          status: 'error',
        }
      if (!websocket)
        return {
          message: 'Connecting',
          color: 'var(--tui-warning-fill)',
          icon: 'tuiIconCloudOff',
          status: 'warning',
        }
      if (status['shutting-down'])
        return {
          message: 'Shutting Down',
          color: 'var(--tui-neutral-fill)',
          icon: 'tuiIconPower',
          status: 'neutral',
        }
      if (status.restarting)
        return {
          message: 'Restarting',
          color: 'var(--tui-neutral-fill)',
          icon: 'tuiIconPower',
          status: 'neutral',
        }

      return {
        message: 'Connected',
        color: 'var(--tui-success-fill)',
        icon: 'tuiIconCloud',
        status: 'success',
      }
    }),
  )
}
