import { inject, InjectionToken } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, startWith } from 'rxjs'
import { ConnectionService } from './connection.service'
import { NetworkService } from './network.service'
import { DataModel } from './patch-db/data-model'
import { i18nKey } from '@start9labs/shared'

export const STATUS = new InjectionToken('', {
  factory: () =>
    toSignal(
      combineLatest({
        network: inject(NetworkService),
        websocket: inject(ConnectionService),
        status: inject<PatchDB<DataModel>>(PatchDB)
          .watch$('serverInfo', 'statusInfo')
          .pipe(startWith({ restarting: false, shuttingDown: false })),
      }).pipe(
        map(({ network, websocket, status }) => {
          if (!network) return OFFLINE
          if (!websocket) return CONNECTING
          if (status.shuttingDown) return SHUTTING_DOWN
          if (status.restarting) return RESTARTING

          return CONNECTED
        }),
      ),
      { initialValue: CONNECTING },
    ),
})

const OFFLINE: ServerStatus = {
  message: 'No Internet',
  color: 'var(--tui-status-negative)',
  icon: '@tui.cloud-off',
  status: 'error',
}
const CONNECTING: ServerStatus = {
  message: 'Connecting',
  color: 'var(--tui-status-warning)',
  icon: '@tui.cloud-off',
  status: 'warning',
}
const SHUTTING_DOWN: ServerStatus = {
  message: 'Shutting down',
  color: 'var(--tui-status-neutral)',
  icon: '@tui.power',
  status: 'neutral',
}
const RESTARTING: ServerStatus = {
  message: 'Restarting',
  color: 'var(--tui-status-neutral)',
  icon: '@tui.power',
  status: 'neutral',
}
const CONNECTED: ServerStatus = {
  message: 'Connected',
  color: 'var(--tui-status-positive)',
  icon: '@tui.cloud',
  status: 'success',
}

type ServerStatus = {
  message: i18nKey
  color: string
  icon: string
  status: 'error' | 'warning' | 'neutral' | 'success'
}
