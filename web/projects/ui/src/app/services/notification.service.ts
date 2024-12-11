import { inject, Injectable } from '@angular/core'
import { ErrorService, MARKDOWN } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom, merge, shareReplay, Subject } from 'rxjs'
import { REPORT } from 'src/app/components/report.component'
import {
  ServerNotification,
  ServerNotifications,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class NotificationService {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly localUnreadCount$ = new Subject<number>()

  readonly unreadCount$ = merge(
    this.patch.watch$('serverInfo', 'unreadNotifications', 'count'),
    this.localUnreadCount$,
  ).pipe(shareReplay(1))

  async markSeen(notifications: ServerNotifications) {
    const ids = notifications.filter(n => !n.read).map(n => n.id)

    this.updateCount(-ids.length)

    this.api
      .markSeenNotifications({ ids })
      .catch(e => this.errorService.handleError(e))
  }

  async markSeenAll(latestId: number) {
    this.localUnreadCount$.next(0)

    this.api
      .markSeenAllNotifications({ before: latestId })
      .catch(e => this.errorService.handleError(e))
  }

  async markUnseen(notifications: ServerNotifications) {
    const ids = notifications.filter(n => n.read).map(n => n.id)

    this.updateCount(ids.length)

    this.api
      .markUnseenNotifications({ ids })
      .catch(e => this.errorService.handleError(e))
  }

  async remove(notifications: ServerNotifications): Promise<void> {
    this.updateCount(-notifications.filter(n => !n.read).length)

    this.api
      .deleteNotifications({ ids: notifications.map(n => n.id) })
      .catch(e => this.errorService.handleError(e))
  }

  getColor(notification: ServerNotification<number>): string {
    switch (notification.level) {
      case 'info':
        return 'var(--tui-status-info)'
      case 'success':
        return 'var(--tui-status-positive)'
      case 'warning':
        return 'var(--tui-status-warning)'
      case 'error':
        return 'var(--tui-status-negative)'
      default:
        return ''
    }
  }

  getIcon(notification: ServerNotification<number>): string {
    switch (notification.level) {
      case 'info':
        return '@tui.info'
      case 'success':
        return '@tui.circle-check'
      case 'warning':
      case 'error':
        return '@tui.circle-alert'
      default:
        return ''
    }
  }

  viewModal(
    { data, createdAt, code, title, message }: ServerNotification<number>,
    full = false,
  ) {
    const label = full || code === 2 ? title : 'Backup Report'
    const content = code === 1 ? REPORT : MARKDOWN

    this.dialogs
      .open(full ? message : content, {
        label,
        data: {
          content: data,
          timestamp: createdAt,
        },
      })
      .subscribe()
  }

  private async updateCount(toAdjust: number) {
    const currentCount = await firstValueFrom(this.unreadCount$)

    this.localUnreadCount$.next(Math.max(currentCount + toAdjust, 0))
  }
}
