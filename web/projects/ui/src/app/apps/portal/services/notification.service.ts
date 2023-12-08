import { inject, Injectable } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import {
  NotificationLevel,
  ServerNotification,
  ServerNotifications,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { REPORT } from '../modals/report.component'
import { firstValueFrom, merge, shareReplay, Subject } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class NotificationService {
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly localUnreadCount$ = new Subject<number>()

  readonly unreadCount$ = merge(
    this.patch.watch$('server-info', 'unreadNotifications', 'count'),
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
      case NotificationLevel.Info:
        return 'var(--tui-info-fill)'
      case NotificationLevel.Success:
        return 'var(--tui-success-fill)'
      case NotificationLevel.Warning:
        return 'var(--tui-warning-fill)'
      case NotificationLevel.Error:
        return 'var(--tui-error-fill)'
      default:
        return ''
    }
  }

  getIcon(notification: ServerNotification<number>): string {
    switch (notification.level) {
      case NotificationLevel.Info:
        return 'tuiIconInfo'
      case NotificationLevel.Success:
        return 'tuiIconCheckCircle'
      case NotificationLevel.Warning:
      case NotificationLevel.Error:
        return 'tuiIconAlertCircle'
      default:
        return ''
    }
  }

  viewFull(notification: ServerNotification<number>) {
    this.dialogs
      .open(notification.message, { label: notification.title })
      .subscribe()
  }

  viewReport(notification: ServerNotification<number>) {
    this.dialogs
      .open(REPORT, {
        label: 'Backup Report',
        data: {
          report: notification.data,
          timestamp: notification['created-at'],
        },
      })
      .subscribe()
  }

  private async updateCount(toAdjust: number) {
    const currentCount = await firstValueFrom(this.unreadCount$)

    this.localUnreadCount$.next(Math.max(currentCount + toAdjust, 0))
  }
}
