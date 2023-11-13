import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter, first } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  ServerNotifications,
  NotificationLevel,
  ServerNotification,
} from 'src/app/services/api/api.types'
import { BackupReportComponent } from '../../modals/backup-report/backup-report.component'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'notifications',
  templateUrl: 'notifications.page.html',
  styleUrls: ['notifications.page.scss'],
})
export class NotificationsPage {
  loading = true
  notifications: ServerNotifications = []
  beforeCursor?: number
  needInfinite = false
  fromToast = !!this.route.snapshot.queryParamMap.get('toast')
  readonly perPage = 40
  readonly packageData$ = this.patch.watch$('package-data').pipe(first())

  constructor(
    private readonly embassyApi: ApiService,
    private readonly loader: LoadingService,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async ngOnInit() {
    this.notifications = await this.getNotifications()
    this.loading = false
  }

  async doInfinite(e: any) {
    const notifications = await this.getNotifications()
    this.notifications = this.notifications.concat(notifications)
    e.target.complete()
  }

  async getNotifications(): Promise<ServerNotifications> {
    try {
      const notifications = await this.embassyApi.getNotifications({
        before: this.beforeCursor,
        limit: this.perPage,
      })

      if (!notifications) return []

      this.beforeCursor = notifications[notifications.length - 1]?.id
      this.needInfinite = notifications.length >= this.perPage

      return notifications
    } catch (e: any) {
      this.errorService.handleError(e)
    }

    return []
  }

  async delete(id: number, index: number): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.embassyApi.deleteNotification({ id })
      this.notifications.splice(index, 1)
      this.beforeCursor = this.notifications[this.notifications.length - 1]?.id
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  presentAlertDeleteAll() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Delete All?',
        size: 's',
        data: {
          content: 'Are you sure you want to delete all notifications?',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.deleteAll())
  }

  async viewBackupReport(notification: ServerNotification<1>) {
    this.dialogs
      .open(new PolymorpheusComponent(BackupReportComponent), {
        label: 'Backup Report',
        data: {
          report: notification.data,
          timestamp: notification['created-at'],
        },
      })
      .subscribe()
  }

  viewFullMessage(label: string, message: string) {
    this.dialogs.open(message, { label }).subscribe()
  }

  truncate(message: string): string {
    return message.length <= 240 ? message : '...' + message.substr(-240)
  }

  getColor({ level }: ServerNotification<number>): string {
    switch (level) {
      case NotificationLevel.Info:
        return 'primary'
      case NotificationLevel.Success:
        return 'success'
      case NotificationLevel.Warning:
        return 'warning'
      case NotificationLevel.Error:
        return 'danger'
      default:
        return ''
    }
  }

  private async deleteAll(): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.embassyApi.deleteAllNotifications({
        before: this.notifications[0].id + 1,
      })
      this.notifications = []
      this.beforeCursor = undefined
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
