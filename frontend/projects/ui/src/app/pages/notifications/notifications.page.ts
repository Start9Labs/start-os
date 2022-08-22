import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  ServerNotifications,
  NotificationLevel,
  ServerNotification,
} from 'src/app/services/api/api.types'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService } from '@start9labs/shared'
import { BackupReportPage } from 'src/app/modals/backup-report/backup-report.page'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

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
  fromToast = false
  readonly perPage = 40
  readonly packageData$ = this.patch.watch$('package-data')

  constructor(
    private readonly embassyApi: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDbService,
  ) {}

  async ngOnInit() {
    this.fromToast = !!this.route.snapshot.queryParamMap.get('toast')
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
      this.errToast.present(e)
    }

    return []
  }

  async delete(id: number, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    try {
      await this.embassyApi.deleteNotification({ id })
      this.notifications.splice(index, 1)
      this.beforeCursor = this.notifications[this.notifications.length - 1]?.id
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async presentAlertDeleteAll() {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Delete All?',
      message: 'Are you sure you want to delete all notifications?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.deleteAll()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async viewBackupReport(notification: ServerNotification<1>) {
    const modal = await this.modalCtrl.create({
      component: BackupReportPage,
      componentProps: {
        report: notification.data,
        timestamp: notification['created-at'],
      },
    })
    await modal.present()
  }

  async viewFullMessage(header: string, message: string) {
    const alert = await this.alertCtrl.create({
      header,
      message,
      cssClass: 'notification-detail-alert',
      buttons: [
        {
          text: `OK`,
          handler: () => {
            alert.dismiss()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
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
    const loader = await this.loadingCtrl.create({
      message: 'Deleting...',
    })
    await loader.present()

    try {
      await this.embassyApi.deleteAllNotifications({
        before: this.notifications[0].id + 1,
      })
      this.notifications = []
      this.beforeCursor = undefined
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
