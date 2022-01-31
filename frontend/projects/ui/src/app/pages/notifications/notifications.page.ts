import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ServerNotification, ServerNotifications } from 'src/app/services/api/api.types'
import { AlertController, LoadingController, ModalController } from '@ionic/angular'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { BackupReportPage } from 'src/app/modals/backup-report/backup-report.page'

@Component({
  selector: 'notifications',
  templateUrl: 'notifications.page.html',
  styleUrls: ['notifications.page.scss'],
})
export class NotificationsPage {
  loading = true
  notifications: ServerNotifications = []
  beforeCursor: number
  needInfinite = false
  fromToast = false
  readonly perPage = 40

  constructor (
    private readonly embassyApi: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly route: ActivatedRoute,
  ) { }

  async ngOnInit () {
    this.fromToast = !!this.route.snapshot.queryParamMap.get('toast')
    this.notifications = await this.getNotifications()
    this.loading = false
  }

  async doInfinite (e: any) {
    const notifications = await this.getNotifications()
    this.notifications = this.notifications.concat(notifications)
    e.target.complete()
  }

  async getNotifications (): Promise<ServerNotifications> {
    let notifications: ServerNotifications = []
    try {
      notifications = await this.embassyApi.getNotifications({ before: this.beforeCursor, limit: this.perPage })
      this.beforeCursor = notifications[notifications.length - 1]?.id
      this.needInfinite = notifications.length >= this.perPage
    } catch (e) {
      this.errToast.present(e)
    } finally {
      return notifications
    }
  }

  async delete (id: number, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.deleteNotification({ id })
      this.notifications.splice(index, 1)
      this.beforeCursor = this.notifications[this.notifications.length - 1]?.id
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async presentAlertDeleteAll () {
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
          cssClass: 'enter-click',
          handler: () => {
            this.deleteAll()
          },
        },
      ],
    })
    await alert.present()
  }

  async viewBackupReport (notification: ServerNotification<1>) {
    const modal = await this.modalCtrl.create({
      component: BackupReportPage,
      componentProps: {
        report: notification.data,
        timestamp: notification['created-at'],
      },
    })
    await modal.present()
  }

  async viewFullMessage (title: string, message: string) {
    const alert = await this.alertCtrl.create({
      header: title,
      message: message,
      cssClass: 'wider-alert',
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

  private async deleteAll (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.deleteAllNotifications({ before: this.notifications[0].id })
      this.notifications = []
      this.beforeCursor = undefined
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

