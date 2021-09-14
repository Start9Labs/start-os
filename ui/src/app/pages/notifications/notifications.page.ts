import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ServerNotification, ServerNotifications } from 'src/app/services/api/api.types'
import { AlertController, LoadingController } from '@ionic/angular'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService } from 'src/app/services/error-toast.service'

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
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly route: ActivatedRoute,
  ) { }

  async ngOnInit () {
    this.fromToast = !!this.route.snapshot.queryParamMap.get('toast')
    this.notifications = await this.getNotifications()
    this.loading = false
  }

  async refresh (e: any) {
    this.beforeCursor = undefined
    this.notifications = await this.getNotifications(),
    e.target.complete()
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

  async deleteAll (): Promise<void> {
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

  async viewBackupReport (notification: ServerNotification<1>) {
    const data = notification.data

    const embassyFailed = !!data.server.error
    const packagesFailed = Object.entries(data.packages).some(([_, val]) => val.error)

    let message: string

    if (embassyFailed || packagesFailed) {
      message = 'There was an issue backing up one or more items. Click "Retry" to retry ONLY the items that failed.'
    } else {
      message = 'All items were successfully backed up'
    }

    const buttons: any[] = [ // why can't I import AlertButton?
      {
        text: 'Dismiss',
        role: 'cancel',
      },
    ]

    if (embassyFailed || packagesFailed) {
      buttons.push({
        text: 'Retry',
      })
    }


    const alert = await this.alertCtrl.create({
      header: 'Backup Report',
      message,
      buttons,
    })

    await alert.present()
  }
}

