import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
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
  page = 1
  needInfinite = false
  fromToast = false
  readonly perPage = 20

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

  async doRefresh (e: any) {
    this.page = 1
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
      notifications = await this.embassyApi.getNotifications({ page: this.page, 'per-page': this.perPage })
      this.needInfinite = notifications.length >= this.perPage
      this.page++
    } catch (e) {
      this.errToast.present(e)
    } finally {
      return notifications
    }
  }

  async remove (id: string, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Deleting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.deleteNotification({ id })
      this.notifications.splice(index, 1)
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
        handler: () => {
          console.log('retry backup')
        },
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

