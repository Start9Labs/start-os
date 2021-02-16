import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { LoaderService } from 'src/app/services/loader.service'
import { ServerNotification, ServerNotifications } from 'src/app/services/api/api-types'
import { AlertController } from '@ionic/angular'

@Component({
  selector: 'notifications',
  templateUrl: 'notifications.page.html',
  styleUrls: ['notifications.page.scss'],
})
export class NotificationsPage {
  error = ''
  loading = true
  notifications: ServerNotifications = []
  page = 1
  needInfinite = false
  readonly perPage = 20

  constructor (
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly alertCtrl: AlertController,
  ) { }

  async ngOnInit () {
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
      notifications = await this.apiService.getNotifications({ page: this.page, 'per-page': this.perPage })
      this.needInfinite = notifications.length >= this.perPage
      this.page++
      this.error = ''
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      return notifications
    }
  }

  async remove (id: string, index: number): Promise<void> {
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(
      this.apiService.deleteNotification({ id }).then(() => {
        this.notifications.splice(index, 1)
        this.error = ''
      }),
    ).catch(e => {
      console.error(e)
      this.error = e.message
    })
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

