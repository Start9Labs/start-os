import { Component } from '@angular/core'
import { ServerModel, S9Notification } from 'src/app/models/server-model'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/util/misc.util'
import { LoaderService } from 'src/app/services/loader.service'
@Component({
  selector: 'notifications',
  templateUrl: 'notifications.page.html',
  styleUrls: ['notifications.page.scss'],
})
export class NotificationsPage {
  error = ''
  loading = true
  notifications: S9Notification[] = []
  page = 1
  needInfinite = false
  readonly perPage = 20

  constructor (
    private readonly serverModel: ServerModel,
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
  ) { }

  async ngOnInit () {
    const [notifications] = await Promise.all([
      this.getNotifications(),
      pauseFor(600),
    ])
    this.notifications = notifications
    this.serverModel.update({ badge: 0 })
    this.loading = false
  }

  async doRefresh (e: any) {
    this.page = 1
    await Promise.all([
      this.getNotifications(),
      pauseFor(600),
    ])
    e.target.complete()
  }

  async doInfinite (e: any) {
    const notifications = await this.getNotifications()
    this.notifications = this.notifications.concat(notifications)
    e.target.complete()
  }

  async getNotifications (): Promise<S9Notification[]> {
    let notifications: S9Notification[] = []
    try {
      notifications = await this.apiService.getNotifications(this.page, this.perPage)
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

  getColor (notification: S9Notification): string {
    const char = notification.code.charAt(0)
    switch (char) {
      case '0':
        return 'primary'
      case '1':
        return 'success'
      case '2':
        return 'warning'
      case '3':
        return 'danger'
      default:
        return ''
    }
  }

  async remove (notificationId: string, index: number): Promise<void> {
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(
      this.apiService.deleteNotification(notificationId).then(() => {
        this.notifications.splice(index, 1)
        this.error = ''
      }),
    ).catch(e => {
      console.error(e)
      this.error = e.message
    })
  }
}

