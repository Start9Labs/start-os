import { Pipe, PipeTransform } from '@angular/core'
import { NotificationLevel, ServerNotification } from '../services/api/api-types'

@Pipe({
  name: 'notificationColor',
})
export class NotificationColorPipe implements PipeTransform {
  transform (notification: ServerNotification<any>): string {
    const level = notification.level
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
}
