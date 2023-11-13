import { inject, Pipe, PipeTransform } from '@angular/core'
import { NotificationsService } from '../services/notifications.service'
import { Observable } from 'rxjs'

@Pipe({
  name: 'toNotifications',
  standalone: true,
})
export class ToNotificationsPipe implements PipeTransform {
  readonly notifications = inject(NotificationsService)

  transform(id: string): Observable<number> {
    return this.notifications.getNotifications(id)
  }
}
