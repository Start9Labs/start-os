import { inject, Injectable } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { Subject } from 'rxjs'
import { ServerNotifications } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

const limit = 40

@Injectable({ providedIn: 'root' })
export class HeaderNotificationsService {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  hasMore = false

  private readonly notifications$ = new Subject<ServerNotifications>()

  getNotifications$() {
    return this.notifications$
  }

  async markSeen(id: number) {
    this.api
      .markSeenNotification({ id })
      .catch(e => this.errorService.handleError(e))
  }

  async markAllSeen(latestId: number) {
    this.api
      .markSeenAllNotifications({ before: latestId })
      .catch(e => this.errorService.handleError(e))
  }

  async delete(
    all: ServerNotifications,
    id: number,
    index: number,
  ): Promise<void> {
    this.api
      .deleteNotification({ id })
      .catch(e => this.errorService.handleError(e))
    all.splice(index, 1)
    this.notifications$.next(all)
  }

  async deleteAll(all: ServerNotifications) {
    this.api
      .deleteAllNotifications({ before: all[0].id + 1 })
      .catch(e => this.errorService.handleError(e))
    all.splice(0, all.length)
    this.notifications$.next(all)
  }

  async getMore(all: ServerNotifications): Promise<ServerNotifications> {
    try {
      const result = await this.api.getNotifications({
        before: all[all.length - 1].id,
      })

      this.hasMore = result?.length === limit

      this.notifications$.next(all.concat(result))
    } catch (e: any) {
      this.errorService.handleError(e)
    }

    return []
  }
}
