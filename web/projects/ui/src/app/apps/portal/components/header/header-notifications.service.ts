import { inject, Injectable } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { BehaviorSubject, Observable } from 'rxjs'
import { ServerNotifications } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

const limit = 40

@Injectable({ providedIn: 'root' })
export class HeaderNotificationsService {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly stream$ = new BehaviorSubject<ServerNotifications | null>(
    null,
  )

  hasMore = false

  get current(): ServerNotifications {
    return this.stream$.value ?? []
  }

  get notification$(): Observable<ServerNotifications | null> {
    return this.stream$
  }

  async delete(id: number, index: number): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteNotification({ id })
      this.stream$.next(this.current.splice(index, 1) || null)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async clear() {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteAllNotifications({ before: this.current[0].id + 1 })
      this.stream$.next([])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async load() {
    this.hasMore = false
    this.stream$.next(this.current.concat(await this.getMore()))
  }

  private async getMore(): Promise<ServerNotifications> {
    const index = this.current.length
    const before = this.current[index]?.id

    try {
      const result = await this.api.getNotifications({ before, limit })

      this.hasMore = result?.length === limit

      return result || []
    } catch (e: any) {
      this.errorService.handleError(e)
    }

    return []
  }
}
