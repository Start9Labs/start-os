import { inject, Injectable } from '@angular/core'
import { ReplaySubject } from 'rxjs'
import { StorageService } from './storage.service'

const SHOW_DEV_TOOLS = 'SHOW_DEV_TOOLS'

@Injectable({
  providedIn: 'root',
})
export class ClientStorageService {
  private readonly storage = inject(StorageService)
  readonly showDevTools$ = new ReplaySubject<boolean>(1)

  init() {
    this.showDevTools$.next(!!this.storage.get(SHOW_DEV_TOOLS))
  }

  toggleShowDevTools(): boolean {
    const newVal = !this.storage.get(SHOW_DEV_TOOLS)
    this.storage.set(SHOW_DEV_TOOLS, newVal)
    this.showDevTools$.next(newVal)
    return newVal
  }
}
