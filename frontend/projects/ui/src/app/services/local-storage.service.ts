import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { BehaviorSubject } from 'rxjs'
const SHOW_DEV_TOOLS = 'SHOW_DEV_TOOLS'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageService {
  showDevTools$: BehaviorSubject<boolean> = new BehaviorSubject(false)
  constructor(private readonly storage: Storage) {}

  async init() {
    this.showDevTools$.next(!!(await this.storage.get(SHOW_DEV_TOOLS)))
  }

  async toggleShowDevTools(): Promise<boolean> {
    const val = !(await this.storage.get(SHOW_DEV_TOOLS))
    this.showDevTools$.next(val)
    await this.storage.set(SHOW_DEV_TOOLS, val)
    return val
  }
}
