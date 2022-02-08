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
    const val = await this.storage.get(SHOW_DEV_TOOLS)
    this.showDevTools$.next(!!val)
  }

  async toggleShowDevTools(): Promise<boolean> {
    const newVal = !(await this.storage.get(SHOW_DEV_TOOLS))
    await this.storage.set(SHOW_DEV_TOOLS, newVal)
    this.showDevTools$.next(newVal)
    return newVal
  }
}
