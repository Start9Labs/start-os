import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { BehaviorSubject } from 'rxjs'
const SHOW_DEV_TOOLS = 'SHOW_DEV_TOOLS'
const SHOW_DISK_REPAIR = 'SHOW_DISK_REPAIR'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageService {
  showDevTools$: BehaviorSubject<boolean> = new BehaviorSubject(false)
  showDiskRepair$: BehaviorSubject<boolean> = new BehaviorSubject(false)
  constructor(private readonly storage: Storage) {}

  async init() {
    const devTools = await this.storage.get(SHOW_DEV_TOOLS)
    this.showDevTools$.next(!!devTools)
    const diskRepair = await this.storage.get(SHOW_DISK_REPAIR)
    this.showDiskRepair$.next(!!diskRepair)
  }

  async toggleShowDevTools(): Promise<boolean> {
    const newVal = !(await this.storage.get(SHOW_DEV_TOOLS))
    await this.storage.set(SHOW_DEV_TOOLS, newVal)
    this.showDevTools$.next(newVal)
    return newVal
  }

  async toggleShowDiskRepair(): Promise<boolean> {
    const newVal = !(await this.storage.get(SHOW_DISK_REPAIR))
    await this.storage.set(SHOW_DISK_REPAIR, newVal)
    this.showDiskRepair$.next(newVal)
    return newVal
  }
}
