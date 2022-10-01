import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { StorageService } from './storage.service'
const SHOW_DEV_TOOLS = 'SHOW_DEV_TOOLS'
const SHOW_DISK_REPAIR = 'SHOW_DISK_REPAIR'

@Injectable({
  providedIn: 'root',
})
export class ClientStorageService {
  readonly showDevTools$ = new BehaviorSubject<boolean>(false)
  readonly showDiskRepair$ = new BehaviorSubject<boolean>(false)

  constructor(private readonly storage: StorageService) {}

  init() {
    this.showDevTools$.next(!!this.storage.get(SHOW_DEV_TOOLS))
    this.showDiskRepair$.next(!!this.storage.get(SHOW_DISK_REPAIR))
  }

  toggleShowDevTools(): boolean {
    const newVal = !this.storage.get(SHOW_DEV_TOOLS)
    this.storage.set(SHOW_DEV_TOOLS, newVal)
    this.showDevTools$.next(newVal)
    return newVal
  }

  toggleShowDiskRepair(): boolean {
    const newVal = !this.storage.get(SHOW_DISK_REPAIR)
    this.storage.set(SHOW_DISK_REPAIR, newVal)
    this.showDiskRepair$.next(newVal)
    return newVal
  }
}
