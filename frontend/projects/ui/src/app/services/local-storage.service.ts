import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
const SHOW_DEV_TOOLS = 'SHOW_DEV_TOOLS'

@Injectable({
  providedIn: 'root',
})
export class LocalStorageService {
  showDevTools: boolean
  constructor(private readonly storage: Storage) {}

  async init() {
    this.showDevTools = !!(await this.storage.get(SHOW_DEV_TOOLS))
  }

  async toggleShowDevTools() {
    this.showDevTools = !(await this.storage.get(SHOW_DEV_TOOLS))
    await this.storage.set(SHOW_DEV_TOOLS, this.showDevTools)
  }
}
