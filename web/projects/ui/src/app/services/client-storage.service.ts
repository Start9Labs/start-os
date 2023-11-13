import { Injectable } from '@angular/core'
import { ReplaySubject, Subject } from 'rxjs'
import { WorkspaceConfig } from '../../../../shared/src/types/workspace-config'
import { StorageService } from './storage.service'
const SHOW_DEV_TOOLS = 'SHOW_DEV_TOOLS'
const SHOW_DISK_REPAIR = 'SHOW_DISK_REPAIR'
const WIDGET_DRAWER = 'WIDGET_DRAWER'

const { enableWidgets } =
  require('../../../../../config.json') as WorkspaceConfig

export type WidgetDrawer = {
  open: boolean
  width: 400 | 600
}

@Injectable({
  providedIn: 'root',
})
export class ClientStorageService {
  readonly showDevTools$ = new ReplaySubject<boolean>(1)
  readonly showDiskRepair$ = new ReplaySubject<boolean>(1)
  readonly widgetDrawer$ = new ReplaySubject<WidgetDrawer>(1)

  constructor(private readonly storage: StorageService) {}

  init() {
    this.showDevTools$.next(!!this.storage.get(SHOW_DEV_TOOLS))
    this.showDiskRepair$.next(!!this.storage.get(SHOW_DISK_REPAIR))
    this.widgetDrawer$.next(
      enableWidgets
        ? this.storage.get(WIDGET_DRAWER) || {
            open: true,
            width: 600,
          }
        : {
            open: false,
            width: 600,
          },
    )
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

  updateWidgetDrawer(drawer: WidgetDrawer) {
    this.widgetDrawer$.next(drawer)
    this.storage.set(WIDGET_DRAWER, drawer)
  }
}
