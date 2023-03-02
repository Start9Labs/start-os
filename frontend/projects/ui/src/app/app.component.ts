import { Component, OnDestroy } from '@angular/core'
import { merge } from 'rxjs'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { PatchDataService } from './services/patch-data.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { ConnectionService } from './services/connection.service'
import { Title } from '@angular/platform-browser'
import { ServerNameService } from './services/server-name.service'
import {
  ClientStorageService,
  WidgetDrawer,
} from './services/client-storage.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnDestroy {
  readonly subscription = merge(this.patchData, this.patchMonitor).subscribe()
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$
  readonly widgetDrawer$ = this.clientStorageService.widgetDrawer$

  constructor(
    private readonly titleService: Title,
    private readonly patchData: PatchDataService,
    private readonly patchMonitor: PatchMonitorService,
    private readonly splitPane: SplitPaneTracker,
    private readonly serverNameService: ServerNameService,
    readonly authService: AuthService,
    readonly connection: ConnectionService,
    readonly clientStorageService: ClientStorageService,
  ) {}

  ngOnInit() {
    this.serverNameService.name$.subscribe(({ current }) =>
      this.titleService.setTitle(current),
    )
  }

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }

  onResize(drawer: WidgetDrawer) {
    this.clientStorageService.updateWidgetDrawer({
      ...drawer,
      width: drawer.width === 400 ? 600 : 400,
    })
  }

  ngOnDestroy() {
    this.subscription.unsubscribe()
  }
}
