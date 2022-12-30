import { Component, OnDestroy } from '@angular/core'
import { merge } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { PatchDataService } from './services/patch-data.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { ConnectionService } from './services/connection.service'
import { Title } from '@angular/platform-browser'
import { ServerNameService } from './services/server-name.service'
import { DataModel } from './services/patch-db/data-model'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnDestroy {
  readonly subscription = merge(this.patchData, this.patchMonitor).subscribe()
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$
  readonly open$ = this.patchDb.watch$('ui', 'widgets', 'open')
  readonly width$ = this.patchDb.watch$('ui', 'widgets', 'width')

  constructor(
    private readonly titleService: Title,
    private readonly patchData: PatchDataService,
    private readonly patchMonitor: PatchMonitorService,
    private readonly splitPane: SplitPaneTracker,
    private readonly patchDb: PatchDB<DataModel>,
    private readonly serverNameService: ServerNameService,
    readonly authService: AuthService,
    readonly connection: ConnectionService,
  ) {}

  ngOnInit() {
    this.serverNameService.name$.subscribe(({ current }) =>
      this.titleService.setTitle(current),
    )
  }

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }

  onResize([x]: readonly [number, number]) {
    // TODO: save size to patch-db with debounce
  }

  ngOnDestroy() {
    this.subscription.unsubscribe()
  }
}
