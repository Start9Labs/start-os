import { Component, OnDestroy } from '@angular/core'
import { merge, take } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { PatchDataService } from './services/patch-data.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { ConnectionService } from './services/connection.service'
import { Title } from '@angular/platform-browser'
import { ServerNameService } from './services/server-name.service'
import { DataModel } from './services/patch-db/data-model'
import { ApiService } from './services/api/embassy-api.service'
import { WidgetsService } from './pages/widgets/built-in/widgets.service'
import { WorkspaceConfig } from '@start9labs/shared'

const { enableWidgets } = require('../../../../config.json') as WorkspaceConfig

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnDestroy {
  readonly subscription = merge(this.patchData, this.patchMonitor).subscribe()
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$
  readonly enableWidgets = enableWidgets

  width = 400

  constructor(
    private readonly titleService: Title,
    private readonly patch: PatchDB<DataModel>,
    private readonly patchData: PatchDataService,
    private readonly patchMonitor: PatchMonitorService,
    private readonly splitPane: SplitPaneTracker,
    private readonly api: ApiService,
    private readonly serverNameService: ServerNameService,
    readonly authService: AuthService,
    readonly connection: ConnectionService,
    readonly widgets$: WidgetsService,
  ) {
    this.patch
      .watch$('ui', 'widgets', 'width')
      .pipe(take(1))
      .subscribe(width => {
        this.width = width
      })
  }

  ngOnInit() {
    this.serverNameService.name$.subscribe(({ current }) =>
      this.titleService.setTitle(current),
    )
  }

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }

  onResize() {
    this.width = this.width === 400 ? 600 : 400
    this.api.setDbValue(['widgets', 'width'], this.width)
  }

  ngOnDestroy() {
    this.subscription.unsubscribe()
  }
}
