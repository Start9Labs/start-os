import { Component, inject, OnDestroy } from '@angular/core'
import { Router } from '@angular/router'
import { combineLatest, map, merge } from 'rxjs'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { PatchDataService } from './services/patch-data.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { ConnectionService } from './services/connection.service'
import { Title } from '@angular/platform-browser'
import {
  ClientStorageService,
  WidgetDrawer,
} from './services/client-storage.service'
import { ThemeSwitcherService } from './services/theme-switcher.service'
import { THEME } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { DataModel } from './services/patch-db/data-model'
import { slideInAnimation } from './route-animation'

function hasNavigation(url: string): boolean {
  return (
    !url.startsWith('/loading') &&
    !url.startsWith('/diagnostic') &&
    !url.startsWith('/portal')
  )
}

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
  animations: [slideInAnimation],
})
export class AppComponent implements OnDestroy {
  readonly subscription = merge(this.patchData, this.patchMonitor).subscribe()
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$
  readonly widgetDrawer$ = this.clientStorageService.widgetDrawer$
  readonly theme$ = inject(THEME)
  readonly navigation$ = combineLatest([
    this.authService.isVerified$,
    this.router.events.pipe(map(() => hasNavigation(this.router.url))),
  ]).pipe(map(([isVerified, hasNavigation]) => isVerified && hasNavigation))

  constructor(
    private readonly router: Router,
    private readonly titleService: Title,
    private readonly patchData: PatchDataService,
    private readonly patchMonitor: PatchMonitorService,
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDB<DataModel>,
    readonly authService: AuthService,
    readonly connection: ConnectionService,
    readonly clientStorageService: ClientStorageService,
    readonly themeSwitcher: ThemeSwitcherService,
  ) {}

  async ngOnInit() {
    this.patch
      .watch$('ui', 'name')
      .subscribe(name => this.titleService.setTitle(name || 'StartOS'))
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
