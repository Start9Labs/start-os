import { Component, OnDestroy } from '@angular/core'
import { merge } from 'rxjs'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { PatchDataService } from './services/patch-data.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { ConnectionService } from './services/connection.service'
import { Title } from '@angular/platform-browser'
import { ServerNameService } from './services/server-name.service'
import { createGesture } from '@ionic/angular'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnDestroy {
  readonly subscription = merge(this.patchData, this.patchMonitor).subscribe()
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$

  constructor(
    private readonly titleService: Title,
    private readonly patchData: PatchDataService,
    private readonly patchMonitor: PatchMonitorService,
    private readonly splitPane: SplitPaneTracker,
    private readonly serverNameService: ServerNameService,
    readonly authService: AuthService,
    readonly connection: ConnectionService,
  ) {}

  ngOnInit() {
    this.serverNameService.name$.subscribe(({ current }) =>
      this.titleService.setTitle(current),
    )
  }

  ngAfterViewInit() {
    // You could also read the value of `--side-width`
    // using `getComputedStyle` in an `onStart` callback
    // let workingWidth = 400
    // let startingWidth = 400
    //
    // const onMove = (ev: any) => {
    //   requestAnimationFrame(() => {
    //     workingWidth = startingWidth - ev.deltaX
    //     rightMenu.style.setProperty('--side-width', `${workingWidth}px`)
    //   })
    // }
    //
    // const onEnd = () => {
    //   startingWidth = workingWidth
    // }
    //
    // const rightMenu = document.querySelector('.right')! as any
    // const divider = document.querySelector('.divider')!
    // const gesture = createGesture({
    //   gestureName: 'resize-menu',
    //   el: divider,
    //   onEnd,
    //   onMove,
    // })
    //
    // gesture.enable(true)
  }

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }

  ngOnDestroy() {
    this.subscription.unsubscribe()
  }
}
