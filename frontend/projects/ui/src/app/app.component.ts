import { Component, Inject, OnDestroy } from '@angular/core'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { merge, Observable } from 'rxjs'
import { GLOBAL_SERVICE } from './app/global/global.module'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent implements OnDestroy {
  readonly subscription = merge(...this.services).subscribe()

  constructor(
    @Inject(GLOBAL_SERVICE)
    private readonly services: readonly Observable<unknown>[],
    readonly authService: AuthService,
    private readonly splitPane: SplitPaneTracker,
  ) {}

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }

  ngOnDestroy() {
    this.subscription.unsubscribe()
  }
}
