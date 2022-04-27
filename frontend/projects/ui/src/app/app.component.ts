import { Component } from '@angular/core'
import { AuthService } from './services/auth.service'
import { SplitPaneTracker } from './services/split-pane.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  constructor(
    readonly authService: AuthService,
    private readonly splitPane: SplitPaneTracker,
  ) {}

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }
}
