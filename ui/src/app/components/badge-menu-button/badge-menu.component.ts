import { Component } from '@angular/core'
import { Observable } from 'rxjs'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'badge-menu-button',
  templateUrl: './badge-menu.component.html',
  styleUrls: ['./badge-menu.component.scss'],
})

export class BadgeMenuComponent {
  badge$: Observable<number>
  menuFixedOpen$: Observable<boolean>

  constructor (
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDbModel,
  ) {
    this.menuFixedOpen$ = this.splitPane.menuFixedOpenOnLeft$.asObservable()
    this.badge$ = this.patch.watch$('server-info', 'unread-notification-count')
  }
}
