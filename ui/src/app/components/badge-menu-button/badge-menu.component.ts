import { Component } from '@angular/core'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { combineLatest, Subscription } from 'rxjs'

@Component({
  selector: 'badge-menu-button',
  templateUrl: './badge-menu.component.html',
  styleUrls: ['./badge-menu.component.scss'],
})

export class BadgeMenuComponent {
  unreadCount: number
  sidebarOpen: boolean

  subs: Subscription[] = []

  constructor (
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.subs = [
      combineLatest([
        this.patch.watch$('server-info', 'unread-notification-count'),
        this.splitPane.sidebarOpen$,
      ])
      .subscribe(([unread, menu]) => {
        this.unreadCount = unread
        this.sidebarOpen = menu
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }
}
