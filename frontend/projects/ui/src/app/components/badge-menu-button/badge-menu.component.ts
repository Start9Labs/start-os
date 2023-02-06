import { ChangeDetectionStrategy, Component } from '@angular/core'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TuiDialogService } from '@taiga-ui/core'
import { WIDGETS_COMPONENT } from '../../pages/widgets/widgets.page'
import { WidgetsService } from '../../pages/widgets/built-in/widgets.service'

@Component({
  selector: 'badge-menu-button',
  templateUrl: './badge-menu.component.html',
  styleUrls: ['./badge-menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BadgeMenuComponent {
  readonly unreadCount$ = this.patch.watch$(
    'server-info',
    'unread-notification-count',
  )
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$

  constructor(
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDB<DataModel>,
    private readonly dialog: TuiDialogService,
    readonly widgets$: WidgetsService,
  ) {}

  onSidebar() {
    this.widgets$.toggle(true)
  }

  onWidgets() {
    this.dialog.open(WIDGETS_COMPONENT, { closeable: false }).subscribe()
  }
}
