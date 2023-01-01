import { ChangeDetectionStrategy, Component } from '@angular/core'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TuiDialogService } from '@taiga-ui/core'
import { WIDGETS_COMPONENT } from '../../pages/widgets/widgets.page'
import { ApiService } from '../../services/api/embassy-api.service'

@Component({
  selector: 'badge-menu-button',
  templateUrl: './badge-menu.component.html',
  styleUrls: ['./badge-menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BadgeMenuComponent {
  readonly open$ = this.patch.watch$('ui', 'widgets', 'open')
  readonly unreadCount$ = this.patch.watch$(
    'server-info',
    'unread-notification-count',
  )
  readonly sidebarOpen$ = this.splitPane.sidebarOpen$

  constructor(
    private readonly api: ApiService,
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDB<DataModel>,
    private readonly dialog: TuiDialogService,
  ) {}

  onSidebar() {
    this.api.setDbValue(['widgets', 'open'], true)
  }

  onWidgets() {
    this.dialog.open(WIDGETS_COMPONENT, { closeable: false }).subscribe()
  }
}
