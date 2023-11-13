import { ChangeDetectionStrategy, Component } from '@angular/core'
import { SplitPaneTracker } from 'src/app/services/split-pane.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TuiDialogService } from '@taiga-ui/core'
import { WIDGETS_COMPONENT } from '../../pages/widgets/widgets.page'
import { WorkspaceConfig } from '@start9labs/shared'
import {
  ClientStorageService,
  WidgetDrawer,
} from 'src/app/services/client-storage.service'

const { enableWidgets } =
  require('../../../../../../config.json') as WorkspaceConfig

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
  readonly widgetDrawer$ = this.clientStorageService.widgetDrawer$

  readonly enableWidgets = enableWidgets

  constructor(
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDB<DataModel>,
    private readonly dialog: TuiDialogService,
    private readonly clientStorageService: ClientStorageService,
  ) {}

  onSidebar(drawer: WidgetDrawer) {
    this.clientStorageService.updateWidgetDrawer({
      ...drawer,
      open: !drawer.open,
    })
  }

  onWidgets() {
    this.dialog.open(WIDGETS_COMPONENT, { label: 'Widgets' }).subscribe()
  }
}
