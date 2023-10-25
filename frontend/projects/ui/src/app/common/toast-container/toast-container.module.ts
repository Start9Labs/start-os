import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { TuiAutoFocusModule } from '@taiga-ui/cdk'
import { TuiAlertModule, TuiDialogModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'

import { ToastContainerComponent } from './toast-container.component'
import { NotificationsToastComponent } from './notifications-toast/notifications-toast.component'
import { RefreshAlertComponent } from './refresh-alert/refresh-alert.component'
import { UpdateToastComponent } from './update-toast/update-toast.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    TuiDialogModule,
    TuiButtonModule,
    TuiAutoFocusModule,
    TuiAlertModule,
  ],
  declarations: [
    ToastContainerComponent,
    NotificationsToastComponent,
    RefreshAlertComponent,
    UpdateToastComponent,
  ],
  exports: [ToastContainerComponent],
})
export class ToastContainerModule {}
