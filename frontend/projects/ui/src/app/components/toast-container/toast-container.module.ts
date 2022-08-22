import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { AlertModule, ToastModule } from '@start9labs/shared'

import { ToastContainerComponent } from './toast-container.component'
import { NotificationsToastComponent } from './notifications-toast/notifications-toast.component'
import { RefreshAlertComponent } from './refresh-alert/refresh-alert.component'
import { UpdateToastComponent } from './update-toast/update-toast.component'

@NgModule({
  imports: [CommonModule, ToastModule, AlertModule, RouterModule],
  declarations: [
    ToastContainerComponent,
    NotificationsToastComponent,
    RefreshAlertComponent,
    UpdateToastComponent,
  ],
  exports: [ToastContainerComponent],
})
export class ToastContainerModule {}
