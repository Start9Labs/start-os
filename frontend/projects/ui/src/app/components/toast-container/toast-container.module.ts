import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { AlertModule, ToastModule } from '@start9labs/shared'

import { ToastContainerComponent } from './toast-container.component'
import { OfflineToastComponent } from './offline-toast/offline-toast.component'
import { RefreshAlertComponent } from './refresh-alert/refresh-alert.component'

@NgModule({
  imports: [CommonModule, ToastModule, AlertModule],
  declarations: [
    ToastContainerComponent,
    OfflineToastComponent,
    RefreshAlertComponent,
  ],
  exports: [ToastContainerComponent],
})
export class ToastContainerModule {}
