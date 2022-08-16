import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { ToastModule } from '@start9labs/shared'

import { ToastContainerComponent } from './toast-container.component'
import { OfflineToastComponent } from './offline-toast/offline-toast.component'

@NgModule({
  imports: [CommonModule, ToastModule],
  declarations: [ToastContainerComponent, OfflineToastComponent],
  exports: [ToastContainerComponent],
})
export class ToastContainerModule {}
