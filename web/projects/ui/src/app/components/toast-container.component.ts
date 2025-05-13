import { ChangeDetectionStrategy, Component } from '@angular/core'
import { NotificationsToastComponent } from './notifications-toast.component'
import { RefreshAlertComponent } from './refresh-alert.component'

@Component({
  selector: 'toast-container',
  template: `
    <notifications-toast />
    <refresh-alert />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [NotificationsToastComponent, RefreshAlertComponent],
})
export class ToastContainerComponent {}
