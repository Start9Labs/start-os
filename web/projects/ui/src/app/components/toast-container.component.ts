import { ChangeDetectionStrategy, Component } from '@angular/core'
import { NotificationsToastComponent } from './notifications-toast.component'
import { RefreshAlertComponent } from './refresh-alert.component'
import { UpdateToastComponent } from './update-toast.component'

@Component({
  standalone: true,
  selector: 'toast-container',
  template: `
    <notifications-toast />
    <refresh-alert />
    <update-toast />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    NotificationsToastComponent,
    UpdateToastComponent,
    RefreshAlertComponent,
  ],
})
export class ToastContainerComponent {}
