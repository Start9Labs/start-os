import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'toast-container',
  templateUrl: './toast-container.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ToastContainerComponent {}
