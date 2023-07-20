import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ServicesService } from '../../services/services.service'

@Component({
  templateUrl: 'desktop.component.html',
  styleUrls: ['desktop.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DesktopComponent {
  // TODO: Only show services added to desktop
  readonly services$ = inject(ServicesService)
}
