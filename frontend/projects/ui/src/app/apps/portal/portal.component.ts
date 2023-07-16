import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  templateUrl: 'portal.component.html',
  styleUrls: ['portal.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PortalComponent {}
