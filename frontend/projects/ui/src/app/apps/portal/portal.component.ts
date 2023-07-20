import { ChangeDetectionStrategy, Component } from '@angular/core'
import { tuiDropdownOptionsProvider } from '@taiga-ui/core'

@Component({
  templateUrl: 'portal.component.html',
  styleUrls: ['portal.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    // TODO: Move to global
    tuiDropdownOptionsProvider({
      appearance: 'start-os',
    }),
  ],
})
export class PortalComponent {}
