import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'advanced-aside',
  template: `
    Advanced options for power users. LuCI provides direct access to OpenWRT
    configuration. Support diagnostics help troubleshoot issues. Factory reset
    restores default settings.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdvancedAside {}
