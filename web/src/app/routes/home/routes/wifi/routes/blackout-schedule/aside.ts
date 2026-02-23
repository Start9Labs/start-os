import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'wifi-blackout-aside',
  template: `
    <p>
      Specifies designated times when WiFi will be disabled, preventing any
      devices from connecting to it. This feature is often used to restrict
      internet access during specific periods for various purposes, such as
      reducing distractions, enforcing bedtime routines, or enhancing network
      security during off-hours.
    </p>
    <p>
      <strong>Double click existing intervals to edit or delete them</strong>
    </p>
    <p>Drag existing intervals to edit them in place.</p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WifiBlackoutAside {}
