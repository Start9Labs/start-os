import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'activity-aside',
  template: `
    View recent login attempts and security events. Monitor for suspicious
    activity and remove old entries as needed.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ActivityAside {}
