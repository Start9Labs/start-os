import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'blackout-dialog-aside',
  template: `
    <p>
      A blackout window disables WiFi during the specified time period,
      preventing any devices from connecting. Useful for reducing distractions,
      enforcing bedtime routines, or enhancing network security during
      off-hours.
    </p>
    <h3>Time Window</h3>
    <p>
      The start and end times for the blackout period. WiFi will be disabled
      between these times on the selected days. End time must be later than
      start time.
    </p>
    <h3>Days</h3>
    <p>Select which days of the week the blackout window should be active.</p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BlackoutDialogAside {}
