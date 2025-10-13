import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'logs-aside',
  template: `
    Logs record system events and help diagnose issues. Useful for
    troubleshooting and support inquiries.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LogsAside {}
