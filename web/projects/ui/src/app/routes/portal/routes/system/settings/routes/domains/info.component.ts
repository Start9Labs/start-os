import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'domains-info',
  template: `
    <tui-notification>
      Adding domains permits accessing your server and services over clearnet.
      <a
        href="https://docs.start9.com/latest/user-manual/domains"
        target="_blank"
        rel="noreferrer"
      >
        View instructions
      </a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification],
})
export class DomainsInfoComponent {}
