import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiNotificationModule } from '@taiga-ui/core'

@Component({
  selector: 'ssh-info',
  template: `
    <tui-notification>
      Adding domains to StartOS enables you to access your server and service
      interfaces over clearnet.
      <a
        href="https://docs.start9.com/0.3.5.x/user-manual/ssh"
        target="_blank"
        rel="noreferrer"
      >
        View instructions
      </a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotificationModule],
})
export class SSHInfoComponent {}
