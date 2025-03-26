import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'ssh-info',
  template: `
    <tui-notification>
      Adding SSH keys to StartOS is useful for command line access, as well as
      for debugging purposes.
      <a
        tuiLink
        href="https://docs.start9.com/latest/user-manual/ssh"
        target="_blank"
        rel="noreferrer"
      >
        View instructions
      </a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiLink],
})
export class SSHInfoComponent {}
