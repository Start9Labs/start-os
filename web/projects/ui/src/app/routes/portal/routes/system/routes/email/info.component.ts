import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'email-info',
  template: `
    <tui-notification>
      Adding SMTP credentials to StartOS enables StartOS and some services to
      send you emails.
      <a
        tuiLink
        href="https://docs.start9.com/latest/user-manual/smtp"
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
export class EmailInfoComponent {}
