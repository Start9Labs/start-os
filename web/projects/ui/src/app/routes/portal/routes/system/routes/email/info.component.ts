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
        iconEnd="@tui.external-link"
        [textContent]="'View instructions'"
      ></a>
    </tui-notification>
  `,
  styles: `
    :host {
      grid-column: span 2;
    }

    :host-context(tui-root._mobile) {
      grid-column: 1;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiLink],
})
export class EmailInfoComponent {}
