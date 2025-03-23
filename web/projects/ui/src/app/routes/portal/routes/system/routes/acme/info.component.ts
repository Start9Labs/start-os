import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'acme-info',
  template: `
    <tui-notification>
      Register with one or more ACME providers such as Let's Encrypt in order to
      generate SSL (https) certificates on-demand for clearnet hosting.
      <a
        tuiLink
        href="https://docs.start9.com/latest/user-manual/acme"
        target="_blank"
        rel="noreferrer"
        iconEnd="@tui.external-link"
        [textContent]="'View instructions'"
      ></a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiLink],
})
export class AcmeInfoComponent {}
