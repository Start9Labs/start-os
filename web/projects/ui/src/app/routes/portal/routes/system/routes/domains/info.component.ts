import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'
import { DocsLinkDirective } from 'projects/shared/src/public-api'

@Component({
  selector: 'domains-info',
  template: `
    <tui-notification>
      Adding domains permits accessing your server and services over clearnet.
      <a tuiLink docsLink href="/@TODO">View instructions</a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiLink, DocsLinkDirective],
})
export class DomainsInfoComponent {}
