import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'
import { DocsLinkDirective } from 'projects/shared/src/public-api'

@Component({
  selector: 'proxies-info',
  template: `
    <tui-notification>
      Currently, StartOS only supports Wireguard proxies, which can be used for:
      <ol>
        <li>
          Proxying
          <i>outbound</i>
          traffic to mask your home/business IP from other servers accessed by
          your server/services
        </li>
        <li>
          Proxying
          <i>inbound</i>
          traffic to mask your home/business IP from anyone accessing your
          server/services over clearnet
        </li>
        <li>
          Creating a Virtual Local Area Network (VLAN) to enable private, remote
          VPN access to your server/services
        </li>
      </ol>
      <a tuiLink docsLink href="/@TODO">View instructions</a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiNotification, TuiLink, DocsLinkDirective],
})
export class ProxiesInfoComponent {}
