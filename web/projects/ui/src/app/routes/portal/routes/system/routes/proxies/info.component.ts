import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'

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
      <a
        tuiLink
        href="https://docs.start9.com/latest/user-manual/vpns/"
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
export class ProxiesInfoComponent {}
