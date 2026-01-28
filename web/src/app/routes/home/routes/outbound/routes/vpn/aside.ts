import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/kit'

@Component({
  selector: 'vpn-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Label</button>
      <tui-expand>
        A friendly name to identify this VPN connection. Use something
        descriptive like "Mullvad Sweden" or "Work VPN".
      </tui-expand>
      <button tuiAccordion appearance="">Connects to</button>
      <tui-expand>
        Where traffic from this VPN should be routed:
        <ul>
          <li>
            <b>Internet:</b>
            Traffic exits directly to the internet through this VPN.
          </li>
          <li>
            <b>Another VPN:</b>
            Chain through another VPN first for extra privacy. Your traffic will
            be encrypted multiple times and exit through multiple servers.
          </li>
        </ul>
        <b>Note:</b>
        Chaining VPNs increases latency and may reduce speeds.
      </tui-expand>
      <button tuiAccordion appearance="">Connection Path</button>
      <tui-expand>
        Shows the full route your traffic takes from this VPN to the internet.
        For example: Mullvad → Proton → Internet means traffic is encrypted by
        Mullvad, sent through Proton, then exits to the internet.
      </tui-expand>
    </tui-accordion>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAccordion],
})
export class VPNAside {}
