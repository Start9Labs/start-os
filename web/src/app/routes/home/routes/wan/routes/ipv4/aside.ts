import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'ipv4-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">IP Address</button>
      <tui-expand>
        <h3>Mode</h3>
        <ul>
          <li>
            <b>DHCP:</b>
            Your ISP automatically assigns an IP address to your router. This is
            the most common configuration.
          </li>
          <li>
            <b>PPPoE:</b>
            Used by some DSL providers. Requires a username and password from
            your ISP.
          </li>
          <li>
            <b>Static:</b>
            Manually configure a fixed IP address. Only use if your ISP has
            assigned you a static IP.
          </li>
        </ul>
      </tui-expand>

      <button tuiAccordion appearance="">DNS</button>
      <tui-expand>
        <p>DNS (Domain Name System) translates domain names to IP addresses.</p>
        <ul>
          <li>
            <b>Get from ISP:</b>
            Use DNS servers provided automatically by your ISP.
          </li>
          <li>
            <b>Custom:</b>
            Specify your own DNS servers (e.g., 1.1.1.1, 8.8.8.8).
          </li>
        </ul>
      </tui-expand>
    </tui-accordion>
  `,
  imports: [TuiAccordion],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv4Aside {}
