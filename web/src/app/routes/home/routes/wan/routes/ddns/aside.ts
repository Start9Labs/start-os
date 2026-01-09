import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ddns-aside',
  template: `
    <h3>Dynamic DNS</h3>
    <p>
      DDNS maps your changing IP address to a fixed domain name, allowing you to
      access your network remotely without knowing your current IP.
    </p>
    <ul>
      <li>
        <b>Start9:</b>
        Free DDNS service provided by Start9. No additional setup required.
      </li>
      <li>
        <b>Other providers:</b>
        Requires an account with the provider. Enter the credentials and
        hostname from your DDNS provider.
      </li>
    </ul>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DdnsAside {}
