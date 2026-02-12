import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'wan-dns-aside',
  template: `
    <h3>DNS</h3>
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
    <h3>TLS</h3>
    <p>
      Enable TLS (DNS over TLS) for encrypted DNS queries. This improves privacy
      by preventing eavesdropping on your DNS traffic.
    </p>
    <p>
      <b>Note:</b>
      Not all DNS servers support TLS. Common servers that do include Cloudflare
      (1.1.1.1), Google (8.8.8.8), and Quad9 (9.9.9.9).
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DnsAside {}
