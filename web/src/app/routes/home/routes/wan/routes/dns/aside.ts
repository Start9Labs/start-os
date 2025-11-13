import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'dns-aside',
  template: `
    <h3>Strategy</h3>
    Dynamic DNS (DDNS) allows you to map a dynamic IP address to a domain name.
    It provides a consistent domain name for devices with changing IP addresses
    and is useful for accessing your network remotely. Enable it after setting
    up a DDNS service with one of the listed providers, providing the hostname
    and credentials if required.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DnsAside {}
