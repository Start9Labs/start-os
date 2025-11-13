import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'mac-aside',
  template: `
    <h3>Strategy</h3>
    <ul>
      <li>
        <b>Router:</b>
        Use the MAC address of the router as the default.
      </li>
      <li>
        <b>Custom:</b>
        In some cases, users may need to enter the MAC address of their previous
        device (e.g., an old router or a computer) to maintain the same IP
        address or meet ISP requirements.
      </li>
    </ul>
    <h3>MAC Address</h3>
    The MAC (Media Access Control) address of the router, a unique identifier
    assigned to the network interface of the router.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MacAside {}
