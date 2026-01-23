import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ethernet-aside',
  template: `
    <h3>Name</h3>
    A preset identifier for the associated Ethernet port.
    <h3>Permissions</h3>
    The default security profile or schedule assigned to devices connected via
    the specified Ethernet port.
    <h3>WAN</h3>
    The Wide Area Network (WAN) port that manages the connection to your ISP,
    ensuring proper internet connectivity. Only one Ethernet port can have this
    designation.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EthernetAside {}
