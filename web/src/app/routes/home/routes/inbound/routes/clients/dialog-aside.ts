import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'inbound-client-dialog-aside',
  template: `
    <h3>Label</h3>
    A clear and descriptive name for the client device, such as "John's Laptop"
    or "Office Phone," to differentiate it from other devices.
    <h3>LAN IP Address</h3>
    The desired LAN IP address for the client device, ensuring it is within the
    IP range allocated for the VPN. Ensures the client device has a unique IP
    address within the VPN network, allowing it to communicate with other
    devices.
    <h3>Public Key</h3>
    The WireGuard public key for the client device. If left empty, a key pair
    will be generated automatically. Enter an existing public key if the client
    device already has one configured.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InboundClientsDialogAside {}
