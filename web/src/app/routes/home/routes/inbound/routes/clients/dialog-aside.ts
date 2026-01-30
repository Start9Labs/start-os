import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'inbound-client-dialog-aside',
  template: `
    <h3>Name</h3>
    A clear and descriptive name for the client device, such as "John's Laptop"
    or "Office Phone," to differentiate it from other devices.
    <h3>LAN IP Address</h3>
    The desired LAN IP address for the client device, ensuring it is within the
    IP range allocated for the VPN. Ensures the client device has a unique IP
    address within the VPN network, allowing it to communicate with other
    devices.
    <h3>Client Keys</h3>
    The cryptographic keys used to secure the connection between the client
    device and the WireGuard VPN server. Generating here simplifies setup by
    scanning the QR code with the client device to import the keys
    automatically. Alternatively, manually enter an existing client public key.
    This option allows pre-generated or externally managed keys to be used if
    preferred.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InboundClientsDialogAside {}
