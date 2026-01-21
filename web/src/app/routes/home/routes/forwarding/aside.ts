import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'forwarding-aside',
  template: `
    <h3>Label</h3>
    Define a label for the port forward rule, such as "Web Server Access,"
    "Online Gaming," or "Remote Desktop", to make the rule easily identifiable.
    <h3>Protocol</h3>
    Specifies the protocol (TCP/UDP) for port forwarding. Determines the type of
    traffic allowed through the forwarded port for correct traffic routing.
    <h3>External Ports</h3>
    The port number on the external network to be forwarded. Defines the entry
    point for incoming traffic. Allows external devices to access services on
    your network.
    <h3>Internal Ports</h3>
    The port number on the internal network. Defines the destination port for
    incoming traffic. Routes traffic to the correct internal device or service.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ForwardingAside {}
