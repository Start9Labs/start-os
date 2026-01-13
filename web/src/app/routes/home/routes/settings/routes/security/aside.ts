import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/kit'

@Component({
  selector: 'security-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Change Password</button>
      <tui-expand>
        Changes the administrator password for the router. Enhances security by
        protecting router settings, preventing unauthorized access. Set a
        strong, unique password.
      </tui-expand>
      <button tuiAccordion appearance="">SSH</button>
      <tui-expand>
        Secure Shell (SSH) access for remote management. Allows secure remote
        command-line access to the router to enable advanced management and
        troubleshooting. Configure access settings by adding a key.
      </tui-expand>
      <button tuiAccordion appearance="">Remote Access</button>
      <tui-expand>
        Enables remote access to manage the router’s settings and configurations
        from outside the local network.
        <ul>
          <li>
            <b>When behind NAT</b>
            (Network Address Translation): Allows remote access only when the
            router is part of a private network using NAT. NAT is commonly used
            in home and small business networks where multiple devices share a
            single public IP address.
          </li>
          <li>
            <b>Never:</b>
            Completely disables remote access. Suitable for environments where
            security is paramount, such as in highly sensitive or isolated
            networks.
          </li>
          <li>
            <b>Always:</b>
            Enables remote access at all times. Ideal for environments where
            continuous remote management is necessary, such as in a business
            network where IT administrators need to maintain constant access to
            the router for monitoring and updates.
          </li>
        </ul>
      </tui-expand>
      <button tuiAccordion appearance="">Activity</button>
      <tui-expand>
        Record events and actions that occur on the router, providing a detailed
        history of network activity, configuration changes, security events, and
        system operations. These logs help users and administrators monitor the
        performance and security of the network.
      </tui-expand>
    </tui-accordion>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAccordion],
})
export class SecurityAside {}
