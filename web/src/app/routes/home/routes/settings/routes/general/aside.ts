import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/kit'

@Component({
  selector: 'general-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Preferences</button>
      <tui-expand>
        <h3 [style.margin-block-start]="0">Theme</h3>
        Choose from dark or light theme, or keep your system settings applied to
        the UI.
        <h3>Language</h3>
        Configure the preferred language for the user interface of the router.
        This setting affects the language displayed for all menus, options, and
        other text elements within the router's web interface.
      </tui-expand>
      <button tuiAccordion appearance="">Remote Access</button>
      <tui-expand>
        Enables remote access to manage the router's settings and configurations
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
      <button tuiAccordion appearance="">Updates</button>
      <tui-expand>
        When an update is available, a banner will appear at the top of this
        page. You can view release notes before updating. The router will
        experience brief downtime during the update process.
      </tui-expand>
    </tui-accordion>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAccordion],
})
export class GeneralAside {}
