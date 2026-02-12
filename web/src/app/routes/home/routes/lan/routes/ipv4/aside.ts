import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ipv4-aside',
  template: `
    <h3>Network Block</h3>
    <p>
      The /16 private IP block for your network. Each security profile will
      receive its own /24 subnet within this block, allowing up to 256 separate
      subnets with 254 devices each.
    </p>
    <h3>Router IP</h3>
    <p>
      The address assigned to your router within the default subnet (.0.x). This
      is the address devices use to reach the router and access the internet.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv4Aside {}
