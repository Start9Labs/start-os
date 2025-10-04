import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'vpn-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Label</button>
      <tui-expand>
        A descriptive label that uniquely identifies the Outbound VPN client
      </tui-expand>
      <button tuiAccordion appearance="">VPN Chaining</button>
      <tui-expand>
        Indicates the path of connections to the Internet configured through VPN
        chaining
      </tui-expand>
    </tui-accordion>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAccordion],
})
export class VPNAside {}
