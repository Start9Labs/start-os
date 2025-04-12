import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { TuiIcon, TuiLink } from '@taiga-ui/core'
import { TuiTooltip } from '@taiga-ui/kit'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { InterfaceActionsComponent } from './actions.component'
import { AddressDetails } from './interface.utils'
import { MaskPipe } from './mask.pipe'

@Component({
  standalone: true,
  selector: 'section[local]',
  template: `
    <header>
      Local
      <tui-icon [tuiTooltip]="tooltip" />
      <ng-template #tooltip>
        Local addresses can only be accessed by devices connected to the same
        LAN as your server, either directly or using a VPN.
        <a
          tuiLink
          href="https://docs.start9.com/latest/user-manual/interface-addresses#local"
          target="_blank"
          rel="noreferrer"
        >
          Learn More
        </a>
      </ng-template>
    </header>
    <table [appTable]="['Network Interface', 'URL', '']">
      @for (address of local(); track $index) {
        <tr>
          <td [style.width.rem]="12">{{ address.label }}</td>
          <td>{{ address.url | mask }}</td>
          <td [actions]="address.url"></td>
        </tr>
      }
    </table>
  `,
  host: { class: 'g-card' },
  imports: [
    TuiIcon,
    TuiTooltip,
    TuiLink,
    TableComponent,
    InterfaceActionsComponent,
    MaskPipe,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceLocalComponent {
  readonly local = input.required<readonly AddressDetails[]>()
}
