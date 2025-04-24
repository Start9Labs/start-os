import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { TuiIcon, TuiLink } from '@taiga-ui/core'
import { TuiTooltip } from '@taiga-ui/kit'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { InterfaceActionsComponent } from './actions.component'
import { AddressDetails } from './interface.utils'
import { MaskPipe } from './mask.pipe'
import { DocsLinkDirective, i18nPipe } from '@start9labs/shared'

@Component({
  standalone: true,
  selector: 'section[local]',
  template: `
    <header>
      {{ 'Local' | i18n }}
      <tui-icon [tuiTooltip]="tooltip" />
      <ng-template #tooltip>
        {{
          'Local addresses can only be accessed by devices connected to the same LAN as your server, either directly or using a VPN.'
            | i18n
        }}
        <a tuiLink docsLink href="/user-manual/connecting-locally.html">
          {{ 'Learn More' | i18n }}
        </a>
      </ng-template>
    </header>
    <table [appTable]="['Network Interface', 'URL', null]">
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
    i18nPipe,
    DocsLinkDirective,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceLocalComponent {
  readonly local = input.required<readonly AddressDetails[]>()
}
