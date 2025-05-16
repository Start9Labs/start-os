import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiIcon, TuiLink } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'tr[serviceInterface]',
  template: `
    <td>
      <strong>{{ info.name }}</strong>
    </td>
    <td>
      <tui-badge size="m" [appearance]="appearance">{{ info.type }}</tui-badge>
    </td>
    <td [style.text-align]="'center'">
      @if (info.public) {
        <tui-icon class="g-positive" icon="@tui.globe" />
      } @else {
        <tui-icon class="g-negative" icon="@tui.lock" />
      }
    </td>
    <td class="g-secondary" [style.grid-area]="'2 / span 4'">
      {{ info.description }}
    </td>
    <td>
      @if (info.type === 'ui') {
        <a
          tuiButton
          appearance="secondary"
          iconEnd="@tui.external-link"
          target="_blank"
          rel="noreferrer"
          size="xs"
          [style.margin-right.rem]="0.5"
          [attr.href]="href"
        >
          {{ 'Launch UI' | i18n }}
        </a>
      }
      <a tuiButton size="xs" [routerLink]="info.routerLink">
        {{ 'Manage' | i18n }}
      </a>
    </td>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    strong {
      white-space: nowrap;
    }

    tui-badge {
      text-transform: uppercase;
      font-weight: bold;
    }

    tui-icon {
      font-size: 1rem;
    }

    td:last-child {
      grid-area: 3 / span 4;
      white-space: nowrap;
      text-align: right;
      flex-direction: row-reverse;
      justify-content: flex-end;
      gap: 0.5rem;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: repeat(3, min-content) 1fr;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
        display: flex;
        padding: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton, TuiBadge, TuiLink, TuiIcon, RouterLink, i18nPipe],
})
export class ServiceInterfaceComponent {
  private readonly config = inject(ConfigService)

  @Input({ required: true })
  info!: T.ServiceInterface & {
    public: boolean
    routerLink: string
  }

  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input()
  disabled = false

  get appearance(): string {
    switch (this.info.type) {
      case 'ui':
        return 'positive'
      case 'api':
        return 'info'
      case 'p2p':
        return 'negative'
    }
  }

  get href(): string | null {
    return this.disabled
      ? null
      : this.config.launchableAddress(this.info, this.pkg.hosts)
  }
}
