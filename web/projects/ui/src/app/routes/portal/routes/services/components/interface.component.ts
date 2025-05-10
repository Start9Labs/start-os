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
      <a tuiLink [routerLink]="info.routerLink">
        <strong>{{ info.name }}</strong>
      </a>
    </td>
    <td>
      <tui-badge size="m" [appearance]="appearance">{{ info.type }}</tui-badge>
    </td>
    <td class="g-secondary" [style.grid-area]="'2 / span 4'">
      {{ info.description }}
    </td>
    <td [style.text-align]="'center'">
      @if (info.public) {
        <tui-icon class="g-positive" icon="@tui.globe" />
      } @else {
        <tui-icon class="g-negative" icon="@tui.lock" />
      }
    </td>
    <td [style.grid-area]="'span 2'">
      @if (info.type === 'ui') {
        <a
          tuiIconButton
          appearance="action"
          iconStart="@tui.external-link"
          target="_blank"
          rel="noreferrer"
          size="s"
          [style.border-radius.%]="100"
          [attr.href]="href"
          (click.stop)="(0)"
        >
          {{ 'Open' | i18n }}
        </a>
      }
    </td>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host {
      cursor: pointer;
      clip-path: inset(0 round var(--tui-radius-m));
      @include transition(background);
    }

    [tuiLink] {
      background: transparent;
    }

    @media ($tui-mouse) {
      :host:hover {
        background: var(--tui-background-neutral-1);
      }
    }

    strong {
      white-space: nowrap;
    }

    tui-badge {
      text-transform: uppercase;
    }

    tui-icon {
      font-size: 1rem;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: repeat(3, min-content) 1fr 2rem;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
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
        return 'primary'
      case 'api':
        return 'accent'
      case 'p2p':
        return 'primary-grayscale'
    }
  }

  get href(): string | null {
    return this.disabled
      ? null
      : this.config.launchableAddress(this.info, this.pkg.hosts)
  }
}
