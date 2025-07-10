import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  DOCUMENT,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
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
        <button
          tuiIconButton
          iconStart="@tui.external-link"
          appearance="flat-grayscale"
          [disabled]="disabled"
          (click)="openUI()"
        ></button>
      }
      <a
        tuiIconButton
        iconStart="@tui.settings"
        appearance="flat-grayscale"
        [routerLink]="info.routerLink"
      ></a>
    </td>
  `,
  styles: `
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
  imports: [TuiButton, TuiBadge, TuiIcon, RouterLink],
})
export class ServiceInterfaceItemComponent {
  private readonly config = inject(ConfigService)
  private readonly document = inject(DOCUMENT)

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

  get href() {
    return this.config.launchableAddress(this.info, this.pkg.hosts)
  }

  openUI() {
    this.document.defaultView?.open(this.href, '_blank', 'noreferrer')
  }
}
