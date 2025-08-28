import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'
import { InterfaceService } from 'src/app/routes/portal/components/interfaces/interface.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'tr[serviceInterface]',
  template: `
    <td><ng-content /></td>
    <td>
      <tui-badge size="m" [appearance]="appearance">{{ info.type }}</tui-badge>
    </td>
    <td class="g-secondary" [style.grid-area]="'2 / 1 / 2 / 3'">
      {{ info.description }}
    </td>
    <td>
      @if (info.type === 'ui') {
        <a
          tuiIconButton
          iconStart="@tui.external-link"
          appearance="flat-grayscale"
          target="_blank"
          rel="noopener noreferrer"
          [attr.href]="disabled ? null : href"
          (click.stop)="(0)"
        ></a>
      }
    </td>
  `,
  styles: `
    :host {
      clip-path: inset(0 round 0.75rem);
      cursor: pointer;

      &:hover {
        background: var(--tui-background-neutral-1);
      }
    }

    td:first-child {
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
      grid-area: 1 / 3 / span 2 / 3;
      white-space: nowrap;
      text-align: right;
      flex-direction: row-reverse;
      justify-content: flex-end;
      gap: 0.5rem;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: min-content;
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
  imports: [TuiButton, TuiBadge],
})
export class ServiceInterfaceItemComponent {
  private readonly interfaceService = inject(InterfaceService)

  @Input({ required: true })
  info!: T.ServiceInterface

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
    const host = this.pkg.hosts[this.info.addressInfo.hostId]

    return host
      ? this.interfaceService.launchableAddress(this.info, host)
      : null
  }
}
