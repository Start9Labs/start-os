import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiIcon } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  selector: 'tr[serviceInterface]',
  template: `
    <td><ng-content /></td>
    <td>
      <span tuiBadge size="m" [appearance]="appearance">
        {{ info().type }}
      </span>
    </td>
    <td class="g-secondary" [style.grid-area]="'2 / 1 / 2 / 3'">
      {{ info().description }}
    </td>
    <td class="chevron">
      <tui-icon icon="@tui.chevron-right" />
    </td>
  `,
  styles: `
    :host {
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

    .chevron {
      text-align: end;
    }

    .chevron tui-icon {
      font-size: 1rem;
      color: var(--tui-text-tertiary);
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: 1fr auto;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
        padding: 0;
      }

      .chevron {
        grid-area: 1 / 2 / 3 / 3;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiBadge, TuiIcon],
})
export class ServiceInterfaceItemComponent {
  readonly info = input.required<T.ServiceInterface>()
  readonly link = input.required<string>()

  get appearance(): string {
    switch (this.info().type) {
      case 'ui':
        return 'positive'
      case 'api':
        return 'info'
      case 'p2p':
        return 'negative'
    }
  }
}
