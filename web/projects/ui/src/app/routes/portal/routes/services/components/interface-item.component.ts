import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiBadge } from '@taiga-ui/kit'

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

    tui-icon {
      font-size: 1rem;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: min-content;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
        padding: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiBadge],
})
export class ServiceInterfaceItemComponent {
  @Input({ required: true })
  info!: T.ServiceInterface

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
}
