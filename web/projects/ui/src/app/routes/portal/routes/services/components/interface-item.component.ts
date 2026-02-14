import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  selector: 'tr[serviceInterface]',
  template: `
    <td><ng-content /></td>
    <td>
      <tui-badge size="m" [appearance]="appearance">
        {{ info().type }}
      </tui-badge>
    </td>
    <td class="g-secondary" [style.grid-area]="'2 / 1 / 2 / 3'">
      {{ info().description }}
    </td>
    <td class="actions">
      <a
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.settings"
        size="s"
        [routerLink]="link()"
      >
        Settings
      </a>
    </td>
  `,
  styles: `
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

    .actions {
      text-align: end;
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

      .actions {
        grid-area: 1 / 2 / 3 / 3;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiBadge, TuiButton, RouterLink],
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
