import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'header',
  template: `
    <img alt="Start9" src="assets/icons/favicon.svg" />
    <h1>StartTunnel</h1>
    <button
      tuiIconButton
      iconStart="@tui.menu"
      appearance="action-grayscale"
      (click.stop)="sidebars.start.set(!sidebars.start())"
    >
      Menu
    </button>
  `,
  styles: `
    :host {
      grid-column: span 2;
      display: flex;
      align-items: center;
      gap: 0.75rem;
      padding-inline-start: 0.75rem;
      background: var(--tui-background-neutral-2);
      box-shadow: var(--tui-shadow-medium);
      border-bottom: 1px solid var(--tui-border-normal);
    }

    h1 {
      font: var(--tui-typography-heading-h6);
      margin-inline-end: auto;
    }

    img {
      width: 2rem;
    }

    button {
      display: none;
    }

    :host-context(tui-root._mobile) {
      grid-column: span 1;

      button {
        display: inherit;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton],
})
export class Header {
  protected readonly sidebars = inject(SidebarService)
}
