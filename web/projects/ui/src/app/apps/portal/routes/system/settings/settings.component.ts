import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterModule } from '@angular/router'
import { TuiIconModule } from '@taiga-ui/experimental'
import { SettingsMenuComponent } from './components/menu.component'

@Component({
  template: `
    <a
      routerLink="/portal/system/settings"
      routerLinkActive="_current"
      [routerLinkActiveOptions]="{ exact: true }"
    >
      <tui-icon icon="tuiIconChevronLeft" />
      Settings
    </a>
    <settings-menu class="page" />
    <router-outlet />
  `,
  styles: [
    `
      :host {
        ::ng-deep tui-notification {
          position: sticky;
          left: 0;
        }
      }

      a {
        position: sticky;
        left: 0;
        display: inline-flex;
        align-items: center;
        gap: 0.5rem;
        margin: 1rem 0;
        font-size: 1rem;
        color: var(--tui-text-01);
      }

      ._current {
        display: none;
      }

      .page {
        display: none;
      }

      ._current + .page {
        display: flex;
        max-width: 45rem;
        margin: 0 auto;
      }
    `,
  ],
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [RouterModule, TuiIconModule, SettingsMenuComponent],
})
export class SettingsComponent {}
