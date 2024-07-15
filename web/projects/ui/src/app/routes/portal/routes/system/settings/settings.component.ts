import { TuiIcon } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterModule } from '@angular/router'
import { SettingsMenuComponent } from './components/menu.component'

@Component({
  template: `
    <a
      routerLink="/portal/system/settings"
      routerLinkActive="_current"
      [routerLinkActiveOptions]="{ exact: true }"
    ></a>
    <settings-menu />
    <router-outlet />
  `,
  styles: [
    `
      :host {
        padding-top: 1rem;

        ::ng-deep tui-notification {
          position: sticky;
          left: 0;
        }
      }

      a,
      settings-menu {
        display: none;
      }

      ._current + settings-menu {
        display: flex;
        max-width: 30rem;
        margin: 0 auto;
      }
    `,
  ],
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [RouterModule, TuiIcon, SettingsMenuComponent],
})
export class SettingsComponent {}
