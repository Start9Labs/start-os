import { TuiIcon } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterModule } from '@angular/router'
import { TitleDirective } from 'src/app/services/title.service'
import { SettingsMenuComponent } from './components/menu.component'

@Component({
  template: `
    <ng-container *title><span>Settings</span></ng-container>
    <a
      routerLink="/portal/settings"
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
      span:not(:last-child),
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
  imports: [RouterModule, TuiIcon, SettingsMenuComponent, TitleDirective],
})
export class SettingsComponent {}
