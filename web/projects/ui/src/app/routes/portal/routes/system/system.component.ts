import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterModule } from '@angular/router'
import { TitleDirective } from 'src/app/services/title.service'
import { SystemMenuComponent } from './components/menu.component'

@Component({
  template: `
    <ng-container *title><span>System System</span></ng-container>
    <system-menu />
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

      span:not(:last-child),
      system-menu:not(:nth-last-child(2)) {
        display: none;
      }

      system-menu,
      router-outlet + ::ng-deep * {
        display: flex;
        flex-direction: column;
        gap: 1rem;
        margin: 0 auto;
        max-width: 45rem;
      }
    `,
  ],
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [RouterModule, SystemMenuComponent, TitleDirective],
})
export class SystemComponent {}
