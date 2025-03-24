import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterModule } from '@angular/router'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiBadgeNotification } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { i18nPipe } from 'src/app/i18n/i18n.pipe'
import { BadgeService } from 'src/app/services/badge.service'
import { TitleDirective } from 'src/app/services/title.service'
import { SYSTEM_MENU } from './system.const'

@Component({
  template: `
    <span *title>{{ 'system.outlet.general' | i18n }}</span>
    <aside class="g-aside">
      @for (cat of menu; track $index) {
        @if ($index) {
          <hr [style.margin.rem]="0.5" />
        }
        @for (page of cat; track $index) {
          <a
            tuiCell="s"
            routerLinkActive="active"
            [routerLink]="page.item.split('.').at(-1)"
          >
            <tui-icon [icon]="page.icon" />
            <span tuiTitle>
              <span>
                {{ page.item | i18n }}
                @if (page.item === 'system.outlet.general' && badge()) {
                  <tui-badge-notification>{{ badge() }}</tui-badge-notification>
                }
              </span>
            </span>
          </a>
        }
      }
    </aside>
    <router-outlet />
  `,
  styles: [
    `
      :host {
        display: flex;
        padding: 0;
      }

      [tuiCell] {
        color: var(--tui-text-secondary);

        &.active {
          color: var(--tui-text-primary);

          [tuiTitle] {
            font-weight: bold;
          }
        }
      }

      span:not(:last-child) {
        display: none;
      }

      router-outlet + ::ng-deep * {
        height: fit-content;
        flex: 1;
        display: flex;
        flex-direction: column;
        gap: 1rem;
        padding: 1rem;
      }

      :host-context(tui-root._mobile) {
        aside {
          padding: 0;
          width: 100%;
          background: none;
          box-shadow: none;

          &:not(:nth-last-child(2)) {
            display: none;
          }
        }

        [tuiCell] {
          color: var(--tui-text-primary);
          margin: 0.5rem 0;

          [tuiTitle] {
            font: var(--tui-font-text-l);
          }
        }

        hr {
          background: var(--tui-border-normal);
        }
      }
    `,
  ],
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    RouterModule,
    TuiCell,
    TuiIcon,
    TuiTitle,
    TitleDirective,
    TuiBadgeNotification,
    i18nPipe,
  ],
})
export class SystemComponent {
  readonly menu = SYSTEM_MENU
  readonly badge = toSignal(inject(BadgeService).getCount('/portal/system'))
}
