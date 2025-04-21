import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink, RouterLinkActive } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import {
  TUI_ANIMATIONS_SPEED,
  tuiFadeIn,
  TuiHint,
  TuiIcon,
  tuiScaleIn,
  tuiToAnimationOptions,
  tuiWidthCollapse,
} from '@taiga-ui/core'
import { TuiBadgedContent, TuiBadgeNotification } from '@taiga-ui/kit'
import { getMenu } from 'src/app/utils/system-utilities'

@Component({
  standalone: true,
  selector: 'header-navigation',
  template: `
    @for (item of utils; track $index) {
      <a
        #rla="routerLinkActive"
        class="link"
        routerLinkActive="link_active"
        tuiHintDirection="bottom"
        [tuiHintShowDelay]="1000"
        [routerLink]="item.routerLink"
        [class.link_system]="item.routerLink === '/portal/system'"
        [tuiHint]="!rla.isActive ? item.name : ''"
      >
        <tui-badged-content
          [style.--tui-radius.%]="50"
          [@tuiFadeIn]="animation"
          [@tuiWidthCollapse]="animation"
          [@tuiScaleIn]="animation"
        >
          @if (item.badge(); as badge) {
            <tui-badge-notification tuiSlot="top" size="s">
              {{ badge }}
            </tui-badge-notification>
          }
          <tui-icon [icon]="item.icon" />
        </tui-badged-content>
        <span>{{ item.name | i18n }}</span>
      </a>
    }
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        position: relative;
        display: flex;
        border-radius: inherit;
        margin-inline-end: 0.875rem;
        isolation: isolate;

        &::before {
          content: '';
          position: absolute;
          top: 0;
          left: -1rem;
          right: -0.5rem;
          bottom: 0;
          transform: skewX(30deg);
          border-radius: var(--bumper);
          z-index: -1;
          backdrop-filter: blur(1rem);
        }
      }

      .link {
        @include transition(all);
        position: relative;
        display: grid;
        grid-template-columns: 1.5rem 0fr;
        align-items: center;
        padding: 0 0.5rem;
        margin: 0;
        border-radius: inherit;
        color: var(--tui-text-secondary);

        &:not(.link_active):hover tui-icon {
          transform: scale(1.1);
        }

        &:not(.link_active):active tui-icon {
          transform: scale(0.8);
        }

        &::before {
          @include transition(all);
          content: '';
          position: absolute;
          inset: 0;
          transform: skewX(30deg);
          background: color-mix(in hsl, var(--start9-base-2) 75%, transparent);
          box-shadow: inset 0 1px rgb(255 255 255 / 25%);
          z-index: -1;
        }

        span {
          @include transition(opacity);
          position: relative;
          overflow: hidden;
          text-indent: 0.5rem;
          opacity: 0;
        }

        &:hover,
        &_active {
          color: var(--tui-text-primary);

          tui-icon {
            color: var(--tui-text-primary);
          }
        }

        &_active {
          grid-template-columns: 1.5rem 1fr;
          padding: 0 1rem;
          margin: 0 calc(var(--bumper) + 0.5rem);

          &.link_system {
            pointer-events: none;
          }

          + .link::before {
            left: -0.5rem;
            border-top-left-radius: var(--bumper);
            border-bottom-left-radius: var(--bumper);
          }

          &::before {
            border-radius: var(--bumper);
            filter: brightness(0.5);
          }

          span {
            opacity: 1;
          }
        }

        &:has(+ .link_active)::before {
          right: -0.5rem;
          border-top-right-radius: var(--bumper);
          border-bottom-right-radius: var(--bumper);
        }

        &:first-child {
          padding: 0 0.5rem 0 1rem !important;
          margin-inline-start: 0;

          &::before {
            left: -2rem;
          }
        }

        &:last-child {
          margin-inline-end: 0;

          &::before {
            right: -0.5rem;
            border-top-right-radius: inherit;
            border-bottom-right-radius: inherit;
          }
        }
      }

      tui-icon {
        @include transition(transform);
        color: var(--tui-text-secondary);
      }

      :host-context(tui-root._mobile) {
        display: none;
      }
    `,
  ],
  animations: [tuiFadeIn, tuiWidthCollapse, tuiScaleIn],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiBadgeNotification,
    TuiBadgedContent,
    RouterLink,
    TuiIcon,
    RouterLinkActive,
    TuiHint,
    i18nPipe,
  ],
})
export class HeaderNavigationComponent {
  readonly animation = tuiToAnimationOptions(inject(TUI_ANIMATIONS_SPEED))
  readonly utils = getMenu()
}
