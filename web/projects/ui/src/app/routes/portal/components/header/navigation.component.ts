import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink, RouterLinkActive } from '@angular/router'
import {
  TUI_ANIMATIONS_SPEED,
  TuiButton,
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
        [routerLink]="item.routerLink"
        [tuiHint]="$index > 3 && !rla.isActive ? item.name : ''"
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
        <span>{{ item.name }}</span>
      </a>
    }
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        display: flex;
        backdrop-filter: blur(1rem);
        border-radius: inherit;
        padding-inline-end: 0.75rem;
        margin-inline-end: -0.4375rem;
        isolation: isolate;
      }

      .link {
        @include transition(all);
        position: relative;
        display: grid;
        grid-template-columns: 1.5rem 0fr;
        align-items: center;
        padding: 0 0 0 1rem;
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
          background: rgb(75 75 75 / 65%);
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

        &:nth-child(1):hover,
        &:nth-child(2):hover,
        &:nth-child(3):hover,
        &:nth-child(4):hover {
          grid-template-columns: 1.5rem 1fr;

          span {
            opacity: 1;
          }
        }

        &_active {
          grid-template-columns: 1.5rem 1fr;
          padding: 0 1rem;
          margin: 0 var(--bumper);

          + .link::before {
            border-top-left-radius: var(--bumper);
            border-bottom-left-radius: var(--bumper);
          }

          &::before {
            border-radius: var(--bumper);
            filter: brightness(0.65);
          }

          span {
            opacity: 1;
          }
        }

        &:has(+ .link_active)::before {
          border-top-right-radius: var(--bumper);
          border-bottom-right-radius: var(--bumper);
        }

        &:has(~ .link_active) {
          padding: 0 1rem 0 0;
        }

        &:first-child {
          padding-inline-start: 1rem !important;
          margin-inline-start: 0;

          &::before {
            left: -2rem;
          }
        }

        &:last-child {
          padding-inline-end: 1rem !important;
          margin-inline-end: 0;

          &::before {
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
    TuiButton,
    RouterLink,
    TuiIcon,
    RouterLinkActive,
    TuiHint,
  ],
})
export class HeaderNavigationComponent {
  readonly animation = tuiToAnimationOptions(inject(TUI_ANIMATIONS_SPEED))
  readonly utils = getMenu()
}
