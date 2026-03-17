import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink, RouterLinkActive } from '@angular/router'
import { TuiButton } from '@taiga-ui/core'
import { TuiBadgeNotification } from '@taiga-ui/kit'
import { SidebarService } from 'src/app/services/sidebar.service'
import { UpdateService } from 'src/app/services/update.service'

@Component({
  selector: 'nav',
  template: `
    <div>
      @for (route of routes; track $index) {
        <a
          tuiButton
          size="s"
          appearance="flat-grayscale"
          routerLinkActive="active"
          [iconStart]="route.icon"
          [routerLink]="route.link"
        >
          {{ route.name }}
        </a>
      }
      <a
        tuiButton
        size="s"
        appearance="flat-grayscale"
        routerLinkActive="active"
        iconStart="@tui.settings"
        routerLink="settings"
      >
        Settings
        @if (update.hasUpdate()) {
          <tui-badge-notification size="s" appearance="positive" />
        }
      </a>
    </div>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      background: var(--tui-background-neutral-1);
      backdrop-filter: blur(1rem);
      z-index: 1;
      overflow: hidden;
      transition: transform var(--tui-duration);
    }

    div {
      flex: 1;
      padding-top: 1rem;
    }

    a {
      display: flex;
      justify-content: start;
      margin: 0 0.5rem;

      &.active {
        background: var(--tui-background-neutral-1);
      }

      tui-badge-notification {
        margin-inline-start: auto;
        background: var(--tui-status-positive);
      }
    }

    :host-context(tui-root._mobile) {
      position: absolute;
      top: 3.5rem;
      width: 14rem;
      bottom: 0;
      inset-inline-start: 0;

      &:not(:focus-within, ._expanded) {
        transform: translate3d(-100%, 0, 0);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiBadgeNotification, RouterLink, RouterLinkActive],
  host: {
    '[class._expanded]': 'sidebars.start()',
    '(document:click)': 'sidebars.start.set(false)',
    '(mousedown.prevent)': '0',
  },
})
export class Nav {
  protected readonly sidebars = inject(SidebarService)
  protected readonly update = inject(UpdateService)

  protected readonly routes = [
    {
      name: 'Subnets',
      icon: '@tui.network',
      link: 'subnets',
    },
    {
      name: 'Devices',
      icon: '@tui.laptop',
      link: 'devices',
    },
    {
      name: 'Port Forwards',
      icon: '@tui.globe',
      link: 'port-forwards',
    },
  ] as const
}
