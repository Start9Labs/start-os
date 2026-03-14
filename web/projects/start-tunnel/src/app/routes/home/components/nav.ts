import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router, RouterLink, RouterLinkActive } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import {
  TuiBadgeNotification,
  TuiNotificationMiddleService,
} from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'
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
    <button
      tuiButton
      iconStart="@tui.log-out"
      appearance="neutral"
      size="s"
      (click)="logout()"
    >
      Logout
    </button>
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

    button {
      width: 100%;
      border-radius: 0;
      justify-content: flex-start;
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
  private readonly service = inject(AuthService)
  private readonly router = inject(Router)
  protected readonly sidebars = inject(SidebarService)
  protected readonly api = inject(ApiService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
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

  protected async logout() {
    const loader = this.loader.open('').subscribe()
    try {
      await this.api.logout()
      this.service.authenticated.set(false)
      this.router.navigate(['.'])
    } catch (e: any) {
      console.error(e)
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
