import { AsyncPipe } from '@angular/common'
import { Component, inject } from '@angular/core'
import { RouterLink, RouterLinkActive } from '@angular/router'
import { TuiTabBarModule } from '@taiga-ui/addon-mobile'
import { combineLatest, map, startWith } from 'rxjs'
import { SYSTEM_UTILITIES } from 'src/app/apps/portal/constants/system-utilities'
import { BadgeService } from 'src/app/apps/portal/services/badge.service'
import { NotificationService } from 'src/app/apps/portal/services/notification.service'

@Component({
  standalone: true,
  selector: 'app-tabs',
  template: `
    <nav tuiTabBar>
      <a
        tuiTabBarItem
        icon="tuiIconGrid"
        routerLink="/portal/dashboard"
        routerLinkActive
        [routerLinkActiveOptions]="{ exact: true }"
      >
        Services
      </a>
      <a
        tuiTabBarItem
        icon="tuiIconActivity"
        routerLink="/portal/dashboard"
        routerLinkActive
        [routerLinkActiveOptions]="{ exact: true }"
        [queryParams]="{ tab: 'metrics' }"
      >
        Metrics
      </a>
      <a
        tuiTabBarItem
        icon="tuiIconSettings"
        routerLink="/portal/dashboard"
        routerLinkActive
        [routerLinkActiveOptions]="{ exact: true }"
        [queryParams]="{ tab: 'utilities' }"
        [badge]="(utils$ | async) || 0"
      >
        Utilities
      </a>
      <a
        tuiTabBarItem
        routerLinkActive
        routerLink="/portal/system/notifications"
        icon="tuiIconBell"
        [badge]="(notification$ | async) || 0"
      >
        Notifications
      </a>
    </nav>
  `,
  styles: `
    :host {
      display: none;
      // TODO: Theme
      --tui-elevation-01: #333;
      --tui-base-04: var(--tui-clear);
      backdrop-filter: blur(1rem);
    }

    [tuiTabBar]::before {
      opacity: 0.7;
    }

    :host-context(tui-root._mobile) {
      display: block;
    }
  `,
  imports: [AsyncPipe, RouterLink, RouterLinkActive, TuiTabBarModule],
})
export class TabsComponent {
  private readonly badge = inject(BadgeService)

  readonly utils$ = combineLatest(
    Object.keys(SYSTEM_UTILITIES)
      .filter(key => key !== '/portal/system/notifications')
      .map(key => this.badge.getCount(key).pipe(startWith(0))),
  ).pipe(map(values => values.reduce((acc, value) => acc + value, 0)))
  readonly notification$ = inject(NotificationService).unreadCount$
}
