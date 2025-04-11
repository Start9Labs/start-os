import {
  Component,
  computed,
  inject,
  TemplateRef,
  viewChildren,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink, RouterLinkActive } from '@angular/router'
import { TuiResponsiveDialogService, TuiTabBar } from '@taiga-ui/addon-mobile'
import { TuiIcon } from '@taiga-ui/core'
import { TuiBadgeNotification } from '@taiga-ui/kit'
import { BadgeService } from 'src/app/services/badge.service'
import { getMenu } from 'src/app/utils/system-utilities'

const FILTER = ['/portal/services', '/portal/system', '/portal/marketplace']

@Component({
  standalone: true,
  selector: 'app-tabs',
  template: `
    <nav tuiTabBar [(activeItemIndex)]="index">
      <a
        tuiTabBarItem
        icon="@tui.layout-grid"
        routerLink="/portal/services"
        routerLinkActive
        (isActiveChange)="update()"
      >
        Services
      </a>
      <a
        tuiTabBarItem
        icon="@tui.shopping-cart"
        routerLink="/portal/marketplace"
        routerLinkActive
        (isActiveChange)="update()"
      >
        Marketplace
      </a>
      <a
        tuiTabBarItem
        icon="@tui.settings"
        routerLink="/portal/system"
        routerLinkActive
        [badge]="badge()"
        (isActiveChange)="update()"
      >
        System
      </a>
      <button
        tuiTabBarItem
        icon="@tui.ellipsis"
        (click)="more(content)"
        [badge]="all()"
      >
        More
        <ng-template #content let-observer>
          @for (item of menu; track $index) {
            <a
              class="item"
              routerLinkActive="item_active"
              [routerLink]="item.routerLink"
              (click)="observer.complete()"
            >
              <tui-icon [icon]="item.icon" />
              {{ item.name }}
              @if (item.badge(); as badge) {
                <tui-badge-notification>{{ badge }}</tui-badge-notification>
              }
            </a>
          }
        </ng-template>
      </button>
    </nav>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host {
      display: none;
      backdrop-filter: blur(1rem);
      // TODO Theme
      --tui-background-elevation-1: #333;
      --tui-background-base: #fff;
      --tui-border-normal: var(--tui-background-neutral-1);
      --tui-status-negative: #f52222;
    }

    [tuiTabBar]::before {
      opacity: 0.7;
    }

    .item {
      @include button-clear();

      display: flex;
      padding: 0.75rem 0.25rem;
      gap: 1rem;
      align-items: center;

      &_active {
        color: var(--tui-text-action);
      }
    }

    :host-context(tui-root._mobile) {
      display: block;
    }
  `,
  imports: [
    RouterLink,
    RouterLinkActive,
    TuiTabBar,
    TuiBadgeNotification,
    TuiIcon,
  ],
})
export class TabsComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly links = viewChildren(RouterLinkActive)

  index = 3

  readonly menu = getMenu().filter(item => !FILTER.includes(item.routerLink))
  readonly badge = toSignal(inject(BadgeService).getCount('/portal/system'), {
    initialValue: 0,
  })

  readonly all = computed(() =>
    this.menu.reduce((acc, item) => acc + item.badge(), 0),
  )

  more(content: TemplateRef<any>) {
    this.dialogs.open(content, { label: 'Start OS' }).subscribe({
      complete: () => this.update(),
    })
  }

  update() {
    const index = this.links().findIndex(link => link.isActive)
    this.index = index === -1 ? 3 : index
  }
}
