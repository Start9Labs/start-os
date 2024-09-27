import {
  Component,
  computed,
  inject,
  TemplateRef,
  viewChildren,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink, RouterLinkActive } from '@angular/router'
import { TuiSheetDialogService, TuiTabBar } from '@taiga-ui/addon-mobile'
import { TuiDialogService, TuiIcon } from '@taiga-ui/core'
import { TuiBadgeNotification } from '@taiga-ui/kit'
import { ABOUT } from 'src/app/routes/portal/components/header/about.component'
import { BadgeService } from 'src/app/services/badge.service'
import { RESOURCES } from 'src/app/utils/resources'
import { getMenu } from 'src/app/utils/system-utilities'

const FILTER = ['/portal/system/settings', '/portal/system/marketplace']

@Component({
  standalone: true,
  selector: 'app-tabs',
  template: `
    <nav tuiTabBar [(activeItemIndex)]="index">
      <a
        tuiTabBarItem
        icon="@tui.layout-grid"
        routerLink="/portal/dashboard"
        routerLinkActive
        (isActiveChange)="update()"
      >
        Services
      </a>
      <a
        tuiTabBarItem
        icon="@tui.shopping-cart"
        routerLink="/portal/system/marketplace"
        routerLinkActive
        (isActiveChange)="update()"
      >
        Marketplace
      </a>
      <a
        tuiTabBarItem
        icon="@tui.settings"
        routerLink="/portal/system/settings"
        routerLinkActive
        [badge]="badge()"
        (isActiveChange)="update()"
      >
        Settings
      </a>
      <a
        tuiTabBarItem
        icon="@tui.settings"
        routerLink="/portal/system/settings"
        routerLinkActive
        [badge]="badge()"
        (isActiveChange)="update()"
      >
        Settings
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
          <button class="item" (click)="about()">
            <tui-icon icon="@tui.info" />
            About this server
          </button>
          @for (link of resources; track $index) {
            <a class="item" target="_blank" rel="noreferrer" [href]="link.href">
              <tui-icon [icon]="link.icon" />
              {{ link.name }}
              <tui-icon
                icon="@tui.external-link"
                [style.margin-inline-start]="'auto'"
              />
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
      // TODO: Theme
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
  private readonly sheets = inject(TuiSheetDialogService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly links = viewChildren(RouterLinkActive)

  index = 3

  readonly resources = RESOURCES
  readonly menu = getMenu().filter(item => !FILTER.includes(item.routerLink))
  readonly badge = toSignal(
    inject(BadgeService).getCount('/portal/system/settings'),
    { initialValue: 0 },
  )

  readonly all = computed(() =>
    this.menu.reduce((acc, item) => acc + item.badge(), 0),
  )

  about() {
    this.dialogs.open(ABOUT, { label: 'About this server' }).subscribe()
  }

  more(content: TemplateRef<any>) {
    this.sheets.open(content, { label: 'Start OS' }).subscribe({
      complete: () => this.update(),
    })
  }

  update() {
    const index = this.links().findIndex(link => link.isActive)
    this.index = index === -1 ? 3 : index
  }
}
