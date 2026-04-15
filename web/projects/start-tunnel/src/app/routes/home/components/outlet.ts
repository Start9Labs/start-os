import {
  ChangeDetectionStrategy,
  Component,
  inject,
  linkedSignal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NavigationEnd,
  Router,
  RouterLink,
  RouterOutlet,
  TitleStrategy,
} from '@angular/router'
import { TUI_BREAKPOINT, TuiIcon, TuiScrollbar } from '@taiga-ui/core'
import { TuiNavigation } from '@taiga-ui/layout'
import { filter, map } from 'rxjs'
import { UpdateService } from 'src/app/services/update.service'

@Component({
  selector: 'app-outlet',
  template: `
    <header tuiNavigationHeader>
      <tui-icon icon="assets/icons/favicon.svg" />
      <h1>StartTunnel</h1>
    </header>
    <section>
      <aside [tuiNavigationAside]="open()">
        @for (route of routes; track $index) {
          <a tuiAsideItem [iconStart]="route.icon" [routerLink]="route.link">
            {{ route.name }}
          </a>
        }
        <a
          tuiAsideItem
          iconStart="@tui.settings"
          routerLink="settings"
          [iconEnd]="update.hasUpdate() ? '@tui.rocket' : ''"
        >
          Settings
        </a>
        <footer>
          <button
            tuiAsideItem
            type="button"
            [iconStart]="open() ? '@tui.chevron-left' : '@tui.chevron-right'"
            (click)="open.set(!open())"
          >
            {{ open() ? 'Collapse' : 'Expand' }}
          </button>
        </footer>
      </aside>
      <main tuiNavigationMain>
        <nav compact tuiSubheader>
          <b>{{ title() }}</b>
        </nav>
        <tui-scrollbar>
          <router-outlet />
        </tui-scrollbar>
      </main>
    </section>
  `,
  styles: `
    :host-context(tui-root._mobile) {
      main {
        min-inline-size: calc(100vw - 3rem);
        border: 0.375rem solid transparent;
      }

      tui-scrollbar {
        border-start-start-radius: 1rem;
        padding: 0;
      }

      nav {
        display: none;
      }
    }

    :host {
      display: flex;
      flex-direction: column;
      block-size: 100%;
      overflow: hidden;

      --tui-theme-color: var(--tui-background-neutral-1);

      header {
        clip-path: inset(0);
      }

      h1 {
        font: var(--tui-typography-body-l);
        font-weight: bold;
      }

      tui-icon {
        margin-inline: 0.25rem 0.5rem;
      }

      section {
        display: flex;
        flex: 1;
        overflow: hidden;
      }

      tui-scrollbar {
        min-inline-size: 100%;
        padding-inline-end: 1.5rem;
        border-radius: var(--tui-radius-s);

        > ::ng-deep tui-scroll-controls {
          transform: translateX(1.5rem);
        }
      }

      [tuiAsideItem]::after {
        color: var(--tui-status-positive);
      }

      nav {
        border-image: none;
        clip-path: inset(0);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterOutlet, TuiNavigation, RouterLink, TuiIcon, TuiScrollbar],
})
export class Outlet {
  protected readonly router = inject(Router)
  protected readonly breakpoint = inject(TUI_BREAKPOINT)
  protected readonly update = inject(UpdateService)
  protected readonly strategy = inject(TitleStrategy)
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

  protected readonly title = toSignal(
    this.router.events.pipe(
      filter(event => event instanceof NavigationEnd),
      map(() => this.strategy.buildTitle(this.router.routerState.snapshot)),
    ),
  )

  protected readonly open = linkedSignal<string[], boolean>({
    source: () => [this.breakpoint(), this.title() || ''],
    computation: (source, previous) =>
      previous?.value !== false && source[0] !== 'mobile',
  })
}
