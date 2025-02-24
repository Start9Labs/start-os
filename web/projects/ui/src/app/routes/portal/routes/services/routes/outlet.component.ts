import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router, RouterModule } from '@angular/router'
import { TuiAppearance, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiFade } from '@taiga-ui/kit'
import { TuiCell, tuiCellOptionsProvider } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { distinctUntilChanged, filter, map, switchMap, tap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

const ICONS = {
  dashboard: '@tui.layout-dashboard',
  actions: '@tui.clapperboard',
  instructions: '@tui.book-open-text',
  logs: '@tui.logs',
  about: '@tui.info',
}

@Component({
  template: `
    @if (service()) {
      <aside>
        <header tuiCell>
          <tui-avatar><img alt="" [src]="service()?.icon" /></tui-avatar>
          <span tuiTitle>
            <strong tuiFade>{{ manifest()?.title }}</strong>
            <span tuiSubtitle>{{ manifest()?.version }}</span>
          </span>
        </header>
        <nav>
          @for (item of nav; track $index) {
            <a
              tuiCell
              tuiAppearance="action-grayscale"
              routerLinkActive="active"
              [routerLinkActiveOptions]="{ exact: true }"
              [routerLink]="item === 'dashboard' ? './' : item"
            >
              <tui-icon [icon]="icons[item]" />
              <span tuiTitle>{{ item }}</span>
            </a>
          }
        </nav>
      </aside>
    }
    <router-outlet />
  `,
  host: { class: 'g-page' },
  styles: `
    :host {
      display: flex;
      padding: 0;
    }

    aside {
      position: sticky;
      top: 1px;
      left: 1px;
      margin: 1px;
      width: 16rem;
      padding: 0.5rem;
      text-transform: capitalize;
      box-shadow: 1px 0 var(--tui-border-normal);
      backdrop-filter: blur(1rem);
      background-color: color-mix(
        in hsl,
        var(--tui-background-base) 90%,
        transparent
      );
    }

    header {
      margin: 0 -0.5rem;
    }

    .active {
      color: var(--tui-text-primary);
    }

    :host-context(tui-root._mobile) {
      flex-direction: column;
      padding: 0;

      aside {
        top: 0;
        left: 0;
        width: 100%;
        padding: 0;
        margin: 0;
        z-index: 1;
        box-shadow: inset 0 1px 0 1px var(--tui-background-neutral-1);

        header {
          display: none;
        }

        nav {
          display: flex;
        }

        a {
          flex: 1;
          justify-content: center;
          border-radius: 0;
          background: var(--tui-background-neutral-1);

          &.active {
            background: none;
          }

          [tuiTitle] {
            display: none;
          }
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    RouterModule,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiAppearance,
    TuiIcon,
    TuiFade,
  ],
  providers: [tuiCellOptionsProvider({ height: 'spacious' })],
})
export class ServiceOutletComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly router = inject(Router)
  private readonly params = inject(ActivatedRoute).paramMap

  protected readonly icons = ICONS
  protected readonly nav = [
    'dashboard',
    'actions',
    'instructions',
    'logs',
    'about',
  ] as const

  protected readonly service = toSignal(
    this.router.events.pipe(
      switchMap(() => this.params),
      map(params => params.get('pkgId')),
      filter(Boolean),
      distinctUntilChanged(),
      switchMap(id => this.patch.watch$('packageData', id)),
      tap(pkg => {
        // if package disappears, navigate to list page
        if (!pkg) {
          this.router.navigate(['./portal/services'])
        }
      }),
    ),
  )

  protected readonly manifest = computed(
    (pkg = this.service()) => pkg && getManifest(pkg),
  )
}
