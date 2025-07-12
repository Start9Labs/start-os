import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router, RouterModule } from '@angular/router'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiAppearance, TuiButton, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiFade } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { distinctUntilChanged, filter, map, switchMap, tap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import {
  PrimaryStatus,
  renderPkgStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { TitleDirective } from 'src/app/services/title.service'
import { getManifest } from 'src/app/utils/get-package-data'

const INACTIVE: PrimaryStatus[] = [
  'installing',
  'updating',
  'removing',
  'restoring',
  'backingUp',
]

@Component({
  template: `
    @if (service()) {
      <div *title class="title">
        <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
        <tui-avatar size="xs" [style.margin-inline-end.rem]="0.75">
          <img alt="" [src]="service()?.icon" />
        </tui-avatar>
        <span tuiFade>{{ manifest()?.title }}</span>
      </div>
      <aside class="g-aside">
        <header tuiCell>
          <tui-avatar><img alt="" [src]="service()?.icon" /></tui-avatar>
          <span tuiTitle>
            <strong tuiFade>{{ manifest()?.title }}</strong>
            <span tuiSubtitle [style.textTransform]="'none'">
              {{ manifest()?.version }}
            </span>
          </span>
        </header>
        <nav [attr.inert]="isInactive() ? '' : null">
          @for (item of nav; track $index) {
            <a
              tuiCell
              tuiAppearance="action-grayscale"
              routerLinkActive="active"
              [routerLinkActiveOptions]="{ exact: true }"
              [routerLink]="item.title === 'dashboard' ? './' : item.title"
            >
              <tui-icon [icon]="item.icon" />
              <span tuiTitle>{{ item.title | i18n }}</span>
              @if (item.title === 'dashboard') {
                <a routerLink="interface" routerLinkActive="active"></a>
              }
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

    .title {
      display: flex;
      align-items: center;
      margin-inline-start: -1rem;

      &:not(:only-child) {
        display: none;
      }
    }

    header {
      margin: 0 -0.5rem;
    }

    nav[inert] a:not(:first-child) {
      opacity: var(--tui-disabled-opacity);
    }

    a a {
      display: none;
    }

    .active,
    a:has(.active) {
      color: var(--tui-text-primary);

      [tuiTitle] {
        font-weight: bold;
      }
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
          padding: 0.125rem;
          background: var(--tui-background-neutral-1);
          box-shadow: inset 0 -1px var(--tui-background-neutral-1);

          &.active,
          &:has(.active) {
            background: none;
            box-shadow: none;
          }

          [tuiTitle] {
            display: none;
          }
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterModule,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiAppearance,
    TuiIcon,
    TuiFade,
    TitleDirective,
    TuiButton,
    i18nPipe,
  ],
})
export class ServiceOutletComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly router = inject(Router)
  private readonly params = inject(ActivatedRoute).paramMap

  protected readonly nav: { title: i18nKey; icon: string }[] = [
    { title: 'dashboard', icon: '@tui.layout-dashboard' },
    { title: 'actions', icon: '@tui.clapperboard' },
    { title: 'instructions', icon: '@tui.book-open-text' },
    { title: 'logs', icon: '@tui.logs' },
    { title: 'about', icon: '@tui.info' },
  ]

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
          this.router.navigate(['services'])
        }
      }),
    ),
  )

  protected readonly manifest = computed(
    (pkg = this.service()) => pkg && getManifest(pkg),
  )

  protected readonly isInactive = computed(
    (pkg = this.service()) =>
      !pkg || INACTIVE.includes(renderPkgStatus(pkg).primary),
  )
}
