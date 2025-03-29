import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  Marketplace,
  StoreIconComponentModule,
  StoreIdentity,
} from '@start9labs/marketplace'
import { TuiTable } from '@taiga-ui/addon-table'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiButton, TuiNotification, TuiTitle } from '@taiga-ui/core'
import {
  TuiAvatar,
  TuiBadgeNotification,
  TuiFade,
  TuiSkeleton,
} from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, tap } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { isInstalled, isUpdating } from 'src/app/utils/get-package-data'
import { FilterUpdatesPipe } from './filter-updates.pipe'
import { UpdatesItemComponent } from './item.component'

interface UpdatesData {
  hosts: StoreIdentity[]
  marketplace: Marketplace
  localPkgs: Record<string, PackageDataEntry<InstalledState | UpdatingState>>
  errors: string[]
}

@Component({
  template: `
    <ng-container *title>
      <label>Updates</label>
      @if (current()) {
        <button
          tuiIconButton
          iconStart="@tui.arrow-left"
          (click)="current.set(null)"
        >
          Back
        </button>
        {{ current()?.name }}
      }
    </ng-container>
    <aside class="g-aside">
      @for (registry of data()?.hosts; track $index) {
        <button
          tuiCell
          [class.g-secondary]="current()?.url !== registry.url"
          (click)="current.set(registry)"
        >
          <tui-avatar>
            <store-icon [url]="registry.url" [marketplace]="mp" />
          </tui-avatar>
          <span tuiTitle>
            <b tuiFade>{{ registry.name }}</b>
            <span tuiSubtitle tuiFade>{{ clear(registry.url) }}</span>
          </span>
          @if (
            (
              data()?.marketplace?.[registry.url]?.packages || []
              | filterUpdates: data()?.localPkgs
            ).length;
            as length
          ) {
            <tui-badge-notification>
              {{ length }}
            </tui-badge-notification>
          }
        </button>
      }
    </aside>
    <section class="g-subpage">
      @if (data()?.errors?.includes(current()?.url || '')) {
        <tui-notification appearance="negative">
          Request Failed
        </tui-notification>
      }
      <section class="g-card" [style.padding]="'0 1rem 1rem'">
        <table tuiTable class="g-table">
          <thead>
            <tr>
              <th tuiTh>Name</th>
              <th tuiTh>Version</th>
              <th tuiTh>Package Hash</th>
              <th tuiTh>Published</th>
              <th tuiTh></th>
            </tr>
          </thead>
          <tbody>
            @if (
              data()?.marketplace?.[current()?.url || '']?.packages;
              as packages
            ) {
              @if (packages | filterUpdates: data()?.localPkgs; as updates) {
                @for (pkg of updates; track $index) {
                  <updates-item
                    [item]="pkg"
                    [local]="data()?.localPkgs?.[pkg.id]!"
                  />
                } @empty {
                  <tr>
                    <td colspan="5">All services are up to date!</td>
                  </tr>
                }
              }
            } @else {
              <tr>
                <td colspan="5" [tuiSkeleton]="true">Loading</td>
              </tr>
              <tr>
                <td colspan="5" [tuiSkeleton]="true">Loading</td>
              </tr>
            }
          </tbody>
        </table>
      </section>
    </section>
  `,
  styles: `
    :host {
      display: flex;
      padding: 0;
    }

    aside {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
      padding-top: 1rem;
      color: var(--tui-text-secondary);
    }

    label:not(:last-child) {
      display: none;
    }

    [tuiCell] {
      white-space: nowrap;

      &:not(.g-secondary) {
        background: var(--tui-background-neutral-1);
        box-shadow: inset 0 0 0 1px var(--tui-background-neutral-1-hover);
        color: var(--tui-text-primary);
      }
    }

    td {
      clip-path: inset(0.5rem round var(--tui-radius-s));
    }

    :host-context(tui-root._mobile) {
      aside {
        width: 100%;
      }

      section {
        background: none;
        box-shadow: none;
      }

      [tuiCell] {
        color: var(--tui-text-primary) !important;
      }

      :host._selected {
        aside {
          display: none;
        }
      }

      :host:not(._selected) {
        section {
          display: none;
        }
      }
    }
  `,
  host: {
    class: 'g-page',
    '[class._selected]': 'current()',
  },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiNotification,
    TuiSkeleton,
    TuiTable,
    TuiBadgeNotification,
    TuiFade,
    TuiButton,
    StoreIconComponentModule,
    FilterUpdatesPipe,
    UpdatesItemComponent,
    TitleDirective,
  ],
})
export default class UpdatesComponent {
  private readonly isMobile = inject(TUI_IS_MOBILE)
  private readonly marketplaceService = inject(MarketplaceService)

  readonly mp = inject(ConfigService).marketplace
  readonly current = signal<StoreIdentity | null>(null)

  readonly data = toSignal<UpdatesData>(
    combineLatest({
      hosts: this.marketplaceService
        .getKnownHosts$(true)
        .pipe(tap(([store]) => !this.isMobile && this.current.set(store))),
      marketplace: this.marketplaceService.marketplace$,
      localPkgs: inject<PatchDB<DataModel>>(PatchDB)
        .watch$('packageData')
        .pipe(
          map(pkgs =>
            Object.entries(pkgs).reduce<
              Record<string, PackageDataEntry<InstalledState | UpdatingState>>
            >(
              (acc, [id, val]) =>
                isInstalled(val) || isUpdating(val)
                  ? { ...acc, [id]: val }
                  : acc,
              {},
            ),
          ),
        ),
      errors: this.marketplaceService.getRequestErrors$(),
    }),
  )

  clear(url: string): string {
    return url.replace(/https?:\/\//, '').replace(/\/$/, '')
  }
}
