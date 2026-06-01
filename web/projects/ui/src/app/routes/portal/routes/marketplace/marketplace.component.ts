import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router } from '@angular/router'
import {
  FilterPackagesPipe,
  MarketplaceAsideComponent,
  StoreIconDirective,
} from '@start9labs/marketplace'
import { DialogService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiCell, TuiScrollbar, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiFade, TuiSkeleton } from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
import { tap } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { StorageService } from 'src/app/services/storage.service'
import { TitleDirective } from 'src/app/services/title.service'

import { MarketplaceTileComponent } from './components/tile.component'
import { MARKETPLACE_REGISTRY } from './modals/registry.component'

@Component({
  template: `
    <ng-container *title>{{ 'Marketplace' | i18n }}</ng-container>
    <marketplace-aside
      [registry]="registry()"
      [(sort)]="sort"
      [(category)]="category"
      [(query)]="query"
    >
      <button tuiButton iconEnd="@tui.repeat" (click)="changeRegistry()">
        <span tuiAvatar appearance="action-grayscale" size="xs">
          <img [storeIcon]="registry()?.url" />
        </span>
        <span tuiFade [tuiSkeleton]="!registry()?.info?.name">
          {{ registry()?.info?.name || 'Loading...' }}
        </span>
      </button>
    </marketplace-aside>
    <tui-scrollbar [style.flex]="1">
      <section>
        @if (registry(); as reg) {
          @for (
            pkg of reg.packages | filterPackages: query() : category() : sort();
            track $index
          ) {
            <button type="button" [marketplaceTile]="pkg"></button>
          }
        } @else {
          @for (_ of '-'.repeat(15); track $index) {
            <div tuiCardLarge="compact" [tuiSkeleton]="true">
              <span tuiCell>
                <span tuiAvatar></span>
                <span tuiTitle>
                  Loading
                  <span tuiSubtitle>Loading</span>
                </span>
              </span>
              <span tuiDescription>Loading</span>
            </div>
          }
        }
      </section>
    </tui-scrollbar>
  `,
  host: { class: 'g-page' },
  styles: `
    :host {
      display: flex;
      overflow: hidden;
      padding: 1px;
      background: #1c1d26;
    }

    [tuiButton] {
      inline-size: 100%;
      justify-content: flex-start;

      [tuiAvatar] {
        margin-inline-start: -0.375rem;
      }

      &::after {
        margin-inline-start: auto;
        color: var(--tui-text-tertiary);
      }
    }

    section {
      padding: 1rem;
      display: grid;
      gap: 1rem;
      grid-template-columns: repeat(auto-fill, minmax(17rem, 1fr));
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarketplaceTileComponent,
    TuiScrollbar,
    FilterPackagesPipe,
    TitleDirective,
    i18nPipe,
    MarketplaceAsideComponent,
    TuiButton,
    StoreIconDirective,
    TuiAvatar,
    TuiFade,
    TuiSkeleton,
    TuiCardLarge,
    TuiCell,
    TuiTitle,
  ],
})
export default class MarketplaceComponent {
  private readonly dialog = inject(DialogService)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly configService = inject(ConfigService)
  private readonly router = inject(Router)
  private readonly storage = inject(StorageService)
  private readonly route = inject(ActivatedRoute)
    .queryParamMap.pipe(
      takeUntilDestroyed(),
      tap(params => {
        const registry = params.get('registry')

        // Only override the query from the URL when `search` is explicitly
        // present (e.g. a deep link from a dependency tile). Otherwise we'd
        // wipe out the user's typed search every time another query param
        // changes (such as `id`/`flavor` when opening a service drawer).
        if (params.has('search')) {
          this.query.set(params.get('search') || '')
        }

        if (!registry) {
          this.router.navigate([], {
            queryParams: {
              registry:
                this.storage.get('selectedRegistry') ||
                this.configService.defaultRegistry,
            },
            queryParamsHandling: 'merge',
          })
        } else {
          this.marketplaceService.currentRegistryUrl$.next(registry)
        }
      }),
    )
    .subscribe()

  readonly sort = signal('a')
  readonly category = signal('all')
  readonly query = signal('')
  protected readonly registry = toSignal(
    this.marketplaceService.currentRegistry$,
  )

  changeRegistry() {
    this.dialog.openComponent(MARKETPLACE_REGISTRY).subscribe()
  }
}
