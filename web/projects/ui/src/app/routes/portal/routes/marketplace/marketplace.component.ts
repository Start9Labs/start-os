import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { takeUntilDestroyed, toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { ActivatedRoute, Router } from '@angular/router'
import {
  FilterPackagesPipe,
  MarketplaceAsideComponent,
} from '@start9labs/marketplace'
import { i18nPipe, LocalizePipe } from '@start9labs/shared'
import {
  TuiCell,
  TuiDataList,
  TuiDropdown,
  TuiScrollbar,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiAvatar, TuiChevron, TuiSelect, TuiSkeleton } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { tap } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { StorageService } from 'src/app/services/storage.service'
import { TitleDirective } from 'src/app/services/title.service'

import { MarketplaceRegistrySelectComponent } from './components/registry-select.component'
import { MarketplaceTileComponent } from './components/tile.component'

@Component({
  template: `
    <ng-container *title>{{ 'Marketplace' | i18n }}</ng-container>
    <marketplace-aside
      [registry]="registry()"
      [(category)]="category"
      [(query)]="query"
    >
      <marketplace-registry-select />
    </marketplace-aside>
    <div class="content">
      <header tuiHeader="h4">
        <hgroup tuiTitle>
          <h2 [tuiSkeleton]="!registry()">{{ categoryName() | localize }}</h2>
        </hgroup>
        <aside tuiAccessories>
          <tui-textfield
            tuiTextfieldSize="s"
            tuiChevron
            [tuiTextfieldCleaner]="false"
          >
            <input tuiSelect [(ngModel)]="sortLabel" />
            <tui-data-list *tuiDropdown>
              @for (key of sortKeys; track key) {
                <button tuiOption [value]="getLabel(key)">
                  {{ getLabel(key) }}
                </button>
              }
            </tui-data-list>
          </tui-textfield>
        </aside>
      </header>
      <tui-scrollbar [style.flex]="1">
        <section>
          @if (registry(); as reg) {
            @for (
              pkg of reg.packages
                | filterPackages: query() : category() : sort();
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
    </div>
  `,
  host: { class: 'g-page' },
  styles: `
    :host {
      display: flex;
      overflow: hidden;
      padding: 1px;
      background: #1c1d26;
    }

    .content {
      flex: 1;
      min-width: 0;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }

    [tuiHeader] {
      align-items: center;
      flex-wrap: wrap;
      gap: 0.5rem 1rem;
      padding: 1rem 2rem 0;
    }

    [tuiHeader] [tuiAccessories] {
      flex-shrink: 0;
      gap: 0;
    }

    tui-textfield {
      inline-size: 10rem;
    }

    tui-scrollbar {
      min-height: 0;
    }

    section {
      padding: 1rem 2rem;
      display: grid;
      gap: 1rem;
      grid-template-columns: repeat(auto-fill, minmax(17rem, 1fr));
    }

    :host-context(tui-root._mobile) [tuiHeader],
    :host-context(tui-root._mobile) section {
      padding-inline: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    MarketplaceTileComponent,
    TuiScrollbar,
    FilterPackagesPipe,
    TitleDirective,
    i18nPipe,
    LocalizePipe,
    MarketplaceAsideComponent,
    MarketplaceRegistrySelectComponent,
    TuiChevron,
    TuiDataList,
    TuiDropdown,
    TuiHeader,
    TuiSelect,
    TuiAvatar,
    TuiSkeleton,
    TuiCardLarge,
    TuiCell,
    TuiTitle,
  ],
})
export default class MarketplaceComponent {
  private readonly i18n = inject(i18nPipe)
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
  protected readonly sortKeys = ['a', '1']
  protected readonly registry = toSignal(
    this.marketplaceService.currentRegistry$,
  )

  protected get sortLabel(): string {
    return this.getLabel(this.sort())
  }

  protected set sortLabel(label: string) {
    this.sort.set(
      this.sortKeys.find(key => this.getLabel(key) === label) || 'a',
    )
  }

  protected readonly categoryName = computed(() => {
    const category = this.category()
    return this.registry()?.info?.categories?.[category]?.name || category
  })

  getLabel(sort: string) {
    switch (sort) {
      case 'a':
        return this.i18n.transform('Alphabetical')
      default:
        return this.i18n.transform('Recently updated')
    }
  }
}
