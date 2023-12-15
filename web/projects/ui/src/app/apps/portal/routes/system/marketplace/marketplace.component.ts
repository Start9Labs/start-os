import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractCategoryService,
  AbstractMarketplaceService,
  FilterPackagesPipe,
} from '@start9labs/marketplace'
import { combineLatest, map } from 'rxjs'
import { MarketplaceNotificationComponent } from './components/notification.component'
import { MarketplaceMenuComponent } from './components/menu.component'
import { MarketplaceTileComponent } from './components/tile.component'
import { MarketplaceControlsComponent } from './components/controls.component'
import { MarketplacePreviewComponent } from './modals/preview.component'

@Component({
  standalone: true,
  template: `
    <marketplace-menu />
    <div
      class="sm:pl-[34vw] md:pl-[28vw] lg:pl-[22vw] 2xl:pl-[280px] min-h-screen flex justify-between overflow-auto scroll-smooth"
    >
      <div class="pt-24 sm:pt-3 md:pb-10 md:px-8">
        <marketplace-notification [url]="(details$ | async)?.url || ''" />
        <div class="mt-8 px-6 mb-10">
          <h1 class="text-4xl sm:text-5xl font-bold text-zinc-50/80">
            {{ category$ | async | titlecase }}
          </h1>
        </div>
        @if (filtered$ | async; as filtered) {
          <section
            class="p-6 md:p-8 grid grid-cols-1 xl:grid-cols-2 2xl:grid-cols-3 gap-16 list-none"
          >
            @for (pkg of filtered; track $index) {
              <marketplace-tile
                [pkg]="pkg"
                [style.--animation-order]="$index"
                class="block h-full"
              />
            }
          </section>
        } @else {
          <h1 class="text-xl pl-6">
            Loading
            <span class="loading-dots"></span>
          </h1>
        }
      </div>
    </div>
  `,
  styles: [
    `
      :host {
        max-height: 100%;
        overflow: auto;
        background: url('/assets/img/background.png') no-repeat center right
          fixed;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [FilterPackagesPipe],
  imports: [
    CommonModule,
    MarketplaceTileComponent,
    MarketplaceMenuComponent,
    MarketplaceNotificationComponent,
    MarketplaceControlsComponent,
    MarketplacePreviewComponent,
  ],
})
export class MarketplaceComponent {
  private readonly pipe = inject(FilterPackagesPipe)
  private readonly categoryService = inject(AbstractCategoryService)
  private readonly marketplaceService = inject(AbstractMarketplaceService)

  readonly details$ = this.marketplaceService.getSelectedHost$()
  readonly category$ = this.categoryService.getCategory$()
  readonly filtered$ = combineLatest([
    this.marketplaceService
      .getSelectedStore$()
      .pipe(map(({ packages }) => packages)),
    this.categoryService.getQuery$(),
    this.category$,
  ]).pipe(map(args => this.pipe.transform(...args)))
}
