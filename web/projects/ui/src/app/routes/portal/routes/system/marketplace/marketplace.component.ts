import { TuiScrollbar } from '@taiga-ui/core'
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
import { MarketplaceSidebarsComponent } from './components/sidebars.component'

@Component({
  standalone: true,
  template: `
    <marketplace-menu />
    <tui-scrollbar>
      <div class="marketplace-content-wrapper">
        <div class="marketplace-content-inner">
          <marketplace-notification [url]="(details$ | async)?.url || ''" />
          <div class="title-wrapper">
            <h1>
              {{ category$ | async | titlecase }}
            </h1>
          </div>
          @if (filtered$ | async; as filtered) {
            <section class="marketplace-content-list">
              @for (pkg of filtered; track $index) {
                <marketplace-tile
                  [pkg]="pkg"
                  [style.--animation-order]="$index"
                  class="tile-wrapper"
                />
              }
            </section>
          } @else {
            <h1 class="loading-text">
              Loading
              <span class="loading-dots"></span>
            </h1>
          }
        </div>
      </div>
    </tui-scrollbar>
    <marketplace-sidebars />
  `,
  host: { class: 'g-page' },
  styles: [
    `
      :host {
        height: calc(100vh - 3.5rem);
        display: flex;
        flex-direction: column;
        overflow: hidden;
        padding: 0;
        background: rgb(55 58 63 / 90%)
          url('/assets/img/background_marketplace.png') no-repeat top right;
      }

      .marketplace-content {
        &-wrapper {
          @media (min-width: 768px) {
            padding-left: 17rem;
          }

          @media (min-width: 1536px) {
            padding-left: 18rem;
          }
        }

        &-inner {
          padding-top: 6rem;

          @media (min-width: 768px) {
            padding: 0 2rem 2.5rem 2rem;
          }

          .title-wrapper {
            margin: 2rem 0 2.5rem 0;
            padding: 0 1.5rem;

            h1 {
              font-size: 2.25rem;
              line-height: 2.5rem;
              font-weight: 700;
              color: rgb(250 250 250 / 0.8);
              pointer-events: none;

              @media (min-width: 640px) {
                font-size: 3rem;
                line-height: 1;
              }
            }
          }
        }

        &-list {
          display: grid;
          grid-template-columns: repeat(1, minmax(0, 1fr));
          gap: 4rem 3rem;
          padding: 1.5rem;

          @media (min-width: 768px) {
            padding: 2rem;
          }
          @media (min-width: 1024px) {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          @media (min-width: 1280px) {
            grid-template-columns: repeat(3, minmax(0, 1fr));
          }
          @media (min-width: 1536px) {
            grid-template-columns: repeat(4, minmax(0, 1fr));
          }

          .tile-wrapper {
            display: block;
            height: 100%;
          }
        }
      }

      .loading-text {
        font-size: 1.25rem;
        line-height: 1.75rem;
        padding-left: 1.5rem;
      }

      :host-context(tui-root._mobile) {
        height: calc(100vh - 3.5rem - var(--tui-height-l));
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
    MarketplaceSidebarsComponent,
    TuiScrollbar,
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
