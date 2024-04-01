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
    <marketplace-sidebars />
  `,
  styles: [
    `
      :host {
        max-height: 100%;
        overflow: auto;
        // TODO: Theme
        background: #18181b url('/assets/img/background_marketplace.png')
          no-repeat top right;
      }

      ::ng-deep menu {
        margin: 0;
        padding: 0;
      }

      ::ng-deep button {
        background-color: transparent;
        background-image: none;
      }

      .marketplace-content {
        &-wrapper {
          display: flex;
          justify-content: space-between;
          overflow: auto;
          scroll-behavior: smooth;
          min-height: 100vh;

          @media (min-width: 640px) {
            padding-left: 34vw;
          }
          @media (min-width: 768px) {
            padding-left: 28vw;
          }
          @media (min-width: 1024px) {
            padding-left: 22vw;
          }
          @media (min-width: 1536px) {
            padding-left: 280px;
          }
        }

        &-inner {
          padding-top: 6rem;

          @media (min-width: 640px) {
            padding-top: 0.75rem;
          }
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
          gap: 4rem;
          list-style-type: none;
          padding: 1.5rem;

          @media (min-width: 768px) {
            padding: 2rem;
          }
          @media (min-width: 1280px) {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          @media (min-width: 1536px) {
            grid-template-columns: repeat(3, minmax(0, 1fr));
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
