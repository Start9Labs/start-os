import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router } from '@angular/router'
import {
  AbstractCategoryService,
  FilterPackagesPipe,
  FilterPackagesPipeModule,
} from '@start9labs/marketplace'
import { i18nPipe } from '@start9labs/shared'
import { TuiScrollbar } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { tap, withLatestFrom } from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { MarketplaceMenuComponent } from './components/menu.component'
import { MarketplaceNotificationComponent } from './components/notification.component'
import { MarketplaceTileComponent } from './components/tile.component'

@Component({
  standalone: true,
  template: `
    <ng-container *title>{{ 'Marketplace' | i18n }}</ng-container>
    <marketplace-menu />
    <tui-scrollbar>
      <div class="marketplace-content-wrapper">
        <div class="marketplace-content-inner">
          <marketplace-notification [url]="(url$ | async) || ''" />
          <div class="title-wrapper">
            <h1>
              {{ category$ | async | titlecase }}
            </h1>
          </div>
          @if (registry$ | async; as registry) {
            <section class="marketplace-content-list">
              @for (
                pkg of registry.packages
                  | filterPackages: (query$ | async) : (category$ | async);
                track $index
              ) {
                <marketplace-tile
                  [pkg]="pkg"
                  [style.--animation-order]="$index"
                  class="tile-wrapper"
                />
              }
            </section>
          } @else {
            <h1 class="loading-text">
              {{ 'Loading' | i18n }}
              <span class="loading-dots"></span>
            </h1>
          }
        </div>
      </div>
    </tui-scrollbar>
  `,
  host: { class: 'g-page' },
  styles: [
    `
      :host {
        display: flex;
        flex-direction: column;
        overflow: hidden;
        padding: 0;
        background: rgb(55 58 63 / 90%)
          url('/assets/img/background_marketplace.png') no-repeat top right;
        background-size: cover;
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
        font-weight: normal;
      }

      :host-context(tui-root._mobile) {
        padding: 0;
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
    TuiScrollbar,
    FilterPackagesPipeModule,
    TitleDirective,
    i18nPipe,
  ],
})
export default class MarketplaceComponent {
  private readonly categoryService = inject(AbstractCategoryService)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly router = inject(Router)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly route = inject(ActivatedRoute)
    .queryParamMap.pipe(
      takeUntilDestroyed(),
      withLatestFrom(this.patch.watch$('ui', 'marketplace', 'selectedUrl')),
      tap(([params, selectedUrl]) => {
        const registry = params.get('registry')
        if (!registry) {
          this.router.navigate([], {
            queryParams: { registry: selectedUrl },
            queryParamsHandling: 'merge',
          })
        } else {
          this.marketplaceService.setRegistryUrl(registry)
        }
      }),
    )
    .subscribe()

  readonly url$ = this.marketplaceService.getRegistryUrl$()
  readonly category$ = this.categoryService.getCategory$()
  readonly query$ = this.categoryService.getQuery$()
  readonly registry$ = this.marketplaceService.getRegistry$()
}
