import { CommonModule } from '@angular/common'
import { Component, inject, input } from '@angular/core'
import { toObservable } from '@angular/core/rxjs-interop'
import {
  AboutModule,
  AdditionalModule,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
} from '@start9labs/marketplace'
import {
  DialogService,
  Exver,
  MARKDOWN,
  SharedPipesModule,
} from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { filter, first, map, of, switchMap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { MarketplaceControlsComponent } from '../marketplace/components/controls.component'
import { MarketplacePkgSideload } from './sideload.utils'

@Component({
  selector: 'sideload-package',
  template: `
    <div class="outer-container">
      <ng-content />
      <marketplace-package-hero [pkg]="pkg()">
        <marketplace-controls
          slot="controls"
          class="controls-wrapper"
          [pkg]="pkg()"
          [localPkg]="local$ | async"
          [localFlavor]="!!(flavor$ | async)"
          [file]="file()"
        />
      </marketplace-package-hero>
      <div class="package-details">
        <div class="package-details-main">
          <marketplace-about [pkg]="pkg()" />
          @if (!(pkg().dependencyMetadata | empty)) {
            <marketplace-dependencies [pkg]="pkg()" />
          }
        </div>
        <div class="package-details-additional">
          <marketplace-additional [pkg]="pkg()" (static)="onStatic($event)" />
        </div>
      </div>
    </div>
  `,
  styles: [
    `
      .outer-container {
        display: grid;
        justify-content: center;
        width: 100%;

        @media (min-width: 1024px) {
          margin: auto;
          padding: 2.5rem 4rem 2rem 4rem;
        }
      }

      .package-details {
        -moz-column-gap: 2rem;
        column-gap: 2rem;

        &-main {
          grid-column: span 12 / span 12;
        }

        &-additional {
          grid-column: span 12 / span 12;
        }

        @media (min-width: 1536px) {
          grid-template-columns: repeat(12, minmax(0, 1fr));
          &-main {
            grid-column: span 8 / span 8;
          }
          &-additional {
            grid-column: span 4 / span 4;
            margin-top: 0px;
          }
        }
      }

      .controls-wrapper {
        display: flex;
        justify-content: flex-start;
        gap: 0.5rem;
        height: 4.5rem;
      }
    `,
  ],
  standalone: true,
  imports: [
    CommonModule,
    SharedPipesModule,
    AboutModule,
    AdditionalModule,
    MarketplacePackageHeroComponent,
    MarketplaceDependenciesComponent,
    MarketplaceControlsComponent,
  ],
})
export class SideloadPackageComponent {
  private readonly exver = inject(Exver)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly dialog = inject(DialogService)

  readonly pkg = input.required<MarketplacePkgSideload>()
  readonly file = input.required<File>()

  readonly local$ = toObservable(this.pkg).pipe(
    filter(Boolean),
    switchMap(({ id, flavor }) =>
      this.patch.watch$('packageData', id).pipe(
        filter(Boolean),
        map(pkg =>
          this.exver.getFlavor(getManifest(pkg).version) === flavor
            ? pkg
            : null,
        ),
      ),
    ),
    first(),
  )

  readonly flavor$ = this.local$.pipe(map(pkg => !pkg))

  onStatic(type: 'license' | 'instructions') {
    const label = type === 'license' ? 'License' : 'Instructions'
    const key = type === 'license' ? 'fullLicense' : 'instructions'

    this.dialog
      .openComponent(MARKDOWN, {
        label,
        size: 'l',
        data: { content: of(this.pkg()[key]) },
      })
      .subscribe()
  }
}
