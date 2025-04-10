import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import {
  AboutModule,
  AdditionalModule,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkgBase,
} from '@start9labs/marketplace'
import {
  ErrorService,
  Exver,
  LoadingService,
  SharedPipesModule,
} from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MarketplaceControlsComponent } from '../marketplace/components/controls.component'
import { filter, first, map } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { MarketplacePkgSideload } from './sideload.utils'

@Component({
  selector: 'sideload-package',
  template: `
    <div class="outer-container">
      <ng-content />
      <marketplace-package-hero [pkg]="pkg">
        <marketplace-controls
          slot="controls"
          class="controls-wrapper"
          [pkg]="pkg"
          [localPkg]="local$ | async"
          [localFlavor]="!!(flavor$ | async)"
        />
      </marketplace-package-hero>
      <div class="package-details">
        <div class="package-details-main">
          <marketplace-about [pkg]="pkg" />
          @if (!(pkg.dependencyMetadata | empty)) {
            <marketplace-dependencies [pkg]="pkg" />
          }
        </div>
        <div class="package-details-additional">
          <marketplace-additional [pkg]="pkg" (static)="onStatic($event)" />
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
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly exver = inject(Exver)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  // @Input({ required: true })
  // pkg!: MarketplacePkgSideload

  // @Alex why do I need to initialize pkg below? I would prefer to do the above, but it's not working
  @Input({ required: true })
  pkg: MarketplacePkgSideload = {} as MarketplacePkgSideload

  @Input({ required: true })
  file!: File

  readonly local$ = this.patch.watch$('packageData', this.pkg.id).pipe(
    filter(Boolean),
    map(pkg =>
      this.exver.getFlavor(getManifest(pkg).version) === this.pkg.flavor
        ? pkg
        : null,
    ),
    first(),
  )

  readonly flavor$ = this.local$.pipe(map(pkg => !pkg))

  onStatic(type: 'License' | 'Instructions') {
    // @TODO Aiden return License or Instructions
  }

  async upload() {
    const loader = this.loader.open('Starting upload').subscribe()

    try {
      const { upload } = await this.api.sideloadPackage()
      this.api.uploadPackage(upload, this.file).catch(console.error)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
