import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import {
  AboutModule,
  AdditionalModule,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
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

@Component({
  selector: 'sideload-package',
  template: `
    <div class="package-container">
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
          max-width: 80%;
          margin: auto;
          padding: 2.5rem 4rem 2rem 4rem;
        }
      }

      .inner-container {
        display: flex;
        justify-content: flex-start;
        margin: -0.5rem 0 1.5rem -1px;
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

  @Input({ required: true })
  pkg!: MarketplacePkg

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
    // @TODO Matt return License or Instructions
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
