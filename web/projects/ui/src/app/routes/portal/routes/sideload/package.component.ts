import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
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
          [file]="file"
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
  private readonly exver = inject(Exver)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly dialog = inject(DialogService)

  // @Input({ required: true })
  // pkg!: MarketplacePkgSideload

  // @TODO Alex why do I need to initialize pkg below? I would prefer to do the above, but it's not working
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

  // @TODO Alex, struggling to get this working. I don't understand how to use this markdown component, only one other example, and it's very different.
  onStatic(type: 'license' | 'instructions') {
    this.dialog
      .openComponent(MARKDOWN, {
        label: type === 'license' ? 'License' : 'Instructions',
        size: 'l',
        data: {
          content:
            this.pkg[type === 'license' ? 'fullLicense' : 'instructions'],
        },
      })
      .subscribe()
  }
}
