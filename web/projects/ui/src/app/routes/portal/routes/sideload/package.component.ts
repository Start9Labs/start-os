import { CommonModule } from '@angular/common'
import { Component, inject, input } from '@angular/core'
import {
  AboutModule,
  AdditionalModule,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
} from '@start9labs/marketplace'
import { DialogService, MARKDOWN, SharedPipesModule } from '@start9labs/shared'
import { of } from 'rxjs'
import { MarketplaceControlsComponent } from '../marketplace/components/controls.component'
import { MarketplacePkgSideload } from './sideload.utils'

@Component({
  selector: 'sideload-package',
  template: `
    <div class="outer-container">
      <ng-content />
      <marketplace-package-hero [pkg]="pkg()">
        <marketplace-controls [pkg]="pkg()" [file]="file()" />
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
  styles: `
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
          margin-top: 0;
        }
      }
    }
  `,
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
  private readonly dialog = inject(DialogService)

  readonly pkg = input.required<MarketplacePkgSideload>()
  readonly file = input.required<File>()

  onStatic(type: 'license' | 'instructions') {
    const label = type === 'license' ? 'License' : 'Instructions'
    const key = type === 'license' ? 'fullLicense' : 'instructions'
    const content = of(this.pkg()[key])

    this.dialog
      .openComponent(MARKDOWN, { label, size: 'l', data: { content } })
      .subscribe()
  }
}
