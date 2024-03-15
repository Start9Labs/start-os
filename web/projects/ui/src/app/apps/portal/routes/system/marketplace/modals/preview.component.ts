import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import {
  AboutModule,
  AbstractMarketplaceService,
  AdditionalModule,
  DependenciesModule,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
  ReleaseNotesModule,
} from '@start9labs/marketplace'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { map } from 'rxjs'
import { Router } from '@angular/router'

@Component({
  selector: 'marketplace-preview',
  template: `
    <div class="outer-container">
      <ng-content select="[slot=close]" />
      <marketplace-package-hero [pkg]="pkg">
        <ng-content select="[slot=controls]" />
      </marketplace-package-hero>
      @if (url$ | async; as url) {
        <a
          [href]="url + '/marketplace/' + pkg.manifest.id"
          tuiButton
          appearance="tertiary-solid"
          iconRight="tuiIconExternalLink"
          target="_blank"
        >
          View more details
        </a>
      }
      <div class="inner-container">
        <marketplace-about [pkg]="pkg" />
        @if (!(pkg.manifest.dependencies | empty)) {
          <div class="background-border shadow-color-light box-shadow-lg">
            <div class="dependencies-container">
              <h2>Dependencies</h2>
              <div class="dependencies-list">
                @for (
                  dep of pkg.manifest.dependencies | keyvalue;
                  track $index
                ) {
                  <marketplace-dependencies
                    [dep]="dep"
                    [pkg]="pkg"
                    (click)="open(dep.key)"
                  />
                }
              </div>
            </div>
          </div>
        }
        <release-notes [pkg]="pkg" />
        <marketplace-additional class="additional-wrapper" [pkg]="pkg" />
      </div>
    </div>
  `,
  styles: [
    `
      :host {
        pointer-events: auto;
      }

      .outer-container {
        display: grid;
        justify-content: center;
        gap: 2rem;
        padding: 1.75rem;
      }

      .inner-container {
        display: grid;
        grid-template-columns: repeat(1, minmax(0, 1fr));
        column-gap: 2rem;
      }

      .dependencies {
        &-container {
          background-color: rgb(39 39 42);
          border-radius: 0.75rem;
          padding: 1.75rem;

          @media (min-width: 1024px) {
            grid-column: span 5 / span 5;
          }
          @media (min-width: 1280px) {
            grid-column: span 4 / span 4;
          }

          h2 {
            font-size: 1.125rem;
            line-height: 1.75rem;
            font-weight: 700;
            margin: 0.5rem 0;
            padding-bottom: 0.75rem;
            font-variant: all-small-caps;
          }
        }

        &-list {
          display: grid;
          grid-auto-rows: auto;
          gap: 0.75rem;
        }
      }

      .additional-wrapper {
        margin-top: 1.5rem;
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarketplacePackageHeroComponent,
    TuiButtonModule,
    DependenciesModule,
    ReleaseNotesModule,
    AdditionalModule,
    AboutModule,
    SharedPipesModule,
  ],
})
export class MarketplacePreviewComponent {
  private readonly router = inject(Router)

  @Input({ required: true })
  pkg!: MarketplacePkg

  readonly url$ = inject(AbstractMarketplaceService)
    .getSelectedHost$()
    .pipe(map(({ url }) => url))

  open(id: string) {
    this.router.navigate([], { queryParams: { id } })
  }
}
