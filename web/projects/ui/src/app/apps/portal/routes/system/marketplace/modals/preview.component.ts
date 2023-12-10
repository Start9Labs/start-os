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
    <div class="grid gap-8 p-7 justify-center">
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
      <div class="grid grid-cols-1 gap-x-8">
        <marketplace-about [pkg]="pkg" />
        @if (!(pkg.manifest.dependencies | empty)) {
          <div
            class="rounded-xl bg-gradient-to-bl from-zinc-400/75 to-zinc-600 p-px shadow-lg shadow-zinc-400/10 mt-6"
          >
            <div class="lg:col-span-5 xl:col-span-4 bg-zinc-800 rounded-xl p-7">
              <h2 class="text-lg font-bold small-caps my-2 pb-3">
                Dependencies
              </h2>
              <div class="grid grid-row-auto gap-3">
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
        <marketplace-additional class="mt-6" [pkg]="pkg" />
      </div>
    </div>
  `,
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
