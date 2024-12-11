import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import { Router, RouterLink } from '@angular/router'
import {
  AboutModule,
  AdditionalModule,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
} from '@start9labs/marketplace'
import {
  ErrorService,
  Exver,
  LoadingService,
  SharedPipesModule,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiLet } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiProgressBar } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, firstValueFrom, map } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/service/pipes/install-progress.pipe'
import { SideloadService } from './sideload.service'

@Component({
  selector: 'sideload-package',
  template: `
    <div class="outer-container">
      <ng-content />
      @if (progress$ | async; as progress) {
        @for (phase of progress.phases; track $index) {
          <p>
            {{ phase.name }}
            @if (phase.progress | installingProgress; as progress) {
              : {{ progress }}%
            }
          </p>
          <progress
            tuiProgressBar
            size="xs"
            [style.color]="
              phase.progress === true
                ? 'var(--tui-text-positive)'
                : 'var(--tui-text-action)'
            "
            [attr.value]="(phase.progress | installingProgress) / 100 || null"
          ></progress>
        }
      } @else {
        <marketplace-package-hero
          *tuiLet="button$ | async as button"
          [pkg]="package"
        >
          <div class="inner-container">
            @if (button !== null && button !== 'Install') {
              <a
                tuiButton
                appearance="tertiary-solid"
                [routerLink]="'/portal/service/' + package.id"
              >
                View installed
              </a>
            }
            @if (button) {
              <button tuiButton (click)="upload()">{{ button }}</button>
            }
          </div>
        </marketplace-package-hero>
        <!-- @TODO Matt do we want this here? How do we turn s9pk into MarketplacePkg? -->
        <!--      <marketplace-about [pkg]="package" />-->
        <!--      @if (!(package.dependencyMetadata | empty)) {-->
        <!--        <marketplace-dependencies [pkg]="package" (open)="open($event)" />-->
        <!--      }-->
        <!--      <marketplace-additional [pkg]="package" />-->
      }
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
    RouterLink,
    SharedPipesModule,
    AboutModule,
    AdditionalModule,
    TuiButton,
    TuiLet,
    MarketplacePackageHeroComponent,
    MarketplaceDependenciesComponent,
    InstallingProgressPipe,
    TuiProgressBar,
  ],
})
export class SideloadPackageComponent {
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)
  private readonly exver = inject(Exver)
  private readonly sideloadService = inject(SideloadService)

  readonly progress$ = this.sideloadService.progress$
  readonly button$ = combineLatest([
    inject(ClientStorageService).showDevTools$,
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData')
      .pipe(
        map(local =>
          local[this.package.id]
            ? this.exver.compareExver(
                getManifest(local[this.package.id]).version,
                this.package.version,
              )
            : null,
        ),
      ),
  ]).pipe(
    map(([devtools, version]) => {
      switch (version) {
        case null:
          return 'Install'
        case 1:
          return 'Update'
        case -1:
          return devtools ? 'Downgrade' : ''
        default:
          return ''
      }
    }),
  )

  @Input({ required: true })
  package!: T.Manifest & { icon: string }

  @Input({ required: true })
  file!: File

  async upload() {
    const loader = this.loader.open('Starting upload').subscribe()

    try {
      const { upload, progress } = await this.api.sideloadPackage()

      this.sideloadService.followProgress(progress)
      this.api.uploadPackage(upload, this.file).catch(console.error)
      await firstValueFrom(this.progress$.pipe(filter(Boolean)))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  open(id: string) {
    this.router.navigate(['/marketplace'], { queryParams: { id } })
  }
}
