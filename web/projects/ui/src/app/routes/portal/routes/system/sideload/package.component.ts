import { TuiLet } from '@taiga-ui/cdk'
import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import { Router, RouterLink } from '@angular/router'
import {
  AboutModule,
  AdditionalModule,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  Exver,
  ErrorService,
  LoadingService,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiAlertService, TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'sideload-package',
  template: `
    <div class="outer-container">
      <ng-content />
      <marketplace-package-hero
        *tuiLet="button$ | async as button"
        [pkg]="package"
      >
        <div class="inner-container">
          <a
            *ngIf="button !== null && button !== 'Install'"
            tuiButton
            appearance="tertiary-solid"
            [routerLink]="'/portal/service/' + package.id"
          >
            View installed
          </a>
          <button *ngIf="button" tuiButton (click)="upload()">
            {{ button }}
          </button>
        </div>
      </marketplace-package-hero>
      <marketplace-about [pkg]="package" />
      @if (!(package.dependencyMetadata | empty)) {
        <marketplace-dependencies [pkg]="package" (open)="open($event)" />
      }
      <marketplace-additional [pkg]="package" />
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
  ],
})
export class SideloadPackageComponent {
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)
  private readonly alerts = inject(TuiAlertService)
  private readonly exver = inject(Exver)

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
  package!: MarketplacePkg

  @Input({ required: true })
  file!: File

  async upload() {
    const loader = this.loader.open('Uploading package').subscribe()

    try {
      const { upload } = await this.api.sideloadPackage()

      await this.api.uploadPackage(upload, this.file).catch(console.error)
      await this.router.navigate(['/portal/service', this.package.id])

      this.alerts
        .open('Package uploaded successfully', { status: 'success' })
        .subscribe()
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
