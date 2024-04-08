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
  Emver,
  ErrorService,
  LoadingService,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiAlertService } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { getManifest } from 'src/app/util/get-package-data'

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
            [routerLink]="'/portal/service/' + package.manifest.id"
          >
            View installed
          </a>
          <button *ngIf="button" tuiButton (click)="upload()">
            {{ button }}
          </button>
        </div>
      </marketplace-package-hero>
      <marketplace-about [pkg]="package" />
      @if (!(package.manifest.dependencies | empty)) {
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
    TuiButtonModule,
    TuiLetModule,
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
  private readonly emver = inject(Emver)

  readonly button$ = combineLatest([
    inject(ClientStorageService).showDevTools$,
    inject(PatchDB<DataModel>)
      .watch$('packageData')
      .pipe(
        map(local =>
          local[this.package.manifest.id]
            ? this.emver.compare(
                getManifest(local[this.package.manifest.id]).version,
                this.package.manifest.version,
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
    const { manifest, icon } = this.package
    const { size } = this.file

    try {
      const pkg = await this.api.sideloadPackage({ manifest, icon, size })

      await this.api.uploadPackage(pkg, this.file)
      await this.router.navigate(['/portal/service', manifest.id])

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
