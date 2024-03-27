import { CommonModule } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import { Router, RouterLink } from '@angular/router'
import {
  AboutModule,
  AdditionalModule,
  DependenciesModule,
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
    <div class="grid gap-8 mb-16 p-4 lg:px-16 lg:pb-8 pt-14 justify-center">
      <ng-content />
      <marketplace-package-hero
        *tuiLet="button$ | async as button"
        [pkg]="package"
      >
        <div class="flex justify-start">
          <a
            *ngIf="button !== null && button !== 'Install'"
            tuiButton
            appearance="secondary"
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
      <div
        *ngIf="!(package.manifest.dependencies | empty)"
        class="rounded-xl bg-gradient-to-bl from-zinc-400/75 to-zinc-600 p-px shadow-lg shadow-zinc-400/10"
      >
        <div class="lg:col-span-5 xl:col-span-4 bg-zinc-800 rounded-xl p-7">
          <h2 class="text-lg font-bold small-caps my-2 pb-3">Dependencies</h2>
          <div class="grid grid-row-auto gap-3">
            <div *ngFor="let dep of package.manifest.dependencies | keyvalue">
              <marketplace-dependencies [dep]="dep" [pkg]="package" />
            </div>
          </div>
        </div>
      </div>
      <marketplace-additional [pkg]="package" />
    </div>
  `,
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
    DependenciesModule,
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
}
