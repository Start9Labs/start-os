import { TuiButton } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { Router } from '@angular/router'
import { MarketplacePkgBase } from '@start9labs/marketplace'
import {
  Exver,
  ErrorService,
  isEmptyObject,
  LoadingService,
  sameUrl,
  ExverPipesModule,
  i18nPipe,
} from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/utils/has-deps'
import { getAllPackages, getManifest } from 'src/app/utils/get-package-data'
import { dryUpdate } from 'src/app/utils/dry-update'
import { MarketplaceAlertsService } from '../services/alerts.service'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'marketplace-controls',
  template: `
    @if (localPkg) {
      @if (
        localPkg.stateInfo.state === 'installed' && (localPkg | toManifest);
        as localManifest
      ) {
        @switch (localManifest.version | compareExver: pkg.version) {
          @case (1) {
            <button
              tuiButton
              type="button"
              appearance="secondary-destructive"
              (click)="tryInstall()"
            >
              {{ 'Downgrade' | i18n }}
            </button>
          }
          @case (-1) {
            <button
              tuiButton
              type="button"
              appearance="primary"
              (click)="tryInstall()"
            >
              {{ 'Update' | i18n }}
            </button>
          }
          @case (0) {
            <button
              tuiButton
              type="button"
              appearance="secondary-grayscale"
              (click)="tryInstall()"
            >
              {{ 'Reinstall' | i18n }}
            </button>
          }
        }
      }
      <button
        tuiButton
        type="button"
        appearance="secondary-grayscale"
        (click)="showService()"
      >
        {{ 'View Installed' | i18n }}
      </button>
    } @else {
      <button
        tuiButton
        type="button"
        [appearance]="localFlavor ? 'warning' : 'primary'"
        (click)="tryInstall()"
      >
        {{ localFlavor ? ('Switch' | i18n) : ('Install' | i18n) }}
      </button>
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    ExverPipesModule,
    TuiButton,
    ToManifestPipe,
    i18nPipe,
  ],
})
export class MarketplaceControlsComponent {
  private readonly alerts = inject(MarketplaceAlertsService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly exver = inject(Exver)
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly api = inject(ApiService)

  @Input({ required: true })
  pkg!: MarketplacePkgBase

  @Input()
  localPkg!: PackageDataEntry | null

  @Input()
  localFlavor!: boolean

  // only present if side loading
  @Input()
  file?: File

  async tryInstall() {
    const currentUrl = this.file
      ? null
      : await firstValueFrom(this.marketplaceService.getRegistryUrl$())
    const originalUrl = this.localPkg?.registry || ''

    if (!this.localPkg) {
      if (await this.alerts.alertInstall(this.pkg)) {
        this.installOrUpload(currentUrl)
      }
      return
    }

    if (
      currentUrl &&
      !sameUrl(currentUrl, originalUrl) &&
      !(await this.alerts.alertMarketplace(currentUrl, originalUrl))
    ) {
      return
    }

    const localManifest = getManifest(this.localPkg)

    if (
      hasCurrentDeps(localManifest.id, await getAllPackages(this.patch)) &&
      this.exver.compareExver(localManifest.version, this.pkg.version) !== 0
    ) {
      this.dryInstall(currentUrl)
    } else {
      this.installOrUpload(currentUrl)
    }
  }

  async showService() {
    this.router.navigate(['/portal/services', this.pkg.id])
  }

  private async dryInstall(url: string | null) {
    const breakages = dryUpdate(
      this.pkg,
      await getAllPackages(this.patch),
      this.exver,
    )

    if (
      isEmptyObject(breakages) ||
      (await this.alerts.alertBreakages(breakages))
    ) {
      this.installOrUpload(url)
    }
  }

  private async installOrUpload(url: string | null) {
    if (this.file) {
      await this.upload()
      this.router.navigate(['/portal', 'services'])
    } else if (url) {
      await this.install(url)
    }
  }

  private async install(url: string) {
    const loader = this.loader.open('Beginning install').subscribe()
    const { id, version } = this.pkg

    try {
      await this.marketplaceService.installPackage(id, version, url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async upload() {
    const loader = this.loader.open('Starting upload').subscribe()

    try {
      const { upload } = await this.api.sideloadPackage()
      this.api.uploadPackage(upload, this.file!).catch(console.error)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
