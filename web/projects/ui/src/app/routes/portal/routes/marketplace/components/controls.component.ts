import { CommonModule, TitleCasePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { Router } from '@angular/router'
import { MarketplacePkgBase } from '@start9labs/marketplace'
import {
  ErrorService,
  Exver,
  ExverPipesModule,
  i18nPipe,
  isEmptyObject,
  LoadingService,
  sameUrl,
} from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { dryUpdate } from 'src/app/utils/dry-update'
import { getAllPackages, getManifest } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'

import { MarketplacePreviewComponent } from '../modals/preview.component'
import { MarketplaceAlertsService } from '../services/alerts.service'

@Component({
  selector: 'marketplace-controls',
  template: `
    @if (localPkg) {
      @if (localPkg | toManifest; as localManifest) {
        @switch (localManifest.version | compareExver: version() || '') {
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
        {{
          ('View' | i18n) +
            ' ' +
            ($any(localPkg.stateInfo.state | titlecase) | i18n)
        }}
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
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    ExverPipesModule,
    TuiButton,
    ToManifestPipe,
    i18nPipe,
    TitleCasePipe,
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
  private readonly preview = inject(MarketplacePreviewComponent)

  protected readonly version = toSignal(this.preview.version$)

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
      : await firstValueFrom(this.marketplaceService.currentRegistryUrl$)
    const originalUrl = this.localPkg?.registry || null

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
    const version = this.version() || ''

    if (
      hasCurrentDeps(localManifest.id, await getAllPackages(this.patch)) &&
      this.exver.compareExver(localManifest.version, version) !== 0
    ) {
      this.dryInstall(currentUrl)
    } else {
      this.installOrUpload(currentUrl)
    }
  }

  async showService() {
    this.router.navigate(['/portal/services', this.preview.pkgId])
  }

  private async dryInstall(url: string | null) {
    const id = this.preview.pkgId
    const version = this.version() || ''
    const breakages = dryUpdate(
      { id, version },
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
    const version = this.version() || ''
    const id = this.preview.pkgId

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
