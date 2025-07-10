import { CommonModule, TitleCasePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toObservable, toSignal } from '@angular/core/rxjs-interop'
import { Router } from '@angular/router'
import { MarketplacePkg } from '@start9labs/marketplace'
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
import { firstValueFrom, switchMap } from 'rxjs'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { dryUpdate } from 'src/app/utils/dry-update'
import { getAllPackages, getManifest } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'

import { MarketplaceAlertsService } from '../services/alerts.service'

type KEYS = 'id' | 'version' | 'alerts' | 'flavor'

@Component({
  selector: 'marketplace-controls',
  template: `
    @if (sameFlavor() && localPkg(); as local) {
      @if (local.stateInfo.state === 'installed') {
        @switch ((local | toManifest).version | compareExver: pkg().version) {
          @case (1) {
            <button
              tuiButton
              type="button"
              appearance="warning"
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
        {{ 'View' | i18n }}
        {{ $any(local.stateInfo.state | titlecase) | i18n }}
      </button>
    } @else {
      <button
        tuiButton
        type="button"
        appearance="primary"
        (click)="tryInstall()"
      >
        {{ (sameFlavor() ? 'Install' : 'Switch') | i18n }}
      </button>
    }
  `,
  styles: `
    :host {
      display: flex;
      justify-content: flex-start;
      gap: 0.5rem;
      height: 4.5rem;
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
  private readonly marketplace = inject(MarketplaceService)
  private readonly api = inject(ApiService)

  readonly pkg = input.required<Pick<MarketplacePkg, KEYS>>()

  // only present if side loading
  readonly file = input<File>()

  readonly localPkg = toSignal(
    toObservable(this.pkg).pipe(
      switchMap(({ id }) => this.patch.watch$('packageData', id)),
    ),
  )

  readonly sameFlavor = computed(
    (pkg = this.localPkg()) =>
      !pkg ||
      this.exver.getFlavor(getManifest(pkg).version) === this.pkg().flavor,
  )

  async tryInstall() {
    const localPkg = this.localPkg()
    const currentUrl = this.file()
      ? null
      : await firstValueFrom(this.marketplace.currentRegistryUrl$)
    const originalUrl = localPkg?.registry || null

    if (!localPkg) {
      if (await this.alerts.alertInstall(this.pkg().alerts.install || '')) {
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

    const { id, version } = getManifest(localPkg)

    if (
      hasCurrentDeps(id, await getAllPackages(this.patch)) &&
      this.exver.compareExver(version, this.pkg().version) !== 0
    ) {
      this.dryInstall(currentUrl)
    } else {
      this.installOrUpload(currentUrl)
    }
  }

  async showService() {
    this.router.navigate(['/portal/services', this.pkg().id])
  }

  private async dryInstall(url: string | null) {
    const { id, version } = this.pkg()
    const packages = await getAllPackages(this.patch)
    const breakages = dryUpdate({ id, version }, packages, this.exver)

    if (
      isEmptyObject(breakages) ||
      (await this.alerts.alertBreakages(breakages))
    ) {
      this.installOrUpload(url)
    }
  }

  private async installOrUpload(url: string | null) {
    if (this.file()) {
      await this.upload()
      this.router.navigate(['/portal', 'services'])
    } else if (url) {
      await this.install(url)
    }
  }

  private async install(url: string) {
    const loader = this.loader.open('Beginning install').subscribe()
    const { id, version } = this.pkg()

    try {
      await this.marketplace.installPackage(id, version, url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async upload() {
    const file = this.file()
    if (!file) throw new Error('no file detected')

    const loader = this.loader.open('Starting upload').subscribe()

    try {
      const { upload } = await this.api.sideloadPackage()
      this.api.uploadPackage(upload, file).catch(console.error)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
