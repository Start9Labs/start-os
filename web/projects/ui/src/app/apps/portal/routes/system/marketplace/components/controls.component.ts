import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { Router } from '@angular/router'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  Emver,
  ErrorService,
  isEmptyObject,
  LoadingService,
  sameUrl,
  EmverPipesModule,
} from '@start9labs/shared'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages, getManifest } from 'src/app/util/get-package-data'
import { dryUpdate } from 'src/app/util/dry-update'
import { MarketplaceAlertsService } from '../services/alerts.service'
import { ToManifestPipe } from 'src/app/apps/portal/pipes/to-manifest'

@Component({
  selector: 'marketplace-controls',
  template: `
    @if (localPkg) {
      <button
        tuiButton
        type="button"
        appearance="primary"
        (click)="showService()"
      >
        View Installed
      </button>
      @if (
        localPkg.stateInfo.state === 'installed' && (localPkg | toManifest);
        as localManifest
      ) {
        @switch (localManifest.version | compareEmver: pkg.manifest.version) {
          @case (1) {
            <button
              tuiButton
              type="button"
              appearance="secondary-solid"
              (click)="tryInstall()"
            >
              Downgrade
            </button>
          }
          @case (-1) {
            <button
              tuiButton
              type="button"
              appearance="warning-solid"
              (click)="tryInstall()"
            >
              Update
            </button>
          }
          @case (0) {
            @if (showDevTools$ | async) {
              <button
                tuiButton
                type="button"
                appearance="tertiary-solid"
                (click)="tryInstall()"
              >
                Reinstall
              </button>
            }
          }
        }
      }
    } @else {
      <button
        tuiButton
        type="button"
        appearance="primary"
        (click)="tryInstall()"
      >
        Install
      </button>
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, EmverPipesModule, TuiButtonModule, ToManifestPipe],
})
export class MarketplaceControlsComponent {
  private readonly alerts = inject(MarketplaceAlertsService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly emver = inject(Emver)
  private readonly router = inject(Router)
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  @Input()
  url?: string

  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input()
  localPkg!: PackageDataEntry | null

  readonly showDevTools$ = inject(ClientStorageService).showDevTools$

  async tryInstall() {
    const current = await firstValueFrom(this.marketplace.getSelectedHost$())
    const url = this.url || current.url
    const originalUrl = this.localPkg?.marketplaceUrl || ''

    if (!this.localPkg) {
      if (await this.alerts.alertInstall(this.pkg)) this.install(url)

      return
    }

    if (
      !sameUrl(url, originalUrl) &&
      !(await this.alerts.alertMarketplace(url, originalUrl))
    ) {
      return
    }

    const localManifest = getManifest(this.localPkg)

    if (
      hasCurrentDeps(localManifest.id, await getAllPackages(this.patch)) &&
      this.emver.compare(localManifest.version, this.pkg.manifest.version) !== 0
    ) {
      this.dryInstall(url)
    } else {
      this.install(url)
    }
  }

  async showService() {
    this.router.navigate(['/portal/service', this.pkg.manifest.id])
  }

  private async dryInstall(url: string) {
    const breakages = dryUpdate(
      this.pkg.manifest,
      await getAllPackages(this.patch),
      this.emver,
    )

    if (
      isEmptyObject(breakages) ||
      (await this.alerts.alertBreakages(breakages))
    ) {
      this.install(url)
    }
  }

  private async install(url: string) {
    const loader = this.loader.open('Beginning Install...').subscribe()
    const { id, version } = this.pkg.manifest

    try {
      await this.marketplace.installPackage(id, version, url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
