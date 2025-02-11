import { TuiButton } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { Router } from '@angular/router'
import { MarketplacePkg } from '@start9labs/marketplace'
import {
  Exver,
  ErrorService,
  isEmptyObject,
  LoadingService,
  sameUrl,
  ExverPipesModule,
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
              appearance="warning-solid"
              (click)="tryInstall()"
            >
              Downgrade
            </button>
          }
          @case (-1) {
            <button
              tuiButton
              type="button"
              appearance="primary"
              (click)="tryInstall()"
            >
              Update
            </button>
          }
          @case (0) {
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
      <button
        tuiButton
        type="button"
        appearance="outline-grayscale"
        (click)="showService()"
      >
        View Installed
      </button>
    } @else {
      <button
        tuiButton
        type="button"
        [appearance]="localFlavor ? 'warning' : 'primary'"
        (click)="tryInstall()"
      >
        {{ localFlavor ? 'Switch' : 'Install' }}
      </button>
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, ExverPipesModule, TuiButton, ToManifestPipe],
})
export class MarketplaceControlsComponent {
  private readonly alerts = inject(MarketplaceAlertsService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly exver = inject(Exver)
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(MarketplaceService)

  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input()
  localPkg!: PackageDataEntry | null

  @Input()
  localFlavor!: boolean

  async tryInstall() {
    const currentUrl = await firstValueFrom(
      this.marketplaceService.getRegistryUrl$(),
    )
    const originalUrl = this.localPkg?.registry || ''
    if (!this.localPkg) {
      if (await this.alerts.alertInstall(this.pkg)) this.install(currentUrl)

      return
    }

    if (
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
      this.install(currentUrl)
    }
  }

  async showService() {
    this.router.navigate(['/portal/services', this.pkg.id])
  }

  private async dryInstall(url: string) {
    const breakages = dryUpdate(
      this.pkg,
      await getAllPackages(this.patch),
      this.exver,
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
    const { id, version } = this.pkg

    try {
      await this.marketplaceService.installPackage(id, version, url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
