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
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages } from 'src/app/util/get-package-data'
import { dryUpdate } from 'src/app/util/dry-update'
import { SidebarService } from 'src/app/services/sidebar.service'
import { MarketplaceAlertsService } from '../services/alerts.service'

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
      @if (installed) {
        @switch (localVersion | compareEmver: pkg.manifest.version) {
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
  imports: [CommonModule, EmverPipesModule, TuiButtonModule],
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

  get installed(): boolean {
    return this.localPkg?.state === PackageState.Installed
  }

  get localVersion(): string {
    return this.localPkg?.manifest.version || ''
  }

  async tryInstall() {
    const current = await firstValueFrom(this.marketplace.getSelectedHost$())
    const url = this.url || current.url
    const originalUrl = this.localPkg?.installed?.['marketplace-url'] || ''

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

    if (
      hasCurrentDeps(this.localPkg) &&
      this.emver.compare(this.localVersion, this.pkg.manifest.version) !== 0
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
