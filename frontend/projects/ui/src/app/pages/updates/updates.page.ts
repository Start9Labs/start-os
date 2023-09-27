import { Component, Inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  AbstractMarketplaceService,
  Marketplace,
  MarketplaceManifest,
  MarketplacePkg,
  StoreIdentity,
} from '@start9labs/marketplace'
import { Emver, isEmptyObject } from '@start9labs/shared'
import { Pipe, PipeTransform } from '@angular/core'
import { combineLatest, Observable } from 'rxjs'
import { AlertController, NavController } from '@ionic/angular'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages } from 'src/app/util/get-package-data'
import { dryUpdate } from 'src/app/util/dry-update'

interface UpdatesData {
  hosts: StoreIdentity[]
  marketplace: Marketplace
  localPkgs: Record<string, PackageDataEntry>
  errors: string[]
}

@Component({
  selector: 'updates',
  templateUrl: 'updates.page.html',
  styleUrls: ['updates.page.scss'],
})
export class UpdatesPage {
  readonly data$: Observable<UpdatesData> = combineLatest({
    hosts: this.marketplaceService.getKnownHosts$(true),
    marketplace: this.marketplaceService.getMarketplace$(),
    localPkgs: this.patch.watch$('package-data'),
    errors: this.marketplaceService.getRequestErrors$(),
  })

  constructor(
    @Inject(AbstractMarketplaceService)
    readonly marketplaceService: MarketplaceService,
    private readonly patch: PatchDB<DataModel>,
    private readonly navCtrl: NavController,
    private readonly alertCtrl: AlertController,
    private readonly emver: Emver,
  ) {}

  viewInMarketplace(event: Event, url: string, id: string) {
    event.stopPropagation()

    this.navCtrl.navigateForward([`marketplace/${id}`], {
      queryParams: { url },
    })
  }

  async tryUpdate(
    manifest: MarketplaceManifest,
    url: string,
    local: PackageDataEntry,
    e: Event,
  ): Promise<void> {
    e.stopPropagation()

    const { id, version } = manifest

    delete this.marketplaceService.updateErrors[id]
    this.marketplaceService.updateQueue[id] = true

    if (hasCurrentDeps(local)) {
      this.dryInstall(manifest, url)
    } else {
      this.install(id, version, url)
    }
  }

  private async dryInstall(manifest: MarketplaceManifest, url: string) {
    const { id, version, title } = manifest

    const breakages = dryUpdate(
      manifest,
      await getAllPackages(this.patch),
      this.emver,
    )

    if (isEmptyObject(breakages)) {
      this.install(id, version, url)
    } else {
      const proceed = await this.presentAlertBreakages(title, breakages)
      if (proceed) {
        this.install(id, version, url)
      } else {
        delete this.marketplaceService.updateQueue[id]
      }
    }
  }

  private async presentAlertBreakages(
    title: string,
    breakages: string[],
  ): Promise<boolean> {
    let message: string = `As a result of updating ${title}, the following services will no longer work properly and may crash:<ul>`
    const bullets = breakages.map(depTitle => {
      return `<li><b>${depTitle}</b></li>`
    })
    message = `${message}${bullets.join('')}</ul>`

    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => {
              resolve(false)
            },
          },
          {
            text: 'Continue',
            handler: () => {
              resolve(true)
            },
            cssClass: 'enter-click',
          },
        ],
        cssClass: 'alert-warning-message',
      })

      await alert.present()
    })
  }

  private async install(id: string, version: string, url: string) {
    try {
      await this.marketplaceService.installPackage(id, version, url)
      delete this.marketplaceService.updateQueue[id]
    } catch (e: any) {
      delete this.marketplaceService.updateQueue[id]
      this.marketplaceService.updateErrors[id] = e.message
    }
  }
}

@Pipe({
  name: 'filterUpdates',
})
export class FilterUpdatesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(
    pkgs: MarketplacePkg[],
    local: Record<string, PackageDataEntry | undefined>,
  ): MarketplacePkg[] {
    return pkgs.filter(
      ({ manifest }) =>
        this.emver.compare(
          manifest.version,
          local[manifest.id]?.installed?.manifest.version || '',
        ) === 1,
    )
  }
}
