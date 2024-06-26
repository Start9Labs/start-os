import { Inject, Pipe, PipeTransform } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController, NavController } from '@ionic/angular'
import { MarkdownComponent } from '@start9labs/shared'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ModalService } from 'src/app/services/modal.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { map, Observable } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { AbstractMarketplaceService } from '@start9labs/marketplace'

export interface Button {
  title: string
  description: string
  icon: string
  action: Function
  highlighted$?: Observable<boolean>
  disabled?: boolean
}

@Pipe({
  name: 'toButtons',
})
export class ToButtonsPipe implements PipeTransform {
  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly modalService: ModalService,
    private readonly apiService: ApiService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  transform(pkg: PackageDataEntry<InstalledState>): Button[] {
    const manifest = pkg.stateInfo.manifest

    return [
      // instructions
      {
        action: () => this.presentModalInstructions(pkg),
        title: 'Instructions',
        description: `Understand how to use ${manifest.title}`,
        icon: 'list-outline',
        highlighted$: this.patch
          .watch$('ui', 'ackInstructions', manifest.id)
          .pipe(map(seen => !seen)),
      },
      // config
      {
        action: async () =>
          this.modalService.presentModalConfig({ pkgId: manifest.id }),
        title: 'Config',
        description: `Customize ${manifest.title}`,
        icon: 'options-outline',
      },
      // properties
      {
        action: () =>
          this.navCtrl.navigateForward(['properties'], {
            relativeTo: this.route,
          }),
        title: 'Properties',
        description:
          'Runtime information, credentials, and other values of interest',
        icon: 'briefcase-outline',
      },
      // actions
      {
        action: () =>
          this.navCtrl.navigateForward(['actions'], { relativeTo: this.route }),
        title: 'Actions',
        description: `Uninstall and other commands specific to ${manifest.title}`,
        icon: 'flash-outline',
      },
      // interfaces
      {
        action: () =>
          this.navCtrl.navigateForward(['interfaces'], {
            relativeTo: this.route,
          }),
        title: 'Interfaces',
        description: 'User and machine access points',
        icon: 'desktop-outline',
      },
      // logs
      {
        action: () =>
          this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        title: 'Logs',
        description: 'Raw, unfiltered service logs',
        icon: 'receipt-outline',
      },
      // view in marketplace
      this.viewInMarketplaceButton(pkg),
    ]
  }

  private async presentModalInstructions(
    pkg: PackageDataEntry<InstalledState>,
  ) {
    this.apiService
      .setDbValue<boolean>(
        ['ack-instructions', pkg.stateInfo.manifest.id],
        true,
      )
      .catch(e => console.error('Failed to mark instructions as seen', e))

    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Instructions',
        content: this.marketplaceService.fetchStatic$(
          pkg.stateInfo.manifest.id,
          'instructions',
          pkg.stateInfo.manifest.version,
          pkg.registry,
        ),
      },
      component: MarkdownComponent,
    })

    await modal.present()
  }

  private viewInMarketplaceButton(
    pkg: PackageDataEntry<InstalledState>,
  ): Button {
    const url = pkg.registry
    const queryParams = url ? { url } : {}

    let button: Button = {
      title: 'Marketplace Listing',
      icon: 'storefront-outline',
      action: () =>
        this.navCtrl.navigateForward(
          [`marketplace/${pkg.stateInfo.manifest.id}`],
          {
            queryParams,
          },
        ),
      disabled: false,
      description: 'View service in the marketplace',
    }

    if (!url) {
      button.disabled = true
      button.description = 'This package was not installed from the marketplace'
      button.action = () => {}
    }

    return button
  }
}
