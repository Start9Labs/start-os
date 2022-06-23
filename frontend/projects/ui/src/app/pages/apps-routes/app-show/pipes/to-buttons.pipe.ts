import { Inject, Pipe, PipeTransform } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { DOCUMENT } from '@angular/common'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { MarkdownComponent, removeTrailingSlash } from '@start9labs/shared'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ModalService } from 'src/app/services/modal.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { from } from 'rxjs'
import { Marketplace } from '@start9labs/marketplace'
export interface Button {
  title: string
  description: string
  icon: string
  action: Function
  disabled?: boolean
}

@Pipe({
  name: 'toButtons',
})
export class ToButtonsPipe implements PipeTransform {
  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly alertCtrl: AlertController,
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly modalService: ModalService,
    private readonly apiService: ApiService,
  ) {}

  transform(
    pkg: PackageDataEntry,
    currentMarketplace: Marketplace | null,
  ): Button[] {
    const pkgTitle = pkg.manifest.title

    return [
      // instructions
      {
        action: () => this.presentModalInstructions(pkg),
        title: 'Instructions',
        description: `Understand how to use ${pkgTitle}`,
        icon: 'list-outline',
      },
      // config
      {
        action: async () =>
          this.modalService.presentModalConfig({ pkgId: pkg.manifest.id }),
        title: 'Config',
        description: `Customize ${pkgTitle}`,
        icon: 'construct-outline',
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
        description: `Uninstall and other commands specific to ${pkgTitle}`,
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
      this.viewInMarketplaceButton(pkg, currentMarketplace),
      // donate
      {
        action: () => this.donate(pkg),
        title: 'Donate',
        description: `Support ${pkgTitle}`,
        icon: 'logo-bitcoin',
      },
    ]
  }

  private async presentModalInstructions(pkg: PackageDataEntry) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Instructions',
        content: from(
          this.apiService.getStatic(pkg['static-files']['instructions']),
        ),
      },
      component: MarkdownComponent,
    })

    await modal.present()
  }

  private viewInMarketplaceButton(
    pkg: PackageDataEntry,
    currentMarketplace: Marketplace | null,
  ): Button {
    const pkgMarketplace = pkg.installed?.['marketplace-url']
    const pkgM = pkgMarketplace
      ? removeTrailingSlash(pkgMarketplace)
      : pkgMarketplace
    const currentM = currentMarketplace
      ? removeTrailingSlash(currentMarketplace.url)
      : currentMarketplace
    if (!pkgMarketplace) {
      return {
        disabled: true,
        action: () => {},
        title: 'Marketplace',
        description:
          'This package has been sideloaded and is not available in the Start9 Marketplace',
        icon: 'storefront-outline',
      }
    } else if (pkgM && currentM && pkgM !== currentM) {
      return {
        action: async () => {
          const alert = await this.alertCtrl.create({
            header: 'Marketplace Conflict',
            message: `This service was installed from:
            <br><br>
            <span class="courier-new color-success-shade">${pkgM}</span>
            <br><br>but you are currently connected to:<br><br>
            <span class="courier-new color-primary-shade">${currentM}</span>
            <br><br>
            Please change marketplaces to see the relevant details.`,
            buttons: [
              {
                text: 'Cancel',
                role: 'cancel',
              },
              {
                text: 'Change Marketplace',
                handler: () =>
                  this.navCtrl.navigateForward(['embassy/marketplaces']),
                cssClass: 'enter-click',
              },
            ],
          })
          await alert.present()
        },
        title: 'Marketplace',
        description: 'Service was installed from a different marketplace',
        icon: 'storefront-outline',
      }
    } else {
      // package marketplace and current marketplace are the same
      return {
        action: () =>
          this.navCtrl.navigateForward([`marketplace/${pkg.manifest.id}`]),
        title: 'Marketplace',
        description: 'View service in marketplace',
        icon: 'storefront-outline',
      }
    }
  }

  private async donate({ manifest }: PackageDataEntry): Promise<void> {
    const url = manifest['donation-url']
    if (url) {
      this.document.defaultView?.open(url, '_blank', 'noreferrer')
    } else {
      const alert = await this.alertCtrl.create({
        header: 'Not Accepting Donations',
        message: `The developers of ${manifest.title} have not provided a donation URL. Please contact them directly if you insist on giving them money.`,
      })
      await alert.present()
    }
  }
}
