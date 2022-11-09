import { Inject, Pipe, PipeTransform } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { DOCUMENT } from '@angular/common'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { MarkdownComponent } from '@start9labs/shared'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ModalService } from 'src/app/services/modal.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { from, map, Observable } from 'rxjs'
import { PatchDB } from 'patch-db-client'

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
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly alertCtrl: AlertController,
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly modalService: ModalService,
    private readonly apiService: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  transform(pkg: PackageDataEntry): Button[] {
    const pkgTitle = pkg.manifest.title

    return [
      // instructions
      {
        action: () => this.presentModalInstructions(pkg),
        title: 'Instructions',
        description: `Understand how to use ${pkgTitle}`,
        icon: 'list-outline',
        highlighted$: this.patch
          .watch$('ui', 'ack-instructions', pkg.manifest.id)
          .pipe(map(seen => !seen)),
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
      this.viewInMarketplaceButton(pkg),
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
    this.apiService
      .setDbValue<boolean>(['ack-instructions', pkg.manifest.id], true)
      .catch(e => console.error('Failed to mark instructions as seen', e))

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

  private viewInMarketplaceButton(pkg: PackageDataEntry): Button {
    const url = pkg.installed?.['marketplace-url']
    const queryParams = url ? { url } : {}

    let button: Button = {
      title: 'Marketplace Listing',
      icon: 'storefront-outline',
      action: () =>
        this.navCtrl.navigateForward([`marketplace/${pkg.manifest.id}`], {
          queryParams,
        }),
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
