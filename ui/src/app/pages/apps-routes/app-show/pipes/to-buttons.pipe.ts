import { Inject, Pipe, PipeTransform } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { DOCUMENT } from '@angular/common'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { MarkdownPage } from 'src/app/modals/markdown/markdown.page'
import { ModalService } from 'src/app/services/modal.service'

export interface Button {
  title: string
  description: string
  icon: string
  color: string
  action: Function
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
        color: 'danger',
      },
      // config
      {
        action: async () =>
          this.modalService.presentModalConfig({ pkgId: pkg.manifest.id }),
        title: 'Config',
        description: `Customize ${pkgTitle}`,
        icon: 'construct-outline',
        color: 'danger',
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
        color: 'danger',
      },
      // actions
      {
        action: () =>
          this.navCtrl.navigateForward(['actions'], { relativeTo: this.route }),
        title: 'Actions',
        description: `Uninstall and other commands specific to ${pkgTitle}`,
        icon: 'flash-outline',
        color: 'danger',
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
        color: 'danger',
      },
      // metrics
      // {
      //   action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
      //   title: 'Monitor',
      //   description: 'View system usage',
      //   icon: 'pulse-outline',
      //   color: 'danger',
      // },
      // logs
      {
        action: () =>
          this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        title: 'Logs',
        description: 'Raw, unfiltered service logs',
        icon: 'receipt-outline',
        color: 'danger',
      },
      {
        action: () => this.donate(pkg),
        title: 'Donate',
        description: `Support ${pkgTitle}`,
        icon: 'logo-bitcoin',
        color: 'danger',
      },
    ]
  }

  private async presentModalInstructions(pkg: PackageDataEntry) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Instructions',
        contentUrl: pkg['static-files']['instructions'],
      },
      component: MarkdownPage,
    })

    await modal.present()
  }

  private async donate({ manifest }: PackageDataEntry): Promise<void> {
    const url = manifest['donation-url']
    if (url) {
      this.document.defaultView.open(url, '_blank', 'noreferrer')
    } else {
      const alert = await this.alertCtrl.create({
        header: 'Not Accepting Donations',
        message: `The developers of ${manifest.title} have not provided a donation URL. Please contact them directly if you insist on giving them money.`,
      })
      await alert.present()
    }
  }
}
