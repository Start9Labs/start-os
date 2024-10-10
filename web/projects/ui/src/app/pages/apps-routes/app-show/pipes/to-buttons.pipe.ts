import { Pipe, PipeTransform } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { MarkdownComponent } from '@start9labs/shared'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { from, map, Observable, of } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { ActionService } from 'src/app/services/action.service'
import { needsConfig } from 'src/app/util/get-package-data'

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
    private readonly apiService: ApiService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly actionService: ActionService,
    private readonly alertCtrl: AlertController,
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
          pkg.actions['config']
            ? this.actionService.present(
                {
                  id: manifest.id,
                  title: manifest.title,
                  mainStatus: pkg.status.main,
                },
                {
                  id: 'config',
                  metadata: pkg.actions['config'],
                },
              )
            : this.alertCtrl
                .create({
                  header: 'No Config',
                  message: `No config options for ${manifest.title} v${manifest.version}`,
                  buttons: ['OK'],
                })
                .then(a => a.present()),
        title: 'Config',
        description: `Customize ${manifest.title}`,
        icon: 'options-outline',
        highlighted$: of(needsConfig(manifest.id, pkg.requestedActions)),
      },
      // properties
      {
        action: () =>
          pkg.actions['properties']
            ? this.actionService.execute(manifest.id, 'properties')
            : this.alertCtrl
                .create({
                  header: 'No Properties',
                  message: `No properties for ${manifest.title} v${manifest.version}`,
                  buttons: ['OK'],
                })
                .then(a => a.present()),
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
      .setDbValue<boolean>(['ackInstructions', pkg.stateInfo.manifest.id], true)
      .catch(e => console.error('Failed to mark instructions as seen', e))

    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Instructions',
        content: from(
          this.api.getStaticInstalled(
            pkg.stateInfo.manifest.id,
            'instructions.md',
          ),
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
