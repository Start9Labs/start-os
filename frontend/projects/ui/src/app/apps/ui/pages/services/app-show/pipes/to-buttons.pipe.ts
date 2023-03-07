import { Pipe, PipeTransform } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { NavController } from '@ionic/angular'
import { MarkdownComponent } from '@start9labs/shared'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  AppConfigPage,
  PackageConfigData,
} from '../modals/app-config/app-config.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { from, map, Observable } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { TuiDialogService } from '@taiga-ui/core'

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
    private readonly dialogs: TuiDialogService,
    private readonly formDialog: FormDialogService,
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
        action: () =>
          this.formDialog.open<PackageConfigData>(AppConfigPage, {
            label: `${pkg.manifest.title} configuration`,
            data: { pkgId: pkg.manifest.id },
          }),
        title: 'Config',
        description: `Customize ${pkgTitle}`,
        icon: 'options-outline',
      },
      // credentials
      {
        action: () =>
          this.navCtrl.navigateForward(['credentials'], {
            relativeTo: this.route,
          }),
        title: 'Credentials',
        description: 'Password, keys, or other credentials of interest',
        icon: 'key-outline',
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
    ]
  }

  private async presentModalInstructions(pkg: PackageDataEntry) {
    const { id, version } = pkg.manifest

    this.apiService
      .setDbValue<boolean>(['ack-instructions', id], true)
      .catch(e => console.error('Failed to mark instructions as seen', e))

    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label: 'Instructions',
        size: 'l',
        data: {
          content: from(
            this.apiService.getStatic(
              `/public/package-data/${id}/${version}/INSTRUCTIONS.md`,
            ),
          ),
        },
      })
      .subscribe()
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
}
