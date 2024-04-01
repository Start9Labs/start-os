import { inject, Pipe, PipeTransform } from '@angular/core'
import { Params } from '@angular/router'
import { MarkdownComponent } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { from } from 'rxjs'
import {
  PackageConfigData,
  ServiceConfigModal,
} from 'src/app/apps/portal/modals/config.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ProxyService } from 'src/app/services/proxy.service'
import { ServicePropertiesModal } from '../modals/properties.component'
import { getManifest } from 'src/app/util/get-package-data'
import { Manifest } from '../../../../../../../../../../core/startos/bindings/Manifest'

export interface ServiceMenu {
  icon: string
  name: string
  description: string
  action?: () => void
  routerLink?: string
  params?: Params
}

@Pipe({
  name: 'toMenu',
  standalone: true,
})
export class ToMenuPipe implements PipeTransform {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly formDialog = inject(FormDialogService)
  private readonly proxyService = inject(ProxyService)

  transform(pkg: PackageDataEntry): ServiceMenu[] {
    const manifest = getManifest(pkg)

    return [
      {
        icon: 'tuiIconListLarge',
        name: 'Instructions',
        description: `Understand how to use ${manifest.title}`,
        action: () => this.showInstructions(manifest),
      },
      {
        icon: 'tuiIconSlidersLarge',
        name: 'Config',
        description: `Customize ${manifest.title}`,
        action: () => this.openConfig(manifest),
      },
      {
        icon: 'tuiIconKeyLarge',
        name: 'Properties',
        description: `Runtime information, credentials, and other values of interest`,
        action: () =>
          this.dialogs
            .open(new PolymorpheusComponent(ServicePropertiesModal), {
              label: `${manifest.title} credentials`,
              data: manifest.id,
            })
            .subscribe(),
      },
      {
        icon: 'tuiIconZapLarge',
        name: 'Actions',
        description: `Uninstall and other commands specific to ${manifest.title}`,
        routerLink: `actions`,
      },
      {
        icon: 'tuiIconShieldLarge',
        name: 'Outbound Proxy',
        description: `Proxy all outbound traffic from ${manifest.title}`,
        action: () =>
          this.proxyService.presentModalSetOutboundProxy(
            pkg.outboundProxy,
            manifest.id,
          ),
      },
      {
        icon: 'tuiIconFileTextLarge',
        name: 'Logs',
        description: `Raw, unfiltered logs`,
        routerLink: 'logs',
      },
      pkg.marketplaceUrl
        ? {
            icon: 'tuiIconShoppingBagLarge',
            name: 'Marketplace Listing',
            description: `View ${manifest.title} on the Marketplace`,
            routerLink: `/portal/system/marketplace`,
            params: { url: pkg.marketplaceUrl, id: manifest.id },
          }
        : {
            icon: 'tuiIconShoppingBagLarge',
            name: 'Marketplace Listing',
            description: `This package was not installed from the marketplace`,
          },
    ]
  }

  private showInstructions({ title, id, version }: Manifest) {
    this.api
      .setDbValue<boolean>(['ack-instructions', id], true)
      .catch(e => console.error('Failed to mark instructions as seen', e))

    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label: `${title} instructions`,
        size: 'l',
        data: {
          content: from(
            this.api.getStatic(
              `/public/package-data/${id}/${version}/INSTRUCTIONS.md`,
            ),
          ),
        },
      })
      .subscribe()
  }

  private openConfig({ title, id }: Manifest) {
    this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
      label: `${title} configuration`,
      data: { pkgId: id },
    })
  }
}
