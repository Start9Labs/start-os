import { inject, Pipe, PipeTransform } from '@angular/core'
import { Params } from '@angular/router'
import { MarkdownComponent } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { from } from 'rxjs'
import {
  ConfigModal,
  PackageConfigData,
} from 'src/app/routes/portal/modals/config.component'
import { ServiceAdditionalModal } from 'src/app/routes/portal/routes/service/modals/additional.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ProxyService } from 'src/app/services/proxy.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServicePropertiesModal } from 'src/app/routes/portal/routes/service/modals/properties.component'

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
        icon: 'tuiIconList',
        name: 'Instructions',
        description: `Understand how to use ${manifest.title}`,
        action: () => this.showInstructions(manifest),
      },
      {
        icon: 'tuiIconSliders',
        name: 'Config',
        description: `Customize ${manifest.title}`,
        action: () => this.openConfig(manifest),
      },
      {
        icon: 'tuiIconKey',
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
        icon: 'tuiIconZap',
        name: 'Actions',
        description: `Uninstall and other commands specific to ${manifest.title}`,
        routerLink: `actions`,
      },
      {
        icon: 'tuiIconShield',
        name: 'Outbound Proxy',
        description: `Proxy all outbound traffic from ${manifest.title}`,
        action: () =>
          this.proxyService.presentModalSetOutboundProxy(
            pkg.outboundProxy,
            manifest.id,
          ),
      },
      {
        icon: 'tuiIconFileText',
        name: 'Logs',
        description: `Raw, unfiltered logs`,
        routerLink: 'logs',
      },
      {
        icon: 'tuiIconInfo',
        name: 'Additional Info',
        description: `View package details`,
        action: () =>
          this.dialogs
            .open(new PolymorpheusComponent(ServiceAdditionalModal), {
              label: `Additional Info`,
              data: pkg,
            })
            .subscribe(),
      },
      pkg.marketplaceUrl
        ? {
            icon: 'tuiIconShoppingBag',
            name: 'Marketplace Listing',
            description: `View ${manifest.title} on the Marketplace`,
            routerLink: `/portal/system/marketplace`,
            params: { url: pkg.marketplaceUrl, id: manifest.id },
          }
        : {
            icon: 'tuiIconShoppingBag',
            name: 'Marketplace Listing',
            description: `This package was not installed from the marketplace`,
          },
    ]
  }

  private showInstructions({ title, id, version }: T.Manifest) {
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

  private openConfig({ title, id }: T.Manifest) {
    this.formDialog.open<PackageConfigData>(ConfigModal, {
      label: `${title} configuration`,
      data: { pkgId: id },
    })
  }
}
