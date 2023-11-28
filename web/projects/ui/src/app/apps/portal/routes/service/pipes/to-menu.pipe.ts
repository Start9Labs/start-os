import { inject, Pipe, PipeTransform, Type } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { Manifest } from '@start9labs/marketplace'
import { MarkdownComponent } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { from } from 'rxjs'
import {
  InstalledPackageInfo,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ProxyService } from 'src/app/services/proxy.service'
import { PackageConfigData } from '../types/package-config-data'
import { ServiceConfigModal } from '../modals/config.component'
import { ServiceCredentialsModal } from '../modals/credentials.component'

export interface ServiceMenu {
  icon: string
  name: string
  description: string
  action: () => void
}

@Pipe({
  name: 'toMenu',
  standalone: true,
})
export class ToMenuPipe implements PipeTransform {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly formDialog = inject(FormDialogService)
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly proxyService = inject(ProxyService)

  transform({ manifest, installed }: PackageDataEntry): ServiceMenu[] {
    const url = installed?.['marketplace-url']

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
        name: 'Credentials',
        description: `Password, keys, or other credentials of interest`,
        action: () =>
          this.showDialog(
            `${manifest.title} credentials`,
            manifest.id,
            ServiceCredentialsModal,
          ),
      },
      {
        icon: 'tuiIconZapLarge',
        name: 'Actions',
        description: `Uninstall and other commands specific to ${manifest.title}`,
        action: () =>
          this.router.navigate(['actions'], {
            relativeTo: this.route,
          }),
      },
      {
        icon: 'tuiIconShieldLarge',
        name: 'Outbound Proxy',
        description: `Proxy all outbound traffic from ${manifest.title}`,
        action: () => this.setProxy(manifest, installed!),
      },
      {
        icon: 'tuiIconFileTextLarge',
        name: 'Logs',
        description: `Raw, unfiltered logs`,
        action: () =>
          this.router.navigate(['logs'], {
            relativeTo: this.route,
          }),
      },
      url
        ? {
            icon: 'tuiIconShoppingBagLarge',
            name: 'Marketplace Listing',
            description: `View ${manifest.title} on the Marketplace`,
            action: () =>
              this.router.navigate(['marketplace'], {
                relativeTo: this.route,
                queryParams: { url, id: manifest.id },
              }),
          }
        : {
            icon: 'tuiIconShoppingBagLarge',
            name: 'Marketplace Listing',
            description: `This package was not installed from the marketplace`,
            action: () => {},
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

  private showDialog(label: string, data: any, modal: Type<any>) {
    this.dialogs
      .open(new PolymorpheusComponent(modal), {
        size: 'l',
        label,
        data,
      })
      .subscribe()
  }

  private openConfig({ title, id }: Manifest) {
    this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
      label: `${title} configuration`,
      data: { pkgId: id },
    })
  }

  private setProxy(
    { id }: Manifest,
    { outboundProxy, interfaceInfo }: InstalledPackageInfo,
  ) {
    this.proxyService.presentModalSetOutboundProxy({
      outboundProxy,
      packageId: id,
      hasP2P: Object.values(interfaceInfo).some(i => i.type === 'p2p'),
    })
  }
}
