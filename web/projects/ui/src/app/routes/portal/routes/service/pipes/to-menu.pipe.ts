import { inject, Pipe, PipeTransform } from '@angular/core'
import { Params } from '@angular/router'
import { MARKDOWN } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { from } from 'rxjs'
import { ServiceAdditionalModal } from 'src/app/routes/portal/routes/service/modals/additional.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ProxyService } from 'src/app/services/proxy.service'
import { getManifest } from 'src/app/utils/get-package-data'

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
        icon: '@tui.list',
        name: 'Instructions',
        description: `Understand how to use ${manifest.title}`,
        action: () => this.showInstructions(manifest),
      },
      {
        icon: '@tui.zap',
        name: 'Actions',
        description: `Uninstall and other commands specific to ${manifest.title}`,
        routerLink: `actions`,
      },
      {
        icon: '@tui.shield',
        name: 'Outbound Proxy',
        description: `Proxy all outbound traffic from ${manifest.title}`,
        action: () =>
          this.proxyService.presentModalSetOutboundProxy(
            pkg.outboundProxy,
            manifest.id,
          ),
      },
      {
        icon: '@tui.file-text',
        name: 'Logs',
        description: `Raw, unfiltered logs`,
        routerLink: 'logs',
      },
      {
        icon: '@tui.info',
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
      pkg.registry
        ? {
            icon: '@tui.shopping-bag',
            name: 'Marketplace Listing',
            description: `View ${manifest.title} on the Marketplace`,
            routerLink: `/portal/system/marketplace`,
            params: { url: pkg.registry, id: manifest.id },
          }
        : {
            icon: '@tui.shopping-bag',
            name: 'Marketplace Listing',
            description: `This package was not installed from the marketplace`,
          },
    ]
  }

  private showInstructions({ title, id }: T.Manifest) {
    this.api
      .setDbValue<boolean>(['ack-instructions', id], true)
      .catch(e => console.error('Failed to mark instructions as seen', e))

    this.dialogs
      .open(MARKDOWN, {
        label: `${title} instructions`,
        size: 'l',
        data: {
          content: from(this.api.getStaticInstalled(id, 'instructions.md')),
        },
      })
      .subscribe()
  }
}
