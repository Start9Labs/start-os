import { Injectable } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { ApiService } from './api/embassy-api.service'
import { DataModel } from './patch-db/data-model'
import { CB } from '@start9labs/start-sdk'

@Injectable({
  providedIn: 'root',
})
export class ProxyService {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
    private readonly api: ApiService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
  ) {}

  async presentModalSetOutboundProxy(current: string | null, pkgId?: string) {
    const network = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network'),
    )
    const config = CB.Config.of({
      proxyId: CB.Value.select({
        name: 'Select Proxy',
        required: { default: current },
        values: network.proxies
          .filter(p => p.type === 'outbound' || p.type === 'inbound-outbound')
          .reduce((prev, curr) => {
            return {
              [curr.id]: curr.name,
              ...prev,
            }
          }, {}),
      }),
    })

    const options: Partial<
      TuiDialogOptions<FormContext<typeof config.validator._TYPE>>
    > = {
      label: 'Outbound Proxy',
      data: {
        spec: await configBuilderToSpec(config),
        buttons: [
          {
            text: 'Manage proxies',
            link: '/portal/system/settings/proxies',
          },
          {
            text: 'Save',
            handler: async value => {
              await this.saveOutboundProxy(value.proxyId, pkgId)
              return true
            },
          },
        ],
      },
    }
    this.formDialog.open(FormComponent, options)
  }

  private async saveOutboundProxy(proxy: string | null, packageId?: string) {
    const loader = this.loader.open(`Saving`).subscribe()

    try {
      if (packageId) {
        await this.api.setServiceOutboundProxy({ packageId, proxy })
      } else {
        await this.api.setOsOutboundProxy({ proxy })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
