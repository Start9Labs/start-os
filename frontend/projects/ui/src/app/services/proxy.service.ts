import { Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  OsOutboundProxy,
  ServiceOutboundProxy,
} from './patch-db/data-model'
import { firstValueFrom } from 'rxjs'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { TuiDialogOptions } from '@taiga-ui/core'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormContext, FormPage } from '../apps/ui/modals/form/form.page'
import { ApiService } from './api/embassy-api.service'
import { ErrorService, LoadingService } from '@start9labs/shared'

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

  async presentModalSetOutboundProxy(serviceContext?: {
    packageId: string
    outboundProxy: ServiceOutboundProxy
  }) {
    const network = await firstValueFrom(
      this.patch.watch$('server-info', 'network'),
    )

    const outboundProxies = network.proxies
      .filter(p => p.type === 'outbound' || p.type === 'inbound-outbound')
      .reduce((prev, curr) => {
        return {
          [curr.id]: curr.name,
          ...prev,
        }
      }, {})

    const outboundProxy = serviceContext
      ? serviceContext.outboundProxy
      : network.outboundProxy

    const defaultValue = !outboundProxy
      ? 'none'
      : outboundProxy === 'primaryProxy'
      ? 'primary'
      : outboundProxy === 'primaryInterface'
      ? 'interface'
      : 'other'

    const disabled = serviceContext ? [] : ['interface']

    let variants: Record<string, { name: string; spec: Config<any> }> = {}

    if (serviceContext) {
      variants['interface'] = {
        name: 'Mirror Primary Interface',
        spec: Config.of({}),
      }
    }

    variants = {
      ...variants,
      primary: {
        name: 'Use System Primary',
        spec: Config.of({}),
      },
      other: {
        name: 'Other',
        spec: Config.of({
          proxyId: Value.select({
            name: 'Select Specific Proxy',
            required: {
              default:
                outboundProxy && typeof outboundProxy !== 'string'
                  ? outboundProxy.proxyId
                  : null,
            },
            values: outboundProxies,
          }),
        }),
      },
      none: {
        name: 'None',
        spec: Config.of({}),
      },
    }

    const config = Config.of({
      proxy: Value.union(
        {
          name: 'Select Proxy',
          required: { default: defaultValue },
          description: `
  <h5>Use System Primary</h5>The primary <i>inbound</i> proxy will be used. If you do not have a primary inbound proxy, no proxy will be used
  <h5>Mirror Primary Interface</h5>If you have an inbound proxy enabled for the primary interface, outbound traffic will flow through the same proxy
  <h5>Other</h5>The specific proxy you select will be used, overriding the default
  `,
          disabled,
        },
        Variants.of(variants),
      ),
    })

    const options: Partial<
      TuiDialogOptions<FormContext<typeof config.validator._TYPE>>
    > = {
      label: 'Outbound Proxy',
      data: {
        spec: await configBuilderToSpec(config),
        buttons: [
          {
            text: 'Save',
            handler: async value => {
              const proxy =
                value.proxy.unionSelectKey === 'none'
                  ? null
                  : value.proxy.unionSelectKey === 'primary'
                  ? 'primaryProxy'
                  : value.proxy.unionSelectKey === 'interface'
                  ? 'primaryInterface'
                  : { proxyId: value.proxy.unionValueKey.proxyId }
              await this.saveOutboundProxy(proxy, serviceContext?.packageId)
              return true
            },
          },
        ],
      },
    }
    this.formDialog.open(FormPage, options)
  }

  private async saveOutboundProxy(
    proxy: OsOutboundProxy | ServiceOutboundProxy,
    packageId?: string,
  ) {
    const loader = this.loader.open(`Saving`).subscribe()

    try {
      if (packageId) {
        await this.api.setServiceOutboundProxy({ packageId, proxy })
      } else {
        await this.api.setOsOutboundProxy({ proxy: proxy as OsOutboundProxy })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
