import { computed, inject, Injectable } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { T, utils } from '@start9labs/start-core'
import { PatchDB } from 'patch-db-client'

import { DataModel } from './patch-db/data-model'

export type GatewayDetails = T.NetworkInterfaceInfo & {
  id: string
  name: string
  subnets: utils.IpNet[]
  lanIpv4: string[]
  wanIp?: utils.IpAddress
}

export type GatewayPlus = GatewayDetails & { ipInfo: T.IpInfo }

export function gatewayName(
  id: string,
  gateway?: T.NetworkInterfaceInfo,
): string {
  return gateway?.name ?? gateway?.ipInfo?.name ?? id
}

@Injectable()
export class GatewayService {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  private readonly network = toSignal(
    this.patch.watch$('serverInfo', 'network'),
  )

  readonly defaultOutbound = computed(() => this.network()?.defaultOutbound)

  readonly defaultOutboundGateway = computed(() => {
    const network = this.network()
    const id = network?.defaultOutbound
    if (!id) return null
    return { id, name: gatewayName(id, network.gateways[id]) }
  })

  readonly configuredGateways = computed(() => {
    const network = this.network()
    if (!network) return
    return Object.entries(network.gateways)
      .filter(
        ([, gateway]) =>
          !['bridge', 'loopback'].includes(gateway.ipInfo?.deviceType ?? ''),
      )
      .map(([id, gateway]): GatewayDetails => {
        const subnets =
          gateway.ipInfo?.subnets.map(s => utils.IpNet.parse(s)) ?? []
        return {
          ...gateway,
          id,
          name: gatewayName(id, gateway),
          subnets,
          lanIpv4: subnets.filter(s => s.isIpv4()).map(s => s.address),
          wanIp: gateway.ipInfo?.wanIp
            ? utils.IpAddress.parse(gateway.ipInfo.wanIp)
            : undefined,
        }
      })
  })

  readonly gateways = computed(() =>
    this.configuredGateways()?.filter(
      (gateway): gateway is GatewayPlus => !!gateway.ipInfo,
    ),
  )
}
