import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { T, utils } from '@start9labs/start-sdk'
import { map } from 'rxjs/operators'
import { DataModel } from './patch-db/data-model'
import { toSignal } from '@angular/core/rxjs-interop'

export type GatewayPlus = T.NetworkInterfaceInfo & {
  id: string
  name: string
  ipInfo: T.IpInfo
  subnets: utils.IpNet[]
  lanIpv4: string[]
  wanIp?: utils.IpAddress
  public: boolean
}

@Injectable()
export class GatewayService {
  readonly gateways = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('serverInfo', 'network', 'gateways')
      .pipe(
        map(gateways =>
          Object.entries(gateways)
            .filter(([_, val]) => !!val?.ipInfo)
            .map(([id, val]) => {
              const subnets =
                val.ipInfo?.subnets.map(s => utils.IpNet.parse(s)) ?? []
              const name = val.name ?? val.ipInfo!.name
              return {
                ...val,
                id,
                name,
                subnets,
                lanIpv4: subnets.filter(s => s.isIpv4()).map(s => s.address),
                public: val.public ?? subnets.some(s => s.isPublic()),
                wanIp:
                  val.ipInfo?.wanIp && utils.IpAddress.parse(val.ipInfo?.wanIp),
              } as GatewayPlus
            }),
        ),
      ),
  )
}
