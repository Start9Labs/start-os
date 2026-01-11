import { Injectable } from '@angular/core'
import { pauseFor } from '../../utils/pauseFor'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  LoginReq,
  SetUciReq,
  SetUciRes,
} from './api.service'
import { UciFile, UciSection } from './types'
import { wanIpv4Dhcp } from 'src/app/routes/home/routes/wan/routes/ipv4/uci/mocks'
import { wanIpv6Slaac } from 'src/app/routes/home/routes/wan/routes/ipv6/uci/mock'
import { ddnsStart9 } from 'src/app/routes/home/routes/wan/routes/ddns/uci/mocks'
import { macRouterDevice } from 'src/app/routes/home/routes/wan/routes/mac/uci/mocks'
import { lanDefault } from 'src/app/routes/home/routes/lan/routes/ipv4/uci/mocks'
import { dhcpLanSlaacDhcpv6 } from 'src/app/routes/home/routes/lan/routes/ipv6/uci/mocks'
import { mockWireGuardSections } from 'src/app/routes/home/routes/outbound/uci/mocks'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  async login(params: LoginReq): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async logout(): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async exec(params: ExecReq): Promise<ExecRes> {
    await pauseFor(1000)

    return {
      exitCode: 0,
      stdout: 'success',
      stderr: '',
    }
  }

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(1000)

    return {
      modified: new Date().toISOString(),
      contents: '',
    }
  }

  async setFile(params: GetFileRes): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async getUci<T extends Record<string, UciFile<any>>>(
    params: GetUciReq,
  ): Promise<T> {
    await pauseFor(1000)

    return params.names.reduce(
      (obj, name) => ({
        ...obj,
        [name]: mockUci[name],
      }),
      {} as T,
    )
  }

  async setUci<T extends string[]>(params: SetUciReq): Promise<SetUciRes<T>> {
    await pauseFor(1000)

    const isoString = new Date().toISOString()

    return Object.keys(params).reduce(
      (obj, name) => ({
        ...obj,
        [name]: isoString,
      }),
      {} as SetUciRes<T>,
    )
  }
}

export const mockUci: Record<string, UciFile<UciSection>> = {
  network: {
    sections: [
      wanIpv4Dhcp,
      wanIpv6Slaac,
      macRouterDevice,
      lanDefault,
      ...mockWireGuardSections,
    ],
    modified: new Date().toISOString(),
  },
  ddns: {
    sections: [ddnsStart9],
    modified: new Date().toISOString(),
  },
  dhcp: {
    sections: [dhcpLanSlaacDhcpv6],
    modified: new Date().toISOString(),
  },
}
