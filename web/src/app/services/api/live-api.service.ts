import { Injectable, inject } from '@angular/core'
import { ApiService, LoginReq, WanIpv4 } from './api.service'
import { RpcService } from '../rpc.service'
import {
  DnsmasqSection,
  HttpsDnsProxySection,
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from './types'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  private readonly rpc = inject(RpcService)

  constructor() {
    super()
  }

  // ** smart **

  async login(params: LoginReq): Promise<null> {
    return this.rpc.request({ method: 'auth.login', params })
  }

  async logout(): Promise<null> {
    return this.rpc.request({ method: 'auth.logout', params: {} })
  }

  async getWanIpv4(): Promise<WanIpv4> {
    const [network, dhcp, httpsDnsProxy] = await Promise.all([
      this.getUci<NetworkInterfaceSection>({ name: 'network' }),
      this.getUci<DnsmasqSection>({ name: 'dhcp' }),
      this.getUci<HttpsDnsProxySection>({ name: 'https-dns-proxy' }),
    ])

    return { network, dhcp, httpsDnsProxy }
  }

  async setWanIpv4(params: WanIpv4): Promise<null> {
    const { network, dhcp, httpsDnsProxy } = params

    await this.setUci({ network, dhcp, 'https-dns-proxy': httpsDnsProxy })

    await this.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })

    await this.exec({
      command: '/etc/init.d/dnsmasq',
      args: ['restart'],
      timeout: 10000,
    })

    await this.exec({
      command: '/etc/init.d/https-dns-proxy',
      args: ['restart'],
      timeout: 10000,
    })

    return null
  }

  // ** stupid **

  private async exec(params: ExecReq): Promise<ExecRes> {
    return this.rpc.request({ method: 'exec', params })
  }

  private async getFile(params: GetFileReq): Promise<GetFileRes> {
    return this.rpc.request({ method: 'file.get', params })
  }

  private async setFile(params: GetFileRes): Promise<null> {
    return this.rpc.request({ method: 'file.set', params })
  }

  private async getUci<T extends UciSection>(
    params: GetUciReq,
  ): Promise<GetUciRes<T>> {
    return this.rpc.request({ method: 'uci.get', params })
  }

  private async setUci(params: SetUciReq): Promise<null> {
    return this.rpc.request({ method: 'uci.set', params })
  }
}

export type ExecReq = {
  command: string
  args: string[]
  timeout: number
}

export type ExecRes = {
  stdout: string
  stderr: string
  exitCode: number
}

export type GetFileReq = {
  path: string
}

export type GetFileRes = {
  contents: string
  modified: string
}

export type SetFileReq = GetFileReq & GetFileRes

export type GetUciReq = {
  name: string
}

export type GetUciRes<T extends UciSection> = UciFile<T>

export type SetUciReq = Record<string, UciFile<any>>
