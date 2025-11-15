import { Injectable } from '@angular/core'
import {
  DnsmasqSection,
  HttpsDnsProxySection,
  NetworkInterfaceSection,
  UciFile,
} from './types'

@Injectable({
  providedIn: 'root',
})
export abstract class ApiService {
  // ** smart **
  abstract login(params: LoginReq): Promise<null>
  abstract logout(): Promise<null>
  abstract getWanIpv4(): Promise<WanIpv4>
  abstract setWanIpv4(params: WanIpv4): Promise<null>
}

export type LoginReq = { password: string }

export type WanIpv4 = {
  network: UciFile<NetworkInterfaceSection>
  dhcp: UciFile<DnsmasqSection>
  httpsDnsProxy: UciFile<HttpsDnsProxySection>
}
