import { inject, Injectable } from '@angular/core'
import { ApiService, LoginReq, WanIpv4 } from './api.service'
import { pauseFor } from '../../utils/pauseFor'
import { AuthService } from '../auth.service'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  private readonly auth = inject(AuthService)

  constructor() {
    super()
  }

  async login(params: LoginReq): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async logout(): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async getWanIpv4(): Promise<WanIpv4> {
    await pauseFor(1000)
    return {
      network: {
        sections: [
          {
            type: 'interface',
            name: 'wan',
            options: {
              proto: 'dhcp',
              ipaddr: '100.65.227.234',
              netmask: '255.255.255.252',
              gateway: '100.65.227.233',
              device: 'eth0.2',
            },
            lists: {},
          },
        ],
        modified: '2025-11-15T10:30:00Z',
      },

      dhcp: {
        sections: [
          {
            type: 'dnsmasq',
            name: 'cfg01411c',
            options: {},
            lists: {
              server: [], // Empty means using ISP DNS
            },
          },
        ],
        modified: '2025-11-15T10:30:00Z',
      },

      httpsDnsProxy: {
        sections: [], // Empty means not using DNS over TLS
        modified: '2025-11-15T10:30:00Z',
      },
    }
  }

  async setWanIpv4(params: WanIpv4): Promise<null> {
    await pauseFor(1000)
    return null
  }
}
