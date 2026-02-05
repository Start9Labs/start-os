import { inject, Injectable } from '@angular/core'
import {
  ApiService,
  VpnServer,
  VpnServerConfig,
  VpnServerPeer,
  VpnServerPeerAddResponse,
} from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

export type {
  VpnServer,
  VpnServerPeer,
  VpnServerConfig,
  VpnServerPeerAddResponse,
}

@Injectable()
export class InboundService extends FormService<VpnServer[]> {
  private readonly api = inject(ApiService)

  async load() {
    const res = await this.api.vpnServerList()
    return res.servers
  }

  async store() {}

  async setServer(profile: string, config: VpnServerConfig) {
    await this.actions.run(async () => {
      await this.api.vpnServerSet({ profile, config })
      this.refresh()
    })
  }

  async deleteServer(profile: string) {
    await this.actions.run(async () => {
      await this.api.vpnServerDelete({ profile })
      this.refresh()
    })
  }

  async addPeer(
    profile: string,
    peer: VpnServerPeer,
  ): Promise<VpnServerPeerAddResponse | undefined> {
    let response: VpnServerPeerAddResponse | undefined
    await this.actions.run(async () => {
      response = await this.api.vpnServerPeerAdd({ profile, peer })
      this.refresh()
    })
    return response
  }

  async deletePeer(profile: string, public_key: string) {
    await this.actions.run(async () => {
      await this.api.vpnServerPeerDelete({ profile, public_key })
      this.refresh()
    })
  }
}
