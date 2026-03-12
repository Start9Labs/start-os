import { inject, Injectable } from '@angular/core'
import { ApiService, OutboundVpn } from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

@Injectable({ providedIn: 'root' })
export class OutboundService extends FormService<OutboundVpn[]> {
  private readonly api = inject(ApiService)

  async load(): Promise<OutboundVpn[]> {
    return this.api.vpnClientList()
  }

  async store(): Promise<void> {
    // List doesn't have a single store operation
  }

  update(id: string, data: { label: string; target: string }) {
    return this.actions.run(
      async () => {
        await this.api.vpnClientUpdate({ id, ...data })
        this.refresh()
      },
      {
        loading: 'Updating VPN client...',
        success: 'VPN client updated',
        restart: true,
      },
    )
  }

  create(data: { label: string; target: string; config: File }) {
    return this.actions.run(
      async () => {
        const configText = await data.config.text()
        await this.api.vpnClientCreate({
          label: data.label,
          target: data.target,
          config: configText,
        })
        this.refresh()
      },
      {
        loading: 'Creating VPN client...',
        success: 'VPN client created',
        restart: true,
      },
    )
  }

  remove(id: string) {
    return this.actions.run(
      async () => {
        await this.api.vpnClientDelete({ id })
        this.refresh()
      },
      {
        loading: 'Removing VPN client...',
        success: 'VPN client removed',
        restart: true,
      },
    )
  }

  // @TODO matt where should the associated element be exposed in the UI?
  setEnabled(id: string, enabled: boolean) {
    return this.actions.run(
      async () => {
        await this.api.vpnClientSetEnabled({ id, enabled })
        this.refresh()
      },
      {
        loading: enabled ? 'Enabling VPN client...' : 'Disabling VPN client...',
        success: enabled ? 'VPN client enabled' : 'VPN client disabled',
        restart: true,
      },
    )
  }
}
