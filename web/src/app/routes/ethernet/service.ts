import { inject, Injectable, signal } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import {
  ApiService,
  EthernetConfig,
  ProfileId,
} from 'src/app/services/api/api.service'

export interface EthernetPortView {
  name: string
  profile: ProfileId | null
  profileName: string
  wan: boolean
}

@Injectable()
export class EthernetService extends FormService<EthernetPortView[]> {
  private readonly api = inject(ApiService)

  /** Cached wan_ipv6 from last get, preserved across saves */
  private wanIpv6 = false

  /** Available profiles for the dropdown */
  readonly profiles = signal<ProfileId[]>([])

  async load(): Promise<EthernetPortView[]> {
    const [ethernet, profiles] = await Promise.all([
      this.api.ethernetGet(),
      this.api.profilesList(),
    ])

    this.wanIpv6 = ethernet.wan_ipv6
    this.profiles.set(profiles)

    return this.toPortViews(ethernet)
  }

  override async save(data: EthernetPortView[]): Promise<boolean> {
    return this.actions.run(() => this.store(data), {
      loading: 'Restarting network...',
      success: 'Ethernet settings saved',
      restart: true,
    })
  }

  async store(items: EthernetPortView[]): Promise<void> {
    const wanPort = items.find(p => p.wan)?.name ?? null

    await this.api.ethernetSet({
      wan_ipv6: this.wanIpv6,
      wan_port: wanPort,
      ports: Object.fromEntries(
        items.map(p => [
          p.name,
          {
            profile: p.wan ? null : p.profile,
          },
        ]),
      ),
    })
  }

  private toPortViews(ethernet: EthernetConfig): EthernetPortView[] {
    return Object.entries(ethernet.ports)
      .map(([name, port]) => ({
        name,
        profile: port.profile,
        profileName: port.profile?.fullname ?? 'Admin',
        wan: name === ethernet.wan_port,
      }))
      .sort((a, b) => a.name.localeCompare(b.name))
  }
}
