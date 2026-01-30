import { Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { pauseFor } from 'src/app/utils/pauseFor'

export interface Client {
  name: string
  address: string
  key: string
}

export interface Inbound {
  id: string
  label: string
  enabled: boolean
  securityProfile: string
  address: string
  port: number
  clients: Client[]
}

@Injectable()
export class InboundService extends FormService<Inbound[]> {
  private items: Inbound[] = [
    {
      id: '1',
      label: 'Family',
      address: 'Custom',
      enabled: true,
      securityProfile: 'Guest',
      port: 237,
      clients: [],
    },
    {
      id: '2',
      label: 'Matt',
      address: 'agf5d.start9.me',
      enabled: false,
      securityProfile: 'Admin',
      port: 42,
      clients: [
        {
          name: 'tablet',
          address: '192.168.0.237',
          key: '2JIBoK+Bxe7MJzX9zV+lFjqHxLTvehLp3piEROaNJjw=',
        },
        {
          name: 'Smartphone',
          address: '192.168.0.42',
          key: 'bZIOgRYRGTX9x0OQsN1K+R63EhT2Pgo0WzYatTmzdDU=',
        },
      ],
    },
  ]

  async load() {
    await pauseFor(2000)

    return this.items
  }

  async store(items: Inbound[]) {
    this.items = items
  }
}
