import { Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { pauseFor } from 'src/app/utils/pauseFor'

export interface EthernetPort {
  name: string
  permissions: string
  wan: boolean
}

@Injectable()
export class EthernetService extends FormService<EthernetPort[]> {
  private items: EthernetPort[] = [
    {
      name: 'eth0',
      permissions: 'Admin',
      wan: true,
    },
    {
      name: 'eth1',
      permissions: 'Admin',
      wan: false,
    },
    {
      name: 'eth2',
      permissions: 'Guest',
      wan: false,
    },
    {
      name: 'eth3',
      permissions: 'Guest',
      wan: false,
    },
  ]

  async load(): Promise<EthernetPort[]> {
    await pauseFor(2000)

    return this.items
  }

  async store(items: EthernetPort[]): Promise<void> {
    this.items = items
  }
}
