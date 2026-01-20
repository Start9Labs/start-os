import { Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { pauseFor } from 'src/app/utils/pauseFor'

export interface Forwarding {
  enabled?: boolean
  purpose: string
  protocol: string
  ip: string
  external: string
  internal: string
}

@Injectable()
export class ForwardingService extends FormService<Forwarding[]> {
  private items: Forwarding[] = [
    {
      enabled: true,
      purpose: 'Web Server',
      protocol: 'TCP',
      ip: '192.168.237.42',
      external: '1234',
      internal: '4321',
    },
    {
      enabled: false,
      purpose: 'Calls',
      protocol: 'TCP/UDP',
      ip: '192.168.0.69',
      external: '8080',
      internal: '6969',
    },
  ]

  async load(): Promise<Forwarding[]> {
    await pauseFor(2000)

    return this.items
  }

  async store(items: Forwarding[]): Promise<void> {
    this.items = items
  }
}
