import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ForwardingUciService } from './uci/service'

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
  private readonly uci = inject(ForwardingUciService)

  async load(): Promise<Forwarding[]> {
    return this.uci.get()
  }

  async store(items: Forwarding[]): Promise<void> {
    await this.uci.set(items)
  }
}
