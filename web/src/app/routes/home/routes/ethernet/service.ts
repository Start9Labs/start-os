import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ApiService } from 'src/app/services/api/api.service'
import { EthernetPort, EthernetUciService } from './uci/service'

// Re-export for convenience
export type { EthernetPort }

@Injectable()
export class EthernetService extends FormService<EthernetPort[]> {
  private readonly api = inject(ApiService)
  private readonly uci = inject(EthernetUciService)

  async load(): Promise<EthernetPort[]> {
    return this.uci.get()
  }

  async store(items: EthernetPort[]): Promise<void> {
    await this.uci.set(items)
  }

  /**
   * Get available security profiles (stub)
   */
  getProfiles(): string[] {
    return this.uci.getProfiles()
  }

  /**
   * Restart the router
   */
  restart(): void {
    this.api.systemRestart()
  }
}
