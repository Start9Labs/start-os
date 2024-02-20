import { inject, Injectable } from '@angular/core'
import { ApiService, RecoverySource } from './api.service'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  private readonly api = inject(ApiService)

  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'
  recoverySource?: RecoverySource
  recoveryPassword?: string

  async importDrive(guid: string, password: string): Promise<void> {
    await this.api.attach({
      guid,
      'embassy-password': await this.api.encrypt(password),
    })
  }

  async setupEmbassy(
    storageLogicalname: string,
    password: string,
  ): Promise<void> {
    await this.api.execute({
      'embassy-logicalname': storageLogicalname,
      'embassy-password': await this.api.encrypt(password),
      'recovery-source': this.recoverySource || null,
      'recovery-password': this.recoveryPassword
        ? await this.api.encrypt(this.recoveryPassword)
        : null,
    })
  }
}
