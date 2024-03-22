import { Injectable } from '@angular/core'
import { ApiService, RecoverySource } from './api/api.service'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'

  recoverySource?: RecoverySource
  recoveryPassword?: string

  constructor(private readonly api: ApiService) {}

  async importDrive(guid: string, password: string): Promise<void> {
    await this.api.attach({
      guid,
      startOsPassword: await this.api.encrypt(password),
    })
  }

  async setupEmbassy(
    storageLogicalname: string,
    password: string,
  ): Promise<void> {
    await this.api.execute({
      startOsLogicalname: storageLogicalname,
      startOsPassword: await this.api.encrypt(password),
      recoverySource: this.recoverySource || null,
      recoveryPassword: this.recoveryPassword
        ? await this.api.encrypt(this.recoveryPassword)
        : null,
    })
  }
}
