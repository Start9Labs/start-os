import { Injectable } from '@angular/core'
import { ApiService } from './api/api.service'
import { T } from '@start9labs/start-sdk'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'
  recoverySource?: T.RecoverySource<string>

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
      recoverySource: this.recoverySource
        ? this.recoverySource.type === 'migrate'
          ? this.recoverySource
          : {
              ...this.recoverySource,
              password: await this.api.encrypt(this.recoverySource.password),
            }
        : null,
    })
  }
}
