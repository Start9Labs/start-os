import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { ApiService, RecoverySource } from './api/api.service'
import { pauseFor, ErrorToastService } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'

  recoverySource?: RecoverySource
  recoveryPassword?: string

  dataTransferProgress?: {
    bytesTransferred: number
    totalBytes: number | null
    complete: boolean
  }
  dataProgress$ = new BehaviorSubject<number>(0)
  dataCompletionSubject$ = new BehaviorSubject(false)

  constructor(
    private readonly api: ApiService,
    private readonly errorToastService: ErrorToastService,
  ) {}

  async pollDataTransferProgress() {
    await pauseFor(500)

    if (this.dataTransferProgress?.complete) {
      this.dataCompletionSubject$.next(true)
      return
    }

    try {
      const progress = await this.api.getStatus()
      if (!progress) return

      this.dataTransferProgress = {
        bytesTransferred: progress['bytes-transferred'],
        totalBytes: progress['total-bytes'],
        complete: progress.complete,
      }
      if (this.dataTransferProgress.totalBytes) {
        this.dataProgress$.next(
          this.dataTransferProgress.bytesTransferred /
            this.dataTransferProgress.totalBytes,
        )
      }
    } catch (e: any) {
      this.errorToastService.present({
        message: `${e.message}\n\nRestart Embassy to try again.`,
      })
    }
    setTimeout(() => this.pollDataTransferProgress(), 0) // prevent call stack from growing
  }

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
