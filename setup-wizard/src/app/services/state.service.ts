import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { ApiService, DiskInfo } from './api/api.service'
import { ErrorToastService } from './error-toast.service'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  hasProductKey: boolean
  isMigrating: boolean

  polling = false

  storageDrive: DiskInfo
  embassyPassword: string
  recoveryDrive: DiskInfo
  recoveryPassword: string
  dataTransferProgress: { bytesTransferred: number; totalBytes: number } | null
  dataProgress = 0
  dataProgSubject = new BehaviorSubject(this.dataProgress)

  torAddress: string
  lanAddress: string

  constructor (
    private readonly apiService: ApiService,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async pollDataTransferProgress (callback?: () => void) {
    this.polling = true
    await pauseFor(1000)

    if (
      this.dataTransferProgress?.totalBytes &&
      this.dataTransferProgress.bytesTransferred === this.dataTransferProgress.totalBytes
    ) return


    let progress
    try {
      progress = await this.apiService.getRecoveryStatus()
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.details}`)
    }
    if (progress) {
      this.dataTransferProgress = {
        bytesTransferred: progress['bytes-transferred'],
        totalBytes: progress['total-bytes'],
      }
      if (this.dataTransferProgress.totalBytes) {
        this.dataProgress = this.dataTransferProgress.bytesTransferred / this.dataTransferProgress.totalBytes
        this.dataProgSubject.next(this.dataProgress)
      }
    }
    this.pollDataTransferProgress(callback)
  }

  async setupEmbassy () : Promise<void> {
    const ret = await this.apiService.setupEmbassy({
      'embassy-logicalname': this.storageDrive.logicalname,
      'embassy-password': this.embassyPassword,
      'recovery-drive': this.recoveryDrive,
      'recovery-password': this.recoveryPassword,
    })
    this.torAddress = ret['tor-address']
    this.lanAddress = ret['lan-address']
  }
}

export const pauseFor = (ms: number) => {
  const promise = new Promise(resolve => setTimeout(resolve, ms))
  return promise
}
