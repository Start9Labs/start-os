import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs';
import { ApiService, EmbassyDrive, RecoveryDrive } from './api/api.service'

@Injectable({
  providedIn: 'root'
})
export class StateService {
  polling = false

  embassyDrive: EmbassyDrive;
  embassyPassword: string
  recoveryDrive: RecoveryDrive;
  recoveryPassword: string
  dataTransferProgress: { bytesTransfered: number; totalBytes: number } | null;
  dataProgress = 0;
  dataProgSubject = new BehaviorSubject(this.dataProgress)
  constructor(
    private readonly apiService: ApiService
  ) {}

  reset() {
    this.polling = false

    this.embassyDrive = null
    this.embassyPassword = null
    this.recoveryDrive = null
    this.recoveryPassword = null
    this.dataTransferProgress = null
    this.dataProgress = 0
  }

  async pollDataTransferProgress(callback?: () => void) {
    this.polling = true
    await pauseFor(1000)

    if (
      this.dataTransferProgress?.totalBytes &&
      this.dataTransferProgress.bytesTransfered === this.dataTransferProgress.totalBytes
    ) {return }
    
    const progress = await this.apiService.getDataTransferProgress()
    this.dataTransferProgress = {
      bytesTransfered: progress['bytes-transfered'],
      totalBytes: progress['total-bytes']
    }
    if (this.dataTransferProgress.totalBytes) {
      this.dataProgress = this.dataTransferProgress.bytesTransfered / this.dataTransferProgress.totalBytes
      this.dataProgSubject.next(this.dataProgress)
    }
    this.pollDataTransferProgress(callback)
  }

  async setupEmbassy () {
    await this.apiService.setupEmbassy({
      embassyLogicalname: this.embassyDrive.logicalname,
      embassyPassword: this.embassyPassword,
      recoveryLogicalname: this.recoveryDrive?.logicalname,
      recoveryPassword: this.recoveryPassword
    })
  }
}

export const pauseFor = (ms: number) => {
  const promise = new Promise(resolve => setTimeout(resolve, ms))
  return promise
};
