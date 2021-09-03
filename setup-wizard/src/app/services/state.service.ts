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

  torAddress: string

  constructor(
    private readonly apiService: ApiService
  ) {}

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

  async setupEmbassy () : Promise<{ torAddress: string }> {
    const ret = await this.apiService.setupEmbassy({
      embassyLogicalname: this.embassyDrive.logicalname,
      embassyPassword: this.embassyPassword,
      recoveryLogicalname: this.recoveryDrive?.logicalname,
      recoveryPassword: this.recoveryPassword
    })

    return { torAddress: ret['tor-address'] }
  }
}

export const pauseFor = (ms: number) => {
  const promise = new Promise(resolve => setTimeout(resolve, ms))
  return promise
};
