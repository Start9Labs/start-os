import { Injectable } from '@angular/core'
import { ApiService, EmbassyDrive, RecoveryDrive } from './api/api.service'

@Injectable({
  providedIn: 'root'
})
export class StateService {
  polling = false

  embassyDrive: EmbassyDrive;
  recoveryDrive: RecoveryDrive;
  recoveryPassword: string
  dataTransferProgress: { bytesTransfered: number; totalBytes: number } | null;
  dataProgress = 0;

  constructor(
    private readonly apiService: ApiService
  ) {}

  reset() {
    this.polling = false

    this.embassyDrive = null
    this.recoveryDrive = null
    this.recoveryPassword = null
    this.dataTransferProgress = null
    this.dataProgress = 0
  }

  async pollDataTransferProgress() {
    this.polling = true
    await pauseFor(7000)

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
    }
    this.pollDataTransferProgress()
  }
}

export const pauseFor = (ms: number) => {
  const promise = new Promise(resolve => setTimeout(resolve, ms))
  return promise
};
