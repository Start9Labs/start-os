import { Injectable } from '@angular/core'
import { ApiService, DataDrive, RecoveryDrive } from './api/api.service'

@Injectable({
  providedIn: 'root'
})
export class StateService {
  loading = true
  polling = false

  dataDrive: DataDrive;
  recoveryDrive: RecoveryDrive;
  dataTransferProgress: { bytesTransfered: number; totalBytes: number } | null;
  dataProgress = 0;

  constructor(
    private readonly apiService: ApiService
  ) {}

  async getState() {
    this.loading = true
    const state = await this.apiService.getState()
    if(state) {
      this.dataDrive = state['data-drive']
      this.recoveryDrive = state['recovery-drive']

      this.loading = false
    }
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
