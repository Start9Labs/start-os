import { Injectable } from "@angular/core";
import { ApiService } from "./api/api.service"

@Injectable({
  providedIn: 'root'
})
export class StateService {
  loading = true

  selectedDataDrive: string
  recoveryDrive: string
  hasPassword: Boolean
  dataTransferProgress: { bytesTransfered: number, totalBytes: number } | null
  dataProgress = 0

  constructor (
    private readonly apiService: ApiService
  ) {}

  async getState () {
    const state = await this.apiService.getState()
    if(state) {
      this.selectedDataDrive = state['selected-data-drive']
      this.recoveryDrive = state['recovery-drive']
      this.hasPassword = state['has-password']
  
      this.loading = false
    }
  }

  async pollDataTransferProgress () {
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
    await pauseFor(7000)
    this.pollDataTransferProgress()
  }
}

export function pauseFor (ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms))
}