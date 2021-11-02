import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { ApiService, DiskInfo, PartitionInfo } from './api/api.service'
import { ErrorToastService } from './error-toast.service'
import { pauseFor } from '../util/misc.util'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  hasProductKey: boolean
  isMigrating: boolean

  polling = false

  storageDrive: DiskInfo
  embassyPassword: string
  recoveryPartition: PartitionInfo
  recoveryPassword: string
  dataTransferProgress: { bytesTransferred: number; totalBytes: number } | null
  dataProgress = 0
  dataProgSubject = new BehaviorSubject(this.dataProgress)

  torAddress: string
  lanAddress: string
  cert: string

  constructor (
    private readonly apiService: ApiService,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async pollDataTransferProgress () {
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
    this.pollDataTransferProgress()
  }


  async importDrive (guid: string) : Promise<void> {
    const ret = await this.apiService.importDrive(guid)
    this.torAddress = ret['tor-address']
    this.lanAddress = ret['lan-address']
    this.cert = ret['root-ca']
  }

  async setupEmbassy () : Promise<void> {
    const ret = await this.apiService.setupEmbassy({
      'embassy-logicalname': this.storageDrive.logicalname,
      'embassy-password': this.embassyPassword,
      'recovery-partition': this.recoveryPartition,
      'recovery-password': this.recoveryPassword,
    })
    this.torAddress = ret['tor-address']
    this.lanAddress = ret['lan-address']
    this.cert = ret['root-ca']
  }
}
