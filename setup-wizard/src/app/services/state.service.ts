import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { ApiService, CifsRecoverySource, DiskRecoverySource } from './api/api.service'
import { ErrorToastService } from './error-toast.service'
import { pauseFor } from '../util/misc.util'

@Injectable({
  providedIn: 'root',
})
export class StateService {
  hasProductKey: boolean
  isMigrating: boolean

  polling = false
  embassyLoaded = false

  recoverySource: CifsRecoverySource | DiskRecoverySource
  recoveryPassword: string

  dataTransferProgress: { bytesTransferred: number, totalBytes: number, complete: boolean } | null
  dataProgress = 0
  dataCompletionSubject = new BehaviorSubject(false)

  torAddress: string
  lanAddress: string
  cert: string

  constructor (
    private readonly apiService: ApiService,
    private readonly errorToastService: ErrorToastService,
  ) { }

  async pollDataTransferProgress () {
    this.polling = true
    await pauseFor(500)

    if (
      this.dataTransferProgress?.complete
    ) {
      this.dataCompletionSubject.next(true)
      return
    }


    let progress
    try {
      progress = await this.apiService.getRecoveryStatus()
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.details}.\nRestart Embassy to try again.`)
    }
    if (progress) {
      this.dataTransferProgress = {
        bytesTransferred: progress['bytes-transferred'],
        totalBytes: progress['total-bytes'],
        complete: progress.complete,
      }
      if (this.dataTransferProgress.totalBytes) {
        this.dataProgress = this.dataTransferProgress.bytesTransferred / this.dataTransferProgress.totalBytes
      }
    }
    setTimeout(() => this.pollDataTransferProgress(), 0) // prevent call stack from growing
  }

  async importDrive (guid: string): Promise<void> {
    const ret = await this.apiService.importDrive(guid)
    this.torAddress = ret['tor-address']
    this.lanAddress = ret['lan-address']
    this.cert = ret['root-ca']
  }

  async setupEmbassy (storageLogicalname: string, password: string): Promise<void> {
    const ret = await this.apiService.setupEmbassy({
      'embassy-logicalname': storageLogicalname,
      'embassy-password': password,
      'recovery-source': this.recoverySource || null,
      'recovery-password': this.recoveryPassword || null,
    })
    this.torAddress = ret['tor-address']
    this.lanAddress = ret['lan-address']
    this.cert = ret['root-ca']
  }
}
