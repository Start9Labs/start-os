import { Injectable } from '@angular/core'
import { pauseFor } from '../state.service'
import { ApiService } from './api.service'

@Injectable({
  providedIn: 'root'
})
export class MockApiService extends ApiService {

  constructor() {
    super()
  }

  async getState() {
    await pauseFor(2000)
    return {
      'embassy-drive': 
      null,
      // {
      //   logicalname: 'name1',
      //   labels: ['label 1', 'label 2'],
      //   capacity: 1600,
      //   used: 200,
      // },
      'recovery-drive': 
      null,
      // {
      //   logicalname: 'name1',
      //   version: '0.3.3',
      //   name: 'My Embassy'
      // }
    }
  }

  async getDataTransferProgress() {
    tries = Math.min(tries + 1, 4)
    return {
      'bytes-transfered': tries,
      'total-bytes': 4
    }
  }

  async getEmbassyDrives() {
    await pauseFor(2000)
    return [
      {
        logicalname: 'Name1',
        labels: ['label 1', 'label 2'],
        capacity: 1600.66666,
        used: 200.1255312,
      },
      {
        logicalname: 'Name2',
        labels: [],
        capacity: 1600.01234,
        used: 0.00,
      }
    ]
  }

  async selectEmbassyDrive(drive) {
    await pauseFor(2000)
    return
  }

  async getRecoveryDrives() {
    await pauseFor(2000)
    return [
      {
        logicalname: 'Name1',
        version: '0.3.3',
        name: 'My Embassy'
      },
      {
        logicalname: 'Name2',
        version: '0.2.7',
        name: 'My Embassy'
      }
    ]
  }

  async selectRecoveryDrive(logicalName, password) {
    await pauseFor(2000)
    return
  }

  async submitPassword(password) {
    await pauseFor(2000)
    return
  }

  async verifyRecoveryPassword(logicalname, password) {
    await pauseFor(2000)
    return password.length > 8
  }
}

let tries = 0
