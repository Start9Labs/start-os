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
      'data-drive': 
      // null,
      {
        logicalname: 'name1',
        labels: ['label 1', 'label 2'],
        capacity: 1600,
        used: 200,
      },
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

  async getDataDrives() {
    await pauseFor(2000)
    return [
      {
        logicalname: 'Name1',
        labels: ['label 1', 'label 2'],
        capacity: 1600,
        used: 200,
      },
      {
        logicalname: 'Name2',
        labels: [],
        capacity: 1600,
        used: 0,
      }
    ]
  }

  async selectDataDrive(drive) {
    await pauseFor(2000)
    return
  }

  async getRecoveryDrives() {
    await pauseFor(2000)
    return [
      {
        logicalname: 'name1',
        version: '0.3.3',
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
}

let tries = 0
