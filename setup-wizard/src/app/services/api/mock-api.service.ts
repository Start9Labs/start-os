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

  async verifyProductKey(key) {
    await pauseFor(2000)
    return { 
      "is-recovering": false,
      "tor-address": null 
    }
  }

  async getDataTransferProgress() {
    tries = Math.min(tries + 1, 4)
    return {
      'bytes-transfered': tries,
      'total-bytes': 4
    }
  }

  async getDrives() {
    return [
      {
        vendor: 'Vendor',
        model: 'Model',
        logicalname: '/dev/sda',
        partitions: [
          {
            logicalname: 'sda1',
            label: 'label 1',
            capacity: 100000,
            used: 200.1255312
          },
          {
            logicalname: 'sda2',
            label: 'label 2',
            capacity: 50000,
            used: 200.1255312
          }
        ],
        capacity: 150000,
        'embassy_os': null
      },
      {
        vendor: 'Vendor',
        model: 'Model',
        logicalname: 'dev/sdb',
        partitions: [
          // {
          //   logicalname: 'sdb1',
          //   label: null,
          //   capacity: 1600.01234,
          //   used: 0.00,
          // }
        ],
        capacity: 1600.01234,
        'embassy_os': null
      },
      {
        vendor: 'Vendor',
        model: 'Model',
        logicalname: 'dev/sdc',
        partitions: [
          {
            logicalname: 'sdc1',
            label: 'label 1',
            capacity: null,
            used: null
          }
        ],
        capacity: 100000,
        'embassy_os': {
          version: '0.3.3',
        }
      },
      {
        vendor: 'Vendor',
        model: 'Model',
        logicalname: '/dev/sdd',
         partitions: [
          {
            logicalname: 'sdd1',
            label: null,
            capacity: 10000,
            used: null
          }
        ],
        capacity: 10000,
        'embassy_os': {
          version: '0.2.7',
        }
      }
    ]
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

  async verifyRecoveryPassword(logicalname, password) {
    await pauseFor(2000)
    return password.length > 8
  }

  async setupEmbassy (setupInfo) {
    await pauseFor(2000)
    return { "tor-address": 'asdfasdfasdf.onion' }
  }
}

let tries = 0
