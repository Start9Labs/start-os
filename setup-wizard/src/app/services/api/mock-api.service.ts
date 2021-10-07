import { Injectable } from '@angular/core'
import { pauseFor } from '../state.service'
import { ApiService, SetupEmbassyReq } from './api.service'

let tries = 0

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {

  constructor () {
    super()
  }

  // ** UNENCRYPTED **

  async getStatus () {
    await pauseFor(1000)
    return {
      'product-key': true,
      migrating: false,
    }
  }

  async getDrives () {
    await pauseFor(1000)
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
            used: 200.1255312,
          },
          {
            logicalname: 'sda2',
            label: 'label 2',
            capacity: 50000,
            used: 200.1255312,
          },
        ],
        capacity: 150000,
        'embassy-os': null,
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
        'embassy-os': null,
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
            used: null,
          },
        ],
        capacity: 100000,
        'embassy-os': {
          version: '0.3.3',
        },
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
            used: null,
          },
        ],
        capacity: 10000,
        'embassy-os': {
          version: '0.2.7',
        },
      },
    ]
  }

  async set02XDrive () {
    await pauseFor(1000)
    return
  }

  async getRecoveryStatus () {
    tries = Math.min(tries + 1, 4)
    return {
      'bytes-transferred': tries,
      'total-bytes': 4,
    }
  }

  // ** ENCRYPTED **

  async verifyProductKey () {
    await pauseFor(1000)
    return
  }

  async verify03XPassword (logicalname: string, password: string) {
    await pauseFor(2000)
    return password.length > 8
  }

  async setupEmbassy (setupInfo: SetupEmbassyReq) {
    await pauseFor(3000)
    return 'asdfasdfasdf.onion'
  }

  async getRecoveryDrives () {
    await pauseFor(2000)
    return [
      {
        logicalname: 'Name1',
        version: '0.3.3',
        name: 'My Embassy',
      },
      {
        logicalname: 'Name2',
        version: '0.2.7',
        name: 'My Embassy',
      },
    ]
  }
}
