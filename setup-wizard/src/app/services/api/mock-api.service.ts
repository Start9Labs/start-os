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
      'product-key': false,
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
            'embassy-os': null,
          },
          {
            logicalname: 'sda2',
            label: 'label 2',
            capacity: 50000,
            used: 200.1255312,
            'embassy-os': null,
          },
        ],
        capacity: 150000,
      },
      {
        vendor: 'Vendor',
        model: 'Model',
        logicalname: 'dev/sdb',
        partitions: [],
        capacity: 1600.01234,
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
            'embassy-os': {
              version: '0.3.3',
              full: true,
              'password-hash': 'asdfasdfasdf',
            },
          },
          {
            logicalname: 'sdc1MOCKTESTER',
            label: 'label 1',
            capacity: null,
            used: null,
            'embassy-os': {
              version: '0.3.6',
              full: true,
              'password-hash': '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
            },
          },
          {
            logicalname: 'sdc1',
            label: 'label 1',
            capacity: null,
            used: null,
            'embassy-os': {
              version: '0.3.3',
              full: false,
              'password-hash': 'asdfasdfasdf',
            },
          },
        ],
        capacity: 100000,
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
            'embassy-os': {
              version: '0.2.7',
              full: true,
              'password-hash': 'asdfasdfasdf',
            },
          },
        ],
        capacity: 10000,
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
    return {
      'tor-address': 'asdfasdfasdf.onion',
      'lan-address': 'embassy-dfasdf.local',
    }
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
