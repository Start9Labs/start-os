import { Injectable } from '@angular/core'
import { pauseFor } from 'src/app/util/misc.util'
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
      'root-ca': rootCA,
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

const rootCA = 'LS0tLS1CRUdJTiBDRVJUSUZJQ0FURS0tLS0tCk1JSURwekNDQW8rZ0F3SUJBZ0lSQUlJdU9hcmxRRVRsVVFFT1pKR1pZZEl3RFFZSktvWklodmNOQVFFTEJRQXcKYlRFTE1Ba0dBMVVFQmhNQ1ZWTXhGVEFUQmdOVkJBb01ERVY0WVcxd2JHVWdRMjl5Y0RFT01Bd0dBMVVFQ3d3RgpVMkZzWlhNeEN6QUpCZ05WQkFnTUFsZEJNUmd3RmdZRFZRUUREQTkzZDNjdVpYaGhiWEJzWlM1amIyMHhFREFPCkJnTlZCQWNNQjFObFlYUjBiR1V3SGhjTk1qRXdNekE0TVRVME5qSTNXaGNOTWpJd016QTRNVFkwTmpJM1dqQnQKTVFzd0NRWURWUVFHRXdKVlV6RVZNQk1HQTFVRUNnd01SWGhoYlhCc1pTQkRiM0p3TVE0d0RBWURWUVFMREFWVApZV3hsY3pFTE1Ba0dBMVVFQ0F3Q1YwRXhHREFXQmdOVkJBTU1EM2QzZHk1bGVHRnRjR3hsTG1OdmJURVFNQTRHCkExVUVCd3dIVTJWaGRIUnNaVENDQVNJd0RRWUpLb1pJaHZjTkFRRUJCUUFEZ2dFUEFEQ0NBUW9DZ2dFQkFNUDcKdDVBS0ZaUTdhYnFrZXlVanNCVklXUmE5dENoOG9nZTl1L0x2Q2J4VTczOEc0anNzVCtPdWQzV01haklqdU5vdwpjcGMrMFEvZTQyVUxPLzZnVE5yVHM2T0NPbzlsVjZHMERwcmYvZTkxRFdvS2dQYXRlbS9wVWpOeXJhaWZIWmZ1CmI1bUxIQ2ZhaGpXWFVRdGMvc2ptRFFhWlJLM0thcjZsamxVQkUvTGU5TkV5T0FJa1NMUHpEdFc4TFhtNGl3Y1UKQlpyYjgyOHJLZDFBdzlvSTErM2JmekI2eFhtelp4YzVSTFh2ZU9DRWhLR0QzMmpLWi9STkZTQzhBWkF3SmUreApiVHN5cy9sVU9ZRlR1VDhCbjBUR3hSOHg3WTRINzUrRjlCYXZZM3YrV2tMajRNK29sTjlkTVI3RXQ5Rk10NHU0CllSb2t2NXpwOHpJYjVpVG5lMWtDQXdFQUFhTkNNRUF3RHdZRFZSMFRBUUgvQkFVd0F3RUIvekFkQmdOVkhRNEUKRmdRVWFXMytyMzI4dVRMb2tvZzJUa2xtb0JLK3l0NHdEZ1lEVlIwUEFRSC9CQVFEQWdHR01BMEdDU3FHU0liMwpEUUVCQ3dVQUE0SUJBUUFYamQvN1VaOFJERStQTFdTRE5HUWRMZW1PQlRjYXdGK3RLK1B6QTRFdmxtbjlWdU5jCmcreDNvWnZWWlNEUUJBTlV6MGI5b1BlbzU0YUUzOGRXMXpRbTJxZlRhYjg4MjJhcWVXTUx5SjFkTXNBZ3FZWDIKdDkrdTZ3M056UkN3OFB2ejE4VjY5K2RGRTVBZVhtTlAwWjUvZ2R6OEgvTlNwY3RqbHpvcGJTY1JaS0NTbFBpZApSZjNaT1BtOVFQOTJZcFd5WURrZkFVMDR4ZERvMXZSME1ZaktQa2w0TGpScVNVL3RjQ0puUE1iSml3cStiV3BYCjJXSm9FQlhCL3AxNUtuNkp4akkwemUyU25TSTQ4Slo4aXQ0ZnZ4cmhPbzBWb0xOSXVDdU5YSk93VTE3UmRsMVcKWUppZGFxN2plNmsxOEFkZ1BBMEtoOHkxWHRmVUgzZlRhVnc0Ci0tLS0tRU5EIENFUlRJRklDQVRFLS0tLS0='
