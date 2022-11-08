import { Injectable } from '@angular/core'
import { pauseFor } from '@start9labs/shared'
import { ApiService, GetDisksRes, InstallReq } from './api.service'

@Injectable()
export class MockApiService implements ApiService {
  async getDisks(): Promise<GetDisksRes> {
    await pauseFor(500)
    return [
      {
        logicalname: 'abcd',
        vendor: 'Samsung',
        model: 'T5',
        partitions: [
          {
            logicalname: 'pabcd',
            label: null,
            capacity: 73264762332,
            used: null,
            'embassy-os': {
              version: '0.2.17',
              full: true,
              'password-hash':
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              'wrapped-key': null,
            },
          },
        ],
        capacity: 123456789123,
        guid: 'uuid-uuid-uuid-uuid',
      },
      {
        logicalname: 'dcba',
        vendor: 'Crucial',
        model: 'MX500',
        partitions: [
          {
            logicalname: 'pbcba',
            label: null,
            capacity: 73264762332,
            used: null,
            'embassy-os': {
              version: '0.3.1',
              full: true,
              'password-hash':
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              'wrapped-key': null,
            },
          },
        ],
        capacity: 123456789123,
        guid: null,
      },
    ]
  }

  async install(params: InstallReq): Promise<void> {
    await pauseFor(1000)
  }

  async reboot(): Promise<void> {
    await pauseFor(1000)
  }
}
