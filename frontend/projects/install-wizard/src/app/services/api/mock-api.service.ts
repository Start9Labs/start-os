import { Injectable } from '@angular/core'
import { pauseFor } from '@start9labs/shared'
import { ApiService, GetDisksRes, InstallReq } from './api.service'

@Injectable()
export class MockApiService implements ApiService {
  async getDisks(): Promise<GetDisksRes> {
    await pauseFor(1000)
    return [
      {
        logicalname: 'abcdefgh',
        'embassy-data': false,
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
