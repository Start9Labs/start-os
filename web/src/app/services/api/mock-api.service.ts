import { Injectable } from '@angular/core'
import { pauseFor } from '../../utils/pauseFor'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  LoginReq,
  SetUciReq,
  SetUciRes,
} from './api.service'
import { UciFile, UciSection } from './types'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  async login(params: LoginReq): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async logout(): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async exec(params: ExecReq): Promise<ExecRes> {
    await pauseFor(1000)

    return {
      exitCode: 0,
      stdout: 'success',
      stderr: '',
    }
  }

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(1000)

    return {
      modified: new Date().toISOString(),
      contents: '',
    }
  }

  async setFile(params: GetFileRes): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async getUci<T extends Record<string, UciFile<any>>>(
    params: GetUciReq,
  ): Promise<T> {
    await pauseFor(1000)

    return params.names.reduce(
      (obj, name) => ({
        ...obj,
        [name]: mockUci[name],
      }),
      {} as T,
    )
  }

  async setUci<T extends string[]>(params: SetUciReq): Promise<SetUciRes<T>> {
    await pauseFor(1000)

    const isoString = new Date().toISOString()

    return Object.keys(params).reduce(
      (obj, name) => ({
        ...obj,
        [name]: isoString,
      }),
      {} as SetUciRes<T>,
    )
  }
}

export const mockUci: Record<string, UciFile<UciSection>> = {
  network: {
    sections: [
      {
        type: 'interface',
        name: 'wan',
        options: {
          proto: 'dhcp',
          ipaddr: '100.65.227.234',
          netmask: '255.255.255.252',
          gateway: '100.65.227.233',
          device: 'eth0.2',
        },
        lists: {},
      },
    ],
    modified: '2025-11-15T10:30:00Z',
  },
  dhcp: {
    sections: [
      {
        type: 'dnsmasq',
        name: 'cfg01411c',
        options: {},
        lists: {
          server: [], // Empty means using ISP DNS
        },
      },
    ],
    modified: '2025-11-15T10:30:00Z',
  },

  'https-dns-proxy': {
    sections: [], // Empty means not using DNS over TLS
    modified: '2025-11-15T10:30:00Z',
  },
}
