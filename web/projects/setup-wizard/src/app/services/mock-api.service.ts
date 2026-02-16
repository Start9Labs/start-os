import { Injectable } from '@angular/core'
import {
  DiskInfo,
  encodeBase64,
  FullKeyboard,
  pauseFor,
  SetLanguageParams,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import * as jose from 'node-jose'
import { interval, map, Observable } from 'rxjs'
import { ApiService } from './api.service'
import {
  SetupStatusRes,
  InstallOsParams,
  InstallOsRes,
  AttachParams,
  SetupExecuteParams,
  SetupCompleteRes,
  EchoReq,
} from '../types'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  private statusIndex = 0
  private installCompleted = false

  openWebsocket$<T>(guid: string): Observable<T> {
    if (guid === 'logs-guid') {
      return interval(500).pipe(
        map(() => ({
          timestamp: new Date().toISOString(),
          message: 'fake log entry',
          bootId: 'boot-id',
        })),
      ) as Observable<T>
    } else if (guid === 'progress-guid') {
      return interval(1000).pipe(
        map(() => ({
          overall: true,
          phases: [
            { name: 'Preparing Data', progress: true },
            { name: 'Transferring Data', progress: true },
            { name: 'Finalizing Setup', progress: true },
          ],
        })),
      ) as Observable<T>
    } else {
      throw new Error('invalid guid type')
    }
  }

  async echo(params: EchoReq, url: string): Promise<string> {
    if (url) {
      const num = Math.floor(Math.random() * 10) + 1
      if (num > 8) return params.message
      throw new Error()
    }
    await pauseFor(500)
    return params.message
  }

  async getStatus(): Promise<SetupStatusRes> {
    await pauseFor(500)

    this.statusIndex++

    if (this.statusIndex === 1) {
      return { status: 'needs-install', keyboard: null }
      // return {
      //   status: 'incomplete',
      //   attach: false,
      //   guid: 'mock-data-guid',
      //   keyboard: null,
      // }
    }

    if (this.statusIndex > 3) {
      return { status: 'complete' }
    }

    return {
      status: 'running',
      progress: PROGRESS,
      guid: 'progress-guid',
    }
  }

  async getPubKey(): Promise<void> {
    await pauseFor(300)
    this.pubkey = await jose.JWK.asKey({
      kty: 'EC',
      crv: 'P-256',
      x: 'yHTDYSfjU809fkSv9MmN4wuojf5c3cnD7ZDN13n-jz4',
      y: '8Mpkn744A5KDag0DmX2YivB63srjbugYZzWc3JOpQXI',
    })
  }

  async setKeyboard(_params: FullKeyboard): Promise<null> {
    await pauseFor(300)
    return null
  }

  async setLanguage(params: SetLanguageParams): Promise<null> {
    await pauseFor(300)
    return null
  }

  async getDisks(): Promise<DiskInfo[]> {
    await pauseFor(500)
    return MOCK_DISKS
  }

  async installOs(params: InstallOsParams): Promise<InstallOsRes> {
    await pauseFor(2000)
    this.installCompleted = true
    return {
      guid: 'mock-data-guid',
      attach: !params.dataDrive.wipe,
    }
  }

  async verifyCifs(
    params: T.VerifyCifsParams,
  ): Promise<Record<string, StartOSDiskInfo>> {
    await pauseFor(1000)
    return {
      '9876-5432-1234-5678': {
        hostname: 'adjective-noun',
        version: '0.3.6',
        timestamp: new Date().toISOString(),
        passwordHash:
          '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
        wrappedKey: '',
      },
      '9876-5432-1234-5671': {
        hostname: 'adjective-noun',
        version: '0.3.6',
        timestamp: new Date().toISOString(),
        passwordHash:
          '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
        wrappedKey: '',
      },
    }
  }

  async attach(params: AttachParams): Promise<T.SetupProgress> {
    await pauseFor(1000)
    this.statusIndex = 1 // Jump to running state
    return {
      progress: PROGRESS,
      guid: 'progress-guid',
    }
  }

  async execute(params: SetupExecuteParams): Promise<T.SetupProgress> {
    await pauseFor(1000)
    this.statusIndex = 1 // Jump to running state
    return {
      progress: PROGRESS,
      guid: 'progress-guid',
    }
  }

  async initFollowLogs(): Promise<T.LogFollowResponse> {
    await pauseFor(500)
    return {
      startCursor: 'fakestartcursor',
      guid: 'logs-guid',
    }
  }

  async complete(): Promise<SetupCompleteRes> {
    await pauseFor(500)
    return {
      hostname: 'adjective-noun',
      rootCa: encodeBase64(ROOT_CA),
      needsRestart: this.installCompleted,
    }
  }

  async exit(): Promise<void> {
    await pauseFor(500)
  }

  async shutdown(): Promise<void> {
    await pauseFor(500)
  }

  async restart(): Promise<void> {
    await pauseFor(500)
  }
}

const MOCK_DISKS: DiskInfo[] = [
  {
    logicalname: '/dev/sda',
    vendor: 'Samsung',
    model: 'SSD 970 EVO Plus',
    partitions: [
      {
        logicalname: '/dev/sda1',
        label: null,
        capacity: 500000000000,
        used: null,
        startOs: {},
        guid: null,
      },
    ],
    capacity: 500000000000,
    guid: null,
  },
  {
    logicalname: '/dev/sdb',
    vendor: 'Crucial',
    model: 'MX500',
    partitions: [
      {
        logicalname: '/dev/sdb1',
        label: null,
        capacity: 1000000000000,
        used: null,
        startOs: {
          '1234-5678-9876-5432': {
            hostname: 'existing-server',
            version: '0.3.6',
            timestamp: new Date().toISOString(),
            passwordHash:
              '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
            wrappedKey: null,
          },
        },
        guid: 'existing-guid',
      },
    ],
    capacity: 1000000000000,
    guid: 'existing-guid',
  },
  {
    logicalname: '/dev/sdc',
    vendor: 'WD',
    model: 'Blue SN570',
    partitions: [
      {
        logicalname: '/dev/sdc1',
        label: 'Backup',
        capacity: 2000000000000,
        used: 500000000000,
        startOs: {
          'backup-server-id': {
            hostname: 'backup-server',
            version: '0.3.5',
            timestamp: new Date(Date.now() - 86400000).toISOString(),
            passwordHash:
              '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
            wrappedKey: '',
          },
        },
        guid: null,
      },
    ],
    capacity: 2000000000000,
    guid: null,
  },
]

const PROGRESS: T.FullProgress = {
  overall: null,
  phases: [],
}

const ROOT_CA = `-----BEGIN CERTIFICATE-----
MIIDpzCCAo+gAwIBAgIRAIIuOarlQETlUQEOZJGZYdIwDQYJKoZIhvcNAQELBQAw
bTELMAkGA1UEBhMCVVMxFTATBgNVBAoMDEV4YW1wbGUgQ29ycDEOMAwGA1UECwwF
U2FsZXMxCzAJBgNVBAgMAldBMRgwFgYDVQQDDA93d3cuZXhhbXBsZS5jb20xEDAO
BgNVBAcMB1NlYXR0bGUwHhcNMjEwMzA4MTU0NjI3WhcNMjIwMzA4MTY0NjI3WjBt
MQswCQYDVQQGEwJVUzEVMBMGA1UECgwMRXhhbXBsZSBDb3JwMQ4wDAYDVQQLDAVT
YWxlczELMAkGA1UECAwCV0ExGDAWBgNVBAMMD3d3dy5leGFtcGxlLmNvbTEQMA4G
A1UEBwwHU2VhdHRsZTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMP7
t5AKFZQ7abqkeyUjsBVIWRa9tCh8oge9u/LvCbxU738G4jssT+Oud3WMajIjuNow
cpc+0Q/e42ULO/6gTNrTs6OCOo9lV6G0Dprf/e91DWoKgPatem/pUjNyraifHZfu
b5mLHCfahjWXUQtc/sjmDQaZRK3Kar6ljlUBE/Le9NEyOAIkSLPzDtW8LXm4iwcU
BZrb828rKd1Aw9oI1+3bfzB6xXmzZxc5RLXveOCEhKGD32jKZ/RNFSC8AZAwJe+x
bTsys/lUOYFTuT8Bn0TGxR8x7Y4H75+F9BavY3v+WkLj4M+olN9dMR7Et9FMt4u4
YRokv5zp8zIb5iTne1kCAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4E
FgQUaW3+r328uTLokog2TklmoBK+yt4wDgYDVR0PAQH/BAQDAgGGMA0GCSqGSIb3
DQEBCwUAA4IBAQAXjd/7UZ8RDE+PLWSDNGQdLemOBTcawF+tK+PzA4Evlmn9VuNc
g+x3oZvVZSDQBANUz0b9oPeo54aE38dW1zQm2qfTab8822aqeWMLyJ1dMsAgqYX2
t9+u6w3NzRCw8Pvz18V69+dFE5AeXmNP0Z5/gdz8H/NSpctjlzopbScRZKCSlPid
Rf3ZOPm9QP92YpWyYDkfAU04xdDo1vR0MYjKPkl4LjRqSU/tcCJnPMbJiwq+bWpX
2WJoEBXB/p15Kn6JxjI0ze2SnSI48JZ8it4fvxrhOo0VoLNIuCuNXJOwU17Rdl1W
YJidaq7je6k18AdgPA0Kh8y1XtfUH3fTaVw4
-----END CERTIFICATE-----`
