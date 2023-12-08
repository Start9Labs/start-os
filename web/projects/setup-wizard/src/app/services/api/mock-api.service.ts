import { Injectable } from '@angular/core'
import {
  encodeBase64,
  FollowLogsReq,
  FollowLogsRes,
  getSetupStatusMock,
  Log,
  pauseFor,
} from '@start9labs/shared'
import {
  ApiService,
  AttachReq,
  CifsRecoverySource,
  CompleteRes,
  ExecuteReq,
} from './api.service'
import * as jose from 'node-jose'
import { interval, map, Observable } from 'rxjs'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  async getSetupStatus() {
    return getSetupStatusMock()
  }

  async getPubKey() {
    await pauseFor(1000)

    // randomly generated
    // const keystore = jose.JWK.createKeyStore()
    // this.pubkey = await keystore.generate('EC', 'P-256')

    // generated from backend
    this.pubkey = await jose.JWK.asKey({
      kty: 'EC',
      crv: 'P-256',
      x: 'yHTDYSfjU809fkSv9MmN4wuojf5c3cnD7ZDN13n-jz4',
      y: '8Mpkn744A5KDag0DmX2YivB63srjbugYZzWc3JOpQXI',
    })
  }

  async getDrives() {
    await pauseFor(1000)
    return [
      {
        logicalname: '/dev/nvme0n1p3',
        vendor: 'Unknown Vendor',
        model: 'Samsung SSD - 970 EVO Plus 2TB',
        partitions: [
          {
            logicalname: 'pabcd',
            label: null,
            capacity: 1979120929996,
            used: null,
            'embassy-os': {
              version: '0.2.17',
              full: true,
              'password-hash':
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              'wrapped-key': null,
            },
            guid: null,
          },
        ],
        capacity: 1979120929996,
        guid: 'uuid-uuid-uuid-uuid',
      },
      {
        logicalname: 'dcba',
        vendor: 'CT1000MX',
        model: '500SSD1',
        partitions: [
          {
            logicalname: 'pbcba',
            label: null,
            capacity: 73264762332,
            used: null,
            'embassy-os': {
              version: '0.3.3',
              full: true,
              'password-hash':
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              'wrapped-key': null,
            },
            guid: null,
          },
        ],
        capacity: 1000190509056,
        guid: null,
      },
      {
        logicalname: '/dev/sda',
        vendor: 'ASMT',
        model: '2115',
        partitions: [
          {
            logicalname: 'pbcba',
            label: null,
            capacity: 73264762332,
            used: null,
            'embassy-os': {
              version: '0.3.2',
              full: true,
              'password-hash':
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              'wrapped-key': null,
            },
            guid: 'guid-guid-guid-guid',
          },
        ],
        capacity: 1000190509056,
        guid: null,
      },
    ]
  }

  async verifyCifs(params: CifsRecoverySource) {
    await pauseFor(1000)
    return {
      version: '0.3.0',
      full: true,
      'password-hash':
        '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
      'wrapped-key': '',
    }
  }

  async attach(params: AttachReq) {
    await pauseFor(1000)
  }

  async execute(setupInfo: ExecuteReq) {
    await pauseFor(1000)
  }

  async followServerLogs(params: FollowLogsReq): Promise<FollowLogsRes> {
    await pauseFor(1000)
    return {
      'start-cursor': 'fakestartcursor',
      guid: 'fake-guid',
    }
  }

  openLogsWebsocket$(config: WebSocketSubjectConfig<Log>): Observable<Log> {
    return interval(500).pipe(
      map(() => ({
        timestamp: new Date().toISOString(),
        message: 'fake log entry',
      })),
    )
  }

  async complete(): Promise<CompleteRes> {
    await pauseFor(1000)
    return {
      'tor-address': 'https://asdafsadasdasasdasdfasdfasdf.onion',
      'lan-address': 'https://adjective-noun.local',
      'root-ca': encodeBase64(rootCA),
    }
  }

  async exit() {
    await pauseFor(1000)
  }
}

const rootCA = `-----BEGIN CERTIFICATE-----
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
