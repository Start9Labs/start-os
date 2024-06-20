import { Injectable } from '@angular/core'
import {
  DiskListResponse,
  StartOSDiskInfo,
  encodeBase64,
  pauseFor,
} from '@start9labs/shared'
import { ApiService } from './api.service'
import * as jose from 'node-jose'
import { T } from '@start9labs/start-sdk'
import {
  Observable,
  concatMap,
  delay,
  from,
  interval,
  map,
  mergeScan,
  of,
  startWith,
  switchMap,
  switchScan,
  takeWhile,
} from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  // fullProgress$(): Observable<T.FullProgress> {
  //   const phases = [
  //     {
  //       name: 'Preparing Data',
  //       progress: null,
  //     },
  //     {
  //       name: 'Transferring Data',
  //       progress: null,
  //     },
  //     {
  //       name: 'Finalizing Setup',
  //       progress: null,
  //     },
  //   ]

  //   return from(phases).pipe(
  //     switchScan((acc, val, i) => {}, { overall: null, phases }),
  //   )
  // }

  // namedProgress$(namedProgress: T.NamedProgress): Observable<T.NamedProgress> {
  //   return of(namedProgress).pipe(startWith(namedProgress))
  // }

  // progress$(progress: T.Progress): Observable<T.Progress> {}

  // websocket

  openProgressWebsocket$(guid: string): Observable<T.FullProgress> {
    return of(PROGRESS)
    // const numPhases = PROGRESS.phases.length

    // return of(PROGRESS).pipe(
    //   switchMap(full =>
    //     from(PROGRESS.phases).pipe(
    //       mergeScan((full, phase, i) => {
    //         if (
    //           !phase.progress ||
    //           typeof phase.progress !== 'object' ||
    //           !phase.progress.total
    //         ) {
    //           full.phases[i].progress = true

    //           if (
    //             full.overall &&
    //             typeof full.overall === 'object' &&
    //             full.overall.total
    //           ) {
    //             const step = full.overall.total / numPhases
    //             full.overall.done += step
    //           }

    //           return of(full).pipe(delay(2000))
    //         } else {
    //           const total = phase.progress.total
    //           const step = total / 4
    //           let done = phase.progress.done

    //           return interval(1000).pipe(
    //             takeWhile(() => done < total),
    //             map(() => {
    //               done += step

    //               console.error(done)

    //               if (
    //                 full.overall &&
    //                 typeof full.overall === 'object' &&
    //                 full.overall.total
    //               ) {
    //                 const step = full.overall.total / numPhases / 4

    //                 full.overall.done += step
    //               }

    //               if (done === total) {
    //                 full.phases[i].progress = true

    //                 if (i === numPhases - 1) {
    //                   full.overall = true
    //                 }
    //               }
    //               return full
    //             }),
    //           )
    //         }
    //       }, full),
    //     ),
    //   ),
    // )
  }

  private statusIndex = 0
  async getStatus(): Promise<T.SetupStatusRes | null> {
    await pauseFor(1000)

    this.statusIndex++

    switch (this.statusIndex) {
      case 2:
        return {
          status: 'running',
          progress: PROGRESS,
          guid: 'progress-guid',
        }
      case 3:
        return {
          status: 'complete',
          torAddress: 'https://asdafsadasdasasdasdfasdfasdf.onion',
          lanAddress: 'https://adjective-noun.local',
          rootCa: encodeBase64(rootCA),
        }
      default:
        return null
    }
  }

  async getPubKey(): Promise<void> {
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

  async getDrives(): Promise<DiskListResponse> {
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
            startOs: {
              version: '0.2.17',
              full: true,
              passwordHash:
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              wrappedKey: null,
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
            startOs: {
              version: '0.3.3',
              full: true,
              passwordHash:
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              wrappedKey: null,
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
            startOs: {
              version: '0.3.2',
              full: true,
              passwordHash:
                '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
              wrappedKey: null,
            },
            guid: 'guid-guid-guid-guid',
          },
        ],
        capacity: 1000190509056,
        guid: null,
      },
    ]
  }

  async verifyCifs(params: T.VerifyCifsParams): Promise<StartOSDiskInfo> {
    await pauseFor(1000)
    return {
      version: '0.3.0',
      full: true,
      passwordHash:
        '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
      wrappedKey: '',
    }
  }

  async attach(params: T.AttachParams): Promise<T.SetupProgress> {
    await pauseFor(1000)

    return {
      progress: PROGRESS,
      guid: 'progress-guid',
    }
  }

  async execute(setupInfo: T.SetupExecuteParams): Promise<T.SetupProgress> {
    await pauseFor(1000)

    return {
      progress: PROGRESS,
      guid: 'progress-guid',
    }
  }

  async complete(): Promise<T.SetupResult> {
    await pauseFor(1000)
    return {
      torAddress: 'https://asdafsadasdasasdasdfasdfasdf.onion',
      lanAddress: 'https://adjective-noun.local',
      rootCa: encodeBase64(rootCA),
    }
  }

  async exit(): Promise<void> {
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

const PROGRESS = {
  overall: null,
  phases: [],
}
