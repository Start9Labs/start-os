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
        guid: 'theguid',
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
        capacity: 34359738369,
        guid: null,
      },
      {
        vendor: 'Vendor',
        model: 'Model',
        logicalname: 'dev/sdc',
        guid: null,
        partitions: [
          {
            logicalname: 'sdc1',
            label: 'label 1',
            capacity: 0,
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
            capacity: 0,
            used: null,
            'embassy-os': {
              version: '0.3.6',
              full: true,
              // password is 'asdfasdf'
              'password-hash': '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
            },
          },
          {
            logicalname: 'sdc1',
            label: 'label 1',
            capacity: 0,
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
        guid: null,
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

  async importDrive (guid: string) {
    await pauseFor(3000)
    return setupRes
  }

  async setupEmbassy (setupInfo: SetupEmbassyReq) {
    await pauseFor(3000)
    return setupRes
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

const rootCA =
`-----BEGIN CERTIFICATE-----
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

const setupRes = {
  'tor-address': 'http://asdafsadasdasasdasdfasdfasdf.onion',
  'lan-address': 'https://embassy-abcdefgh.local',
  'root-ca': btoa(rootCA),
}
