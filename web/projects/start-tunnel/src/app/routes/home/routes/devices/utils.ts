import { Signal } from '@angular/core'
import { AbstractControl } from '@angular/forms'
import { utils } from '@start9labs/start-sdk'
import { IpNet } from '@start9labs/start-sdk/util'
import { WgServer } from 'src/app/services/patch-db/data-model'

export interface MappedDevice {
  readonly subnet: {
    readonly name: string
    readonly range: string
  }
  readonly ip: string
  readonly name: string
}

export interface MappedSubnet {
  readonly range: string
  readonly name: string
  readonly clients: WgServer['subnets']['']['clients']
}

export interface DeviceData {
  readonly subnets: Signal<readonly MappedSubnet[]>
  readonly device?: MappedDevice
}

export function subnetValidator({ value }: AbstractControl<MappedSubnet>) {
  return value && getIp(value) ? null : { noHosts: 'No hosts available' }
}

export function getIp({ clients, range }: MappedSubnet) {
  const { prefix, octets } = new IpNet(range)
  const used = Object.keys(clients).map(ip =>
    new utils.IpAddress(ip).octets.at(3),
  )

  for (let i = 2; i < totalHosts(prefix); i++) {
    if (!used.includes(i)) {
      return [...octets.slice(0, 3), i].join('.')
    }
  }

  return ''
}

function totalHosts(prefix: number) {
  // Handle special cases per RFC 3021
  if (prefix === 31) return 4 // point-to-point, 2 usable addresses
  if (prefix === 32) return 3 // single host, 1 usable address

  return Math.pow(2, 32 - prefix)
}
