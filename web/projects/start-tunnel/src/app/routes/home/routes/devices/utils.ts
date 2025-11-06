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
  return !value?.clients || getIp(value)
    ? null
    : { noHosts: 'No hosts available' }
}

export const ipInSubnetValidator = (subnet: string | null = null) => {
  const ipnet = subnet && utils.IpNet.parse(subnet)
  return ({ value }: AbstractControl<string>) => {
    let ip: utils.IpAddress
    try {
      ip = utils.IpAddress.parse(value)
    } catch (e) {
      return { invalidIp: 'Not a valid IP Address' }
    }
    if (!ipnet) return null
    return ipnet.zero().cmp(ip) === -1 && ipnet.broadcast().cmp(ip) === 1
      ? null
      : { notInSubnet: `Address is not part of ${subnet}` }
  }
}

export function getIp({ clients, range }: MappedSubnet) {
  const net = IpNet.parse(range)
  const last = net.broadcast()

  for (let ip = net.add(1); ip.cmp(last) === -1; ip.add(1)) {
    if (!clients[ip.address]) {
      return ip.address
    }
  }

  return ''
}
