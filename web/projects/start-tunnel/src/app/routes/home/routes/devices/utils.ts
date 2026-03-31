import { Signal } from '@angular/core'
import { AbstractControl } from '@angular/forms'
import { T, utils } from '@start9labs/start-sdk'
import { IpNet } from '@start9labs/start-sdk/util'

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
  readonly clients: T.Tunnel.WgSubnetClients
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
    const zero = ipnet.zero().cmp(ip)
    const broadcast = ipnet.broadcast().cmp(ip)
    return zero + broadcast === 0
      ? null
      : zero === 0
        ? { isZeroAddr: `Address cannot be the zero address` }
        : broadcast === 0
          ? { isBroadcastAddress: `Address cannot be the broadcast address` }
          : { notInSubnet: `Address is not part of ${subnet}` }
  }
}

export function getIp({ clients, range }: MappedSubnet) {
  const net = IpNet.parse(range)
  const last = net.broadcast()

  for (let ip = net.add(1); ip.cmp(last) === -1; ip = ip.add(1)) {
    if (!clients[ip.address]) {
      return ip.address
    }
  }

  return ''
}
