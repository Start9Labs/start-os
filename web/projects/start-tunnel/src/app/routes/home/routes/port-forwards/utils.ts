import { Signal } from '@angular/core'
import { T } from '@start9labs/start-sdk'

export interface MappedDevice {
  readonly ip: string
  readonly name: string
}

export interface MappedForward {
  readonly externalip: string
  readonly externalport: string
  readonly device: MappedDevice
  readonly internalport: string
  readonly label: T.Tunnel.PortForwardEntry['label']
  readonly enabled: T.Tunnel.PortForwardEntry['enabled']
}

export interface PortForwardsData {
  readonly ips: Signal<any>
  readonly devices: Signal<readonly MappedDevice[]>
}
