import { Signal } from '@angular/core'

export interface MappedDevice {
  readonly ip: string
  readonly name: string
}

export interface MappedForward {
  readonly externalip: string
  readonly externalport: string
  readonly device: MappedDevice
  readonly internalport: string
  readonly label: string
  readonly enabled: boolean
}

export interface PortForwardsData {
  readonly ips: Signal<any>
  readonly devices: Signal<readonly MappedDevice[]>
}
