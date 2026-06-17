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
  readonly label: T.Tunnel.SniRoute['label']
  readonly enabled: T.Tunnel.SniRoute['enabled']
  readonly sni: string | null
  readonly hostname: string | null
}

export interface PortForwardsData {
  readonly ips: Signal<readonly string[]>
  readonly devices: Signal<readonly MappedDevice[]>
}

export function mapForwards(
  portForwards: T.Tunnel.PortForwards,
  devices: readonly MappedDevice[],
): MappedForward[] {
  return Object.entries(portForwards).flatMap(([source, forward]) =>
    forward.kind === 'sni'
      ? Object.entries(forward.routes).map(([hostname, route]) =>
          toRow(source, route.target, route.label, route.enabled, hostname),
        )
      : [toRow(source, forward.target, forward.label, forward.enabled, null)],
  )

  function toRow(
    source: string,
    target: string,
    label: string | null,
    enabled: boolean,
    hostname: string | null,
  ): MappedForward {
    const [externalip, externalport] = source.split(':')
    const [targetip, internalport] = target.split(':')

    return {
      externalip: externalip!,
      externalport: externalport!,
      // Fall back to the raw target IP when it isn't a named device (e.g. a
      // manual SNI route to a non-client) so the row still renders rather than
      // crashing the whole table on an undefined device.
      device: devices.find(d => d.ip === targetip) ?? {
        ip: targetip!,
        name: targetip!,
      },
      internalport: internalport!,
      label,
      enabled,
      sni: hostname,
      hostname,
    }
  }
}
