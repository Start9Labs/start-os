import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { PublishedPortsUciService } from './uci/service'
import {
  PublishedPort,
  PublishedPortDisplay,
  PublishedPortStatus,
} from './types'
import { DevicesUciService } from 'src/app/routes/home/routes/devices/uci/service'
import { Device } from 'src/app/routes/home/routes/devices/utils'

@Injectable()
export class PublishedPortsService extends FormService<PublishedPortDisplay[]> {
  private readonly uci = inject(PublishedPortsUciService)
  private readonly devicesUci = inject(DevicesUciService)

  private devices: Device[] = []
  private ports: PublishedPort[] = []

  async load(): Promise<PublishedPortDisplay[]> {
    // Load devices and ports in parallel
    const [devices, ports] = await Promise.all([
      this.devicesUci.get(),
      this.uci.get(),
    ])

    this.devices = devices
    this.ports = ports

    return this.enrichPorts(ports, devices)
  }

  async store(items: PublishedPortDisplay[]): Promise<void> {
    // Extract just the PublishedPort data (without display fields)
    const ports: PublishedPort[] = items.map(item => ({
      id: item.id,
      enabled: item.enabled,
      label: item.label,
      deviceMac: item.deviceMac,
      ports: item.ports,
      protocol: item.protocol,
      ipv4: item.ipv4,
      ipv6: item.ipv6,
      ipv4PublicPort: item.ipv4PublicPort,
      source: item.source,
    }))

    await this.uci.set(ports)
    this.ports = ports
  }

  getDevices(): Device[] {
    return this.devices
  }

  getDevice(mac: string): Device | undefined {
    return this.devices.find(d => d.mac.toUpperCase() === mac.toUpperCase())
  }

  /**
   * Check if a device has any published ports
   */
  deviceHasPublishedPorts(mac: string): boolean {
    return this.ports.some(p => p.deviceMac.toUpperCase() === mac.toUpperCase())
  }

  private enrichPorts(
    ports: PublishedPort[],
    devices: Device[],
  ): PublishedPortDisplay[] {
    return ports.map(port => {
      const device = devices.find(
        d => d.mac.toUpperCase() === port.deviceMac.toUpperCase(),
      )
      const status = this.computeStatus(port, device)

      return {
        ...port,
        status: status.status,
        statusReason: status.reason,
        deviceName: device?.name || device?.hostname || 'Unknown Device',
        deviceIpv4: device?.ipv4,
        deviceIpv6: device?.ipv6,
        endpointIpv4: this.computeEndpointIpv4(port, device),
        endpointIpv6: this.computeEndpointIpv6(port, device),
      }
    })
  }

  private computeStatus(
    port: PublishedPort,
    device?: Device,
  ): { status: PublishedPortStatus; reason?: string } {
    if (!port.enabled) {
      return { status: 'disabled' }
    }

    if (!device) {
      return { status: 'paused', reason: 'Device not found' }
    }

    if (device.status === 'offline') {
      return { status: 'paused', reason: 'Device offline' }
    }

    if (device.status === 'blocked') {
      return { status: 'paused', reason: 'Device blocked' }
    }

    // Check if IPv4 is requested but unavailable (CGNAT, etc.)
    if (port.ipv4 && !device.ipv4) {
      if (port.ipv6 && device.ipv6) {
        return { status: 'partial', reason: 'IPv4 unavailable' }
      }
      return { status: 'error', reason: 'No addresses available' }
    }

    if (port.ipv6 && !device.ipv6) {
      if (port.ipv4 && device.ipv4) {
        return { status: 'partial', reason: 'IPv6 unavailable' }
      }
      return { status: 'error', reason: 'No addresses available' }
    }

    return { status: 'active' }
  }

  private computeEndpointIpv4(
    port: PublishedPort,
    device?: Device,
  ): string | undefined {
    if (!port.ipv4 || !device?.ipv4) return undefined

    const publicPort = port.ipv4PublicPort || port.ports
    // TODO: Get actual DDNS hostname or WAN IP
    return `example.ddns.net:${publicPort}`
  }

  private computeEndpointIpv6(
    port: PublishedPort,
    device?: Device,
  ): string | undefined {
    if (!port.ipv6 || !device?.ipv6) return undefined

    return `[${device.ipv6}]:${port.ports}`
  }
}
