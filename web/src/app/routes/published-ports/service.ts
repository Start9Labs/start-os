import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { PublishedPortsUciService } from './uci/service'
import {
  PublishedPort,
  PublishedPortDisplay,
  PublishedPortStatus,
} from './types'
import { DevicesUciService } from 'src/app/routes/devices/uci/service'
import { Device, DeviceUpdateData } from 'src/app/routes/devices/utils'

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
   * Reserve static IP addresses for a device
   */
  async reserveDeviceIps(
    mac: string,
    reserveIpv4: boolean,
    reserveIpv6: boolean,
  ): Promise<void> {
    const device = this.getDevice(mac)
    if (!device) return

    // Build update data with current device values
    const updates: DeviceUpdateData = {
      name: device.name,
      ipv4Static: reserveIpv4 ? true : device.ipv4Static,
      ipv4: device.ipv4 || '',
      ipv6Static: reserveIpv6 ? true : device.ipv6Static,
      ipv6: device.ipv6 || '',
    }

    await this.devicesUci.update(mac, updates)

    // Update local device cache
    if (reserveIpv4) device.ipv4Static = true
    if (reserveIpv6) device.ipv6Static = true
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
}
