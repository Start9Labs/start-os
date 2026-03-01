import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { DevicesUciService } from './uci/service'
import {
  DataUsagePeriod,
  DataUsagePoint,
  Device,
  DeviceStatus,
  DeviceTableItem,
  DeviceUpdateData,
} from './utils'

@Injectable()
export class DevicesService extends FormService<Device[]> {
  private readonly uciService = inject(DevicesUciService)

  // Cache of loaded devices
  private devices: Device[] = []

  async load(): Promise<Device[]> {
    this.devices = await this.uciService.get()
    return [...this.devices]
  }

  async store(): Promise<void> {
    // List doesn't have a single store operation
  }

  getDevice(mac: string): Device | undefined {
    return this.devices.find(d => d.mac === mac)
  }

  getDevicesByStatus(status: DeviceStatus): DeviceTableItem[] {
    return this.devices.filter(d => d.status === status)
  }

  // Update device settings via UCI
  update(mac: string, data: DeviceUpdateData) {
    return this.actions.run(async () => {
      await this.uciService.update(mac, data)

      // Update local cache
      this.devices = this.devices.map(d =>
        d.mac === mac
          ? {
              ...d,
              name: data.name,
              ipv4Static: data.ipv4Static,
              ipv4: data.ipv4Static ? data.ipv4 : d.ipv4,
              ipv6Static: data.ipv6Static,
              ipv6: data.ipv6Static ? data.ipv6 : d.ipv6,
            }
          : d,
      )
    })
  }

  // Block device via UCI firewall rule
  block(mac: string) {
    return this.actions.run(
      async () => {
        await this.uciService.block(mac)

        // Update local cache
        this.devices = this.devices.map(d =>
          d.mac === mac
            ? { ...d, status: 'blocked', ipv4Static: false, ipv6Static: false }
            : d,
        )
      },
      { loading: 'Blocking device' },
    )
  }

  // Unblock device via UCI firewall rule removal
  unblock(mac: string) {
    return this.actions.run(
      async () => {
        await this.uciService.unblock(mac)

        // Update local cache - move to offline since we don't know if connected
        this.devices = this.devices.map(d =>
          d.mac === mac ? { ...d, status: 'offline' } : d,
        )
      },
      { loading: 'Unblocking device' },
    )
  }

  // Forget device - remove from UCI config entirely
  forget(mac: string) {
    return this.actions.run(
      async () => {
        await this.uciService.forget(mac)

        // Remove from local cache
        this.devices = this.devices.filter(d => d.mac === mac)
      },
      { loading: 'Forgetting device' },
    )
  }

  // Get historical data usage for a device
  async getDataUsage(
    mac: string,
    period: DataUsagePeriod,
  ): Promise<DataUsagePoint[]> {
    return this.uciService.getDataUsage(mac, period)
  }
}
