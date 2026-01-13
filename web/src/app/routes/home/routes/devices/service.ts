import { inject, Injectable } from '@angular/core'
import { TuiNotificationService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
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
  private readonly alertService = inject(TuiNotificationService)
  private readonly notifications = inject(TuiNotificationMiddleService)
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
  async update(mac: string, data: DeviceUpdateData): Promise<boolean> {
    const saving = this.notifications.open('Saving').subscribe()

    try {
      await this.uciService.update(mac, data)

      // Update local cache
      const index = this.devices.findIndex(d => d.mac === mac)
      if (index !== -1) {
        this.devices[index] = {
          ...this.devices[index],
          name: data.name,
          ipv4Static: data.ipv4Static,
          ipv4: data.ipv4Static ? data.ipv4 : this.devices[index].ipv4,
          ipv6Static: data.ipv6Static,
          ipv6: data.ipv6Static ? data.ipv6 : this.devices[index].ipv6,
        }
      }

      return true
    } catch (e: any) {
      this.alertService.open(e.message, { appearance: 'negative' }).subscribe()
      return false
    } finally {
      saving.unsubscribe()
    }
  }

  // Block device via UCI firewall rule
  async block(mac: string): Promise<boolean> {
    const blocking = this.notifications.open('Blocking device').subscribe()

    try {
      await this.uciService.block(mac)

      // Update local cache
      const device = this.devices.find(d => d.mac === mac)
      if (device) {
        device.status = 'blocked'
        // Clear static IP flags as they're removed on block
        device.ipv4Static = false
        device.ipv6Static = false
      }

      return true
    } catch (e: any) {
      this.alertService.open(e.message, { appearance: 'negative' }).subscribe()
      return false
    } finally {
      blocking.unsubscribe()
    }
  }

  // Unblock device via UCI firewall rule removal
  async unblock(mac: string): Promise<boolean> {
    const unblocking = this.notifications.open('Unblocking device').subscribe()

    try {
      await this.uciService.unblock(mac)

      // Update local cache - move to offline since we don't know if connected
      const device = this.devices.find(d => d.mac === mac)
      if (device) {
        device.status = 'offline'
      }

      return true
    } catch (e: any) {
      this.alertService.open(e.message, { appearance: 'negative' }).subscribe()
      return false
    } finally {
      unblocking.unsubscribe()
    }
  }

  // Forget device - remove from UCI config entirely
  async forget(mac: string): Promise<boolean> {
    const forgetting = this.notifications.open('Forgetting device').subscribe()

    try {
      await this.uciService.forget(mac)

      // Remove from local cache
      const index = this.devices.findIndex(d => d.mac === mac)
      if (index !== -1) {
        this.devices.splice(index, 1)
      }

      return true
    } catch (e: any) {
      this.alertService.open(e.message, { appearance: 'negative' }).subscribe()
      return false
    } finally {
      forgetting.unsubscribe()
    }
  }

  // Get historical data usage for a device
  async getDataUsage(
    mac: string,
    period: DataUsagePeriod,
  ): Promise<DataUsagePoint[]> {
    return this.uciService.getDataUsage(mac, period)
  }
}
