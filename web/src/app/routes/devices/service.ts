import { inject, Injectable } from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { ApiService, DeviceFromApi } from 'src/app/services/api/api.service'
import {
  DataUsagePeriod,
  DataUsagePoint,
  Device,
  DeviceStatus,
  DeviceTableItem,
  DeviceUpdateData,
} from './utils'

/**
 * Root-provided service for device API calls.
 * Used by DevicesService (FormService) and external routes
 * (published-ports, inbound) that need device data.
 */
@Injectable({
  providedIn: 'root',
})
export class DevicesApiService {
  private readonly api = inject(ApiService)

  async get(): Promise<Device[]> {
    const devices = await this.api.devicesList()
    return devices.map(d => this.mapDevice(d))
  }

  async update(mac: string, data: DeviceUpdateData): Promise<void> {
    await this.api.devicesUpdate({
      mac,
      name: data.name,
      ipv4_static: data.ipv4Static,
      ipv4: data.ipv4,
      ipv6_static: data.ipv6Static,
      ipv6: data.ipv6,
    })
  }

  async forget(mac: string): Promise<void> {
    await this.api.devicesForget({ mac })
  }

  async getDataUsage(
    mac: string,
    period: DataUsagePeriod,
  ): Promise<DataUsagePoint[]> {
    return this.api.devicesDataUsage({ mac, period })
  }

  private mapDevice(d: DeviceFromApi): Device {
    const name =
      d.name ||
      (d.hostname && d.hostname !== '*' ? d.hostname : null) ||
      (d.mac ? this.generateNameFromMac(d.mac) : 'VPN Device')

    return {
      mac: d.mac,
      name,
      hostname: d.hostname || '',
      status: d.status,
      connection: d.connection || undefined,
      ipv4: d.ipv4 || undefined,
      ipv6: d.ipv6 || undefined,
      ipv4Static: d.ipv4_static,
      ipv6Static: d.ipv6_static,
      securityProfile: d.security_profile || undefined,
      speed: d.speed || undefined,
      dataUsage: d.data_usage ?? undefined,
    }
  }

  private generateNameFromMac(mac: string): string {
    const suffix = mac.replace(/:/g, '').slice(-6).toLowerCase()
    return `device-${suffix}`
  }
}

@Injectable()
export class DevicesService extends FormService<Device[]> {
  private readonly devicesApi = inject(DevicesApiService)

  // Cache of loaded devices
  private devices: Device[] = []

  async load(): Promise<Device[]> {
    this.devices = await this.devicesApi.get()
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

  // Update device settings
  update(mac: string, data: DeviceUpdateData) {
    return this.actions.run(async () => {
      await this.devicesApi.update(mac, data)

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

  // Forget device - remove entirely
  forget(mac: string) {
    return this.actions.run(
      async () => {
        await this.devicesApi.forget(mac)

        // Remove from local cache
        this.devices = this.devices.filter(d => d.mac !== mac)
      },
      { loading: 'Forgetting device' },
    )
  }

  // Get historical data usage for a device
  async getDataUsage(
    mac: string,
    period: DataUsagePeriod,
  ): Promise<DataUsagePoint[]> {
    return this.devicesApi.getDataUsage(mac, period)
  }
}
