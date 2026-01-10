import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { MacForm } from '../utils'
import {
  NetworkDeviceSection,
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'

type UciFiles = {
  network: UciFile<UciSection>
}

@Injectable({
  providedIn: 'root',
})
export class MacUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles
  private _defaultMac?: string

  async get(): Promise<MacForm> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    const wanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )

    if (!wanInterface) {
      throw new Error('No WAN interface found')
    }

    // Get the device to find its default MAC
    const deviceName = wanInterface.options.device
    const device = this._uciFiles.network.sections.find(
      (s): s is NetworkDeviceSection =>
        s.type === 'device' && s.options.name === deviceName,
    )

    // Store default MAC for later (from device or interface)
    this._defaultMac =
      device?.options.macaddr ||
      wanInterface.options.macaddr ||
      '00:00:00:00:00:00'

    // If macaddr is explicitly set, it's custom; otherwise use router default
    const customMac = wanInterface.options.macaddr || device?.options.macaddr
    const hasCustomMac = !!wanInterface.options.macaddr

    return {
      strategy: hasCustomMac ? 'custom' : 'router',
      address: {
        mac: customMac || this._defaultMac,
      },
    }
  }

  async set({ strategy, address }: MacForm): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    const wanInterface = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )

    if (!wanInterface) {
      throw new Error('No WAN interface found')
    }

    if (strategy === 'custom') {
      wanInterface.options.macaddr = address.mac
    } else {
      // Remove custom MAC to use router default
      delete wanInterface.options.macaddr
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart network
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }
}
