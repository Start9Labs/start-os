import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { buildFullIp, LanIpv4Form, parseIpToForm } from '../utils'
import {
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
export class LanIpv4UciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<LanIpv4Form> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    const lanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'lan',
    )

    if (!lanInterface) {
      // Return defaults if no LAN interface found
      return {
        ip: {
          firstOctet: 192,
          thirdOctet: 0,
          routerOctet: 1,
        },
      }
    }

    const ipaddr = lanInterface.options.ipaddr || '192.168.0.1'
    return parseIpToForm(ipaddr)
  }

  async set(form: LanIpv4Form): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Configuration not loaded yet')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    let lanInterface = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'lan',
    )

    if (!lanInterface) {
      lanInterface = {
        type: 'interface',
        name: 'lan',
        options: {
          proto: 'static',
        },
        lists: {},
      }
      uciFiles.network.sections.push(lanInterface)
    }

    lanInterface.options.proto = 'static'
    lanInterface.options.ipaddr = buildFullIp(form.ip)
    lanInterface.options.netmask = '255.255.255.0'

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart network
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }
}
