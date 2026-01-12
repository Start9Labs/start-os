import { Injectable } from '@angular/core'
import { pauseFor } from '../../utils/pauseFor'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  LoginReq,
  SetUciReq,
  SetUciRes,
} from './api.service'
import { UciFile, UciSection } from './types'
import { wanIpv4Dhcp } from 'src/app/routes/home/routes/wan/routes/ipv4/uci/mocks'
import { wanIpv6Slaac } from 'src/app/routes/home/routes/wan/routes/ipv6/uci/mock'
import { ddnsStart9 } from 'src/app/routes/home/routes/wan/routes/ddns/uci/mocks'
import { macRouterDevice } from 'src/app/routes/home/routes/wan/routes/mac/uci/mocks'
import { lanDefault } from 'src/app/routes/home/routes/lan/routes/ipv4/uci/mocks'
import { dhcpLanSlaacDhcpv6 } from 'src/app/routes/home/routes/lan/routes/ipv6/uci/mocks'
import { mockWireGuardSections } from 'src/app/routes/home/routes/outbound/uci/mocks'
import {
  generateMockDataUsage,
  mockArpOutput,
  mockBlockedDevices,
  mockDhcpHosts,
  mockDhcpLeasesOutput,
} from 'src/app/routes/home/routes/devices/uci/mocks'

@Injectable({
  providedIn: 'root',
})
export class MockApiService extends ApiService {
  async login(params: LoginReq): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async logout(): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async exec(params: ExecReq): Promise<ExecRes> {
    await pauseFor(300)

    // Handle specific commands for device discovery
    if (params.command === 'ip' && params.args[0] === 'neigh') {
      return {
        exitCode: 0,
        stdout: mockArpOutput,
        stderr: '',
      }
    }

    if (params.command === 'cat' && params.args[0] === '/tmp/dhcp.leases') {
      return {
        exitCode: 0,
        stdout: mockDhcpLeasesOutput,
        stderr: '',
      }
    }

    // Handle service restart commands
    if (
      params.command === '/etc/init.d/firewall' ||
      params.command === '/etc/init.d/dnsmasq' ||
      params.command === '/etc/init.d/network'
    ) {
      return {
        exitCode: 0,
        stdout: '',
        stderr: '',
      }
    }

    // Handle nlbwmon queries for data usage
    if (params.command === 'nlbw') {
      // Parse the date from args (-t flag)
      const dateIndex = params.args.indexOf('-t')
      const startDate =
        dateIndex !== -1 ? params.args[dateIndex + 1] : undefined

      // Determine period based on date range
      let period: 'day' | 'week' | 'month' | '3months' = 'week'
      if (startDate) {
        const start = new Date(startDate)
        const now = new Date()
        const daysDiff = Math.floor(
          (now.getTime() - start.getTime()) / (24 * 60 * 60 * 1000),
        )
        if (daysDiff <= 1) period = 'day'
        else if (daysDiff <= 7) period = 'week'
        else if (daysDiff <= 30) period = 'month'
        else period = '3months'
      }

      // Generate mock data for all known MACs
      const macs = [
        '00:1a:2b:3c:4d:5e',
        '00:1a:2b:3c:4d:5f',
        'de:ad:be:ef:ca:fe',
        'de:ad:be:ef:ca:ff',
      ]

      const mockData: any[] = []
      for (const mac of macs) {
        const points = generateMockDataUsage(mac, period)
        for (const point of points) {
          mockData.push([mac, point.download, point.upload, point.timestamp])
        }
      }

      return {
        exitCode: 0,
        stdout: JSON.stringify({
          columns: ['mac', 'rx_bytes', 'tx_bytes', 'interval_start'],
          data: mockData,
        }),
        stderr: '',
      }
    }

    return {
      exitCode: 0,
      stdout: 'success',
      stderr: '',
    }
  }

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(1000)

    return {
      modified: new Date().toISOString(),
      contents: '',
    }
  }

  async setFile(params: GetFileRes): Promise<null> {
    await pauseFor(1000)

    return null
  }

  async getUci<T extends Record<string, UciFile<any>>>(
    params: GetUciReq,
  ): Promise<T> {
    await pauseFor(1000)

    return params.names.reduce(
      (obj, name) => ({
        ...obj,
        [name]: mockUci[name],
      }),
      {} as T,
    )
  }

  async setUci<T extends string[]>(params: SetUciReq): Promise<SetUciRes<T>> {
    await pauseFor(1000)

    const isoString = new Date().toISOString()

    return Object.keys(params).reduce(
      (obj, name) => ({
        ...obj,
        [name]: isoString,
      }),
      {} as SetUciRes<T>,
    )
  }
}

export const mockUci: Record<string, UciFile<UciSection>> = {
  network: {
    sections: [
      wanIpv4Dhcp,
      wanIpv6Slaac,
      macRouterDevice,
      lanDefault,
      ...mockWireGuardSections,
    ],
    modified: new Date().toISOString(),
  },
  ddns: {
    sections: [ddnsStart9],
    modified: new Date().toISOString(),
  },
  dhcp: {
    sections: [dhcpLanSlaacDhcpv6, ...mockDhcpHosts],
    modified: new Date().toISOString(),
  },
  firewall: {
    sections: [...mockBlockedDevices],
    modified: new Date().toISOString(),
  },
}
