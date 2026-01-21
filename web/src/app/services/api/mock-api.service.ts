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
  SystemInfoRes,
  SetFileReq,
  SetUciReq,
  SetUciRes,
  VersionInfo,
  SetPasswordReq,
  SetPreferencesReq,
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
import { mockPublishedPorts } from 'src/app/routes/home/routes/published-ports/uci/mocks'

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

  private mockAuthorizedKeys = `ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl matt@macbook
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJf3LQXK5m7dZtQgkVwMYxPragThKvOHPrLwfCfMR7fa lucy@desktop`

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    await pauseFor(1000)

    if (params.path === '/root/.ssh/authorized_keys') {
      return {
        modified: new Date().toISOString(),
        contents: this.mockAuthorizedKeys,
      }
    }

    return {
      modified: new Date().toISOString(),
      contents: '',
    }
  }

  async setFile(params: SetFileReq): Promise<null> {
    await pauseFor(1000)

    if (params.path === '/root/.ssh/authorized_keys') {
      this.mockAuthorizedKeys = params.contents
    }

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

    // Actually update the mock data
    for (const name of Object.keys(params)) {
      if (mockUci[name]) {
        mockUci[name] = {
          ...params[name],
          modified: isoString,
        }
      }
    }

    return Object.keys(params).reduce(
      (obj, name) => ({
        ...obj,
        [name]: isoString,
      }),
      {} as SetUciRes<T>,
    )
  }

  async systemInfo(): Promise<SystemInfoRes> {
    await pauseFor(300)

    return {
      version: '1.0.0',
      language: 'English',
      date: new Date().toISOString(),
      theme: 'system',
    }
  }

  async systemNewerVersions(): Promise<VersionInfo[]> {
    await pauseFor(500)

    return [
      {
        version: '1.0.1',
        releaseNotes: `## Bug Fixes

- Resolved issue with DHCP lease renewals
- Fixed port forwarding rules not persisting after reboot
- Corrected timezone display in system logs

## Security Updates

- Updated OpenSSL to latest version
- Patched CVE-2024-1234 vulnerability`,
      },
      {
        version: '1.0.2',
        releaseNotes: `## What's New

- Improved Wi-Fi stability and range
- Fixed intermittent connection drops on 5GHz band
- Added support for WPA3 security protocol
- Performance improvements for VPN connections`,
      },
    ]
  }

  async systemRestart(): Promise<null> {
    await pauseFor(1000)
    return null
  }

  async setPassword(params: SetPasswordReq): Promise<null> {
    await pauseFor(500)
    // Mock validation - in real implementation, backend validates old password
    if (params.oldPassword === '') {
      throw new Error('Invalid old password')
    }
    return null
  }

  async setPreferences(params: SetPreferencesReq): Promise<null> {
    await pauseFor(300)
    return null
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
    sections: [...mockBlockedDevices, ...mockPublishedPorts],
    modified: new Date().toISOString(),
  },
}
