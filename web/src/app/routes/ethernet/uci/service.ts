import { inject, Injectable } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import {
  NetworkDeviceSection,
  NetworkInterfaceSection,
  UciFile,
  UciSection,
} from 'src/app/services/api/types'

export interface EthernetPort {
  /** Physical port name (e.g., eth0, eth1) */
  name: string
  /** Security profile name (stub - not yet implemented) */
  profile: string
  /** Whether this port is currently the WAN port */
  wan: boolean
}

type UciFiles = {
  network: UciFile<UciSection>
}

/**
 * Stub: Available security profiles
 * TODO: Replace with actual profile service when implemented
 */
const STUB_PROFILES = ['Admin', 'Guest']

/**
 * Stub: Port to profile mapping
 * TODO: Replace with actual UCI config when security profiles are implemented
 */
const stubPortProfiles: Record<string, string> = {
  eth1: 'Admin',
  eth2: 'Admin',
  eth3: 'Guest',
}

/**
 * Extract base port name from device string
 * Handles VLAN notation (e.g., "eth0.2" -> "eth0")
 */
function getBasePortName(device: string): string {
  // Remove VLAN suffix (e.g., eth0.2 -> eth0)
  const vlanMatch = device.match(/^([a-z]+\d+)(?:\.\d+)?$/)
  return vlanMatch ? vlanMatch[1] : device
}

@Injectable({
  providedIn: 'root',
})
export class EthernetUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  /**
   * Get all Ethernet ports with their current configuration
   */
  async get(): Promise<EthernetPort[]> {
    this._uciFiles = await this.api.getUci<UciFiles>({
      names: ['network'],
    })

    const wanPort = this.getWanPort()
    const lanPorts = this.getLanBridgePorts()

    // Combine WAN and LAN ports, sorted by name
    const allPorts = new Set<string>()
    if (wanPort) allPorts.add(wanPort)
    lanPorts.forEach(p => allPorts.add(p))

    return Array.from(allPorts)
      .sort()
      .map(name => ({
        name,
        profile: name === wanPort ? '' : this.getPortProfile(name),
        wan: name === wanPort,
      }))
  }

  /**
   * Update Ethernet port configuration
   */
  async set(ports: EthernetPort[]): Promise<void> {
    if (!this._uciFiles) {
      throw new Error('Must call get() before set()')
    }

    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles

    const wanPort = ports.find(p => p.wan)
    const lanPorts = ports.filter(p => !p.wan).map(p => p.name)

    // Update WAN device
    this.setWanPort(uciFiles, wanPort?.name || '')

    // Update LAN bridge ports
    this.setLanBridgePorts(uciFiles, lanPorts)

    // Update port profiles (stub)
    for (const port of ports) {
      if (!port.wan && port.profile) {
        stubPortProfiles[port.name] = port.profile
      }
    }

    await this.api.setUci<(keyof typeof uciFiles)[]>(uciFiles)

    // Restart network to apply changes
    await this.api.exec({
      command: '/etc/init.d/network',
      args: ['restart'],
      timeout: 30000,
    })
  }

  /**
   * Get available security profiles (stub)
   */
  getProfiles(): string[] {
    return STUB_PROFILES
  }

  /**
   * Get the current WAN port name (base name, without VLAN suffix)
   */
  private getWanPort(): string | null {
    if (!this._uciFiles) return null

    // Find WAN interface
    const wanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )

    if (!wanInterface) return null

    // WAN can reference a device directly or through a device section
    const deviceName =
      wanInterface.options.device || wanInterface.options.ifname

    if (!deviceName) return null

    // Extract base port name (handles VLAN notation like eth0.2)
    return getBasePortName(deviceName)
  }

  /**
   * Get all ports in the LAN bridge
   */
  private getLanBridgePorts(): string[] {
    if (!this._uciFiles) return []

    // Find LAN interface to get its device
    const lanInterface = this._uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'lan',
    )

    if (!lanInterface) return []

    const bridgeName = lanInterface.options.device || 'br-lan'

    // Find the bridge device section
    const bridgeDevice = this._uciFiles.network.sections.find(
      (s): s is NetworkDeviceSection =>
        s.type === 'device' &&
        (s.options.name === bridgeName ||
          s.name === bridgeName.replace('-', '_')),
    )

    return bridgeDevice?.lists?.ports || []
  }

  /**
   * Set the WAN port
   * Preserves VLAN suffix if originally present
   */
  private setWanPort(uciFiles: UciFiles, portName: string): void {
    // Find WAN interface
    const wanInterface = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan',
    )

    if (wanInterface) {
      // Check if current device has a VLAN suffix
      const currentDevice = wanInterface.options.device || ''
      const vlanMatch = currentDevice.match(/^[a-z]+\d+(\.\d+)$/)
      const vlanSuffix = vlanMatch ? vlanMatch[1] : ''

      // Apply new port with same VLAN suffix (if any)
      wanInterface.options.device = portName + vlanSuffix
      delete wanInterface.options.ifname
    }

    // Also update wan6 if it exists
    const wan6Interface = uciFiles.network.sections.find(
      (s): s is NetworkInterfaceSection =>
        s.type === 'interface' && s.name === 'wan6',
    )

    if (wan6Interface && wan6Interface.options.device) {
      const currentDevice = wan6Interface.options.device
      const vlanMatch = currentDevice.match(/^[a-z]+\d+(\.\d+)$/)
      const vlanSuffix = vlanMatch ? vlanMatch[1] : ''

      wan6Interface.options.device = portName + vlanSuffix
      delete wan6Interface.options.ifname
    }
  }

  /**
   * Set the LAN bridge ports
   */
  private setLanBridgePorts(uciFiles: UciFiles, ports: string[]): void {
    // Find the bridge device section
    const bridgeDevice = uciFiles.network.sections.find(
      (s): s is NetworkDeviceSection =>
        s.type === 'device' && s.options.type === 'bridge',
    )

    if (bridgeDevice) {
      bridgeDevice.lists = bridgeDevice.lists || {}
      bridgeDevice.lists.ports = ports
    }
  }

  /**
   * Get security profile for a port (stub)
   */
  private getPortProfile(portName: string): string {
    return stubPortProfiles[portName] || 'Admin'
  }
}
