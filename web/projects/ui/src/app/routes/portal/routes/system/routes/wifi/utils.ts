import { T } from '@start9labs/start-sdk'

export interface WiFiForm {
  ssid: string
  password: string
}

export interface Wifi extends T.WifiListOut {
  readonly connected?: boolean
}

export interface WifiData {
  known: readonly Wifi[]
  available: readonly Wifi[]
}

export function parseWifi(res: T.WifiListInfo): WifiData {
  return {
    available: res.availableWifi,
    known: Object.entries(res.ssids).map(([ssid, strength]) => ({
      ssid,
      strength,
      security: [],
      connected: ssid === res.connected,
    })),
  }
}
