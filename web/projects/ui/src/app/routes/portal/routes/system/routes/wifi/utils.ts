import { AvailableWifi } from 'src/app/services/api/api.types'
import { RR } from 'src/app/services/api/api.types'

export interface WiFiForm {
  ssid: string
  password: string
}

export interface Wifi extends AvailableWifi {
  readonly connected?: boolean
}

export interface WifiData {
  known: readonly Wifi[]
  available: readonly Wifi[]
}

export function parseWifi(res: RR.GetWifiRes): WifiData {
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
