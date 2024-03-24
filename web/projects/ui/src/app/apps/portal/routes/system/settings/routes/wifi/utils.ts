import { ValueSpecObject } from '@start9labs/start-sdk/cjs/sdk/lib/config/configTypes'
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

export const wifiSpec: ValueSpecObject = {
  type: 'object',
  name: 'WiFi Credentials',
  description:
    'Enter the network SSID and password. You can connect now or save the network for later.',
  warning: null,
  spec: {
    ssid: {
      type: 'text',
      minLength: null,
      maxLength: null,
      patterns: [],
      name: 'Network SSID',
      description: null,
      inputmode: 'text',
      placeholder: null,
      required: true,
      masked: false,
      default: null,
      warning: null,
      disabled: false,
      immutable: false,
      generate: null,
    },
    password: {
      type: 'text',
      minLength: null,
      maxLength: null,
      patterns: [
        {
          regex: '^.{8,}$',
          description: 'Must be longer than 8 characters',
        },
      ],
      name: 'Password',
      description: null,
      inputmode: 'text',
      placeholder: null,
      required: true,
      masked: true,
      default: null,
      warning: null,
      disabled: false,
      immutable: false,
      generate: null,
    },
  },
}
