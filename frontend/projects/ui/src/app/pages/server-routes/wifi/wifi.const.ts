import { ValueSpecObject } from '@start9labs/start-sdk/lib/config/configTypes'

export const wifiConst: ValueSpecObject = {
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
