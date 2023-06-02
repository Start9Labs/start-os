import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'

export type DomainForm = {
  hostname: string
  baseToken: string
  wildcardToken: string
}

export const domain = Config.of({
  provider: Value.select({
    name: 'Provider',
    required: { default: null },
    values: {
      start9: 'Start9',
      namecheap: 'Namecheap',
      googledomains: 'Google Domains',
    },
  }),
  hostname: Value.text({
    name: 'Hostname',
    required: { default: null },
  }),
  username: Value.text({
    name: 'Username',
    required: { default: null },
  }),
  password: Value.text({
    name: 'Password',
    required: { default: null },
  }),
})

export const domainSpec: InputSpec = {
  hostname: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    name: 'Hostname',
    description: null,
    inputmode: 'url',
    placeholder: null,
    required: true,
    masked: false,
    default: null,
    warning: null,
    disabled: false,
    immutable: false,
    generate: null,
  },
  baseToken: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    name: 'Base Token',
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
  wildcardToken: {
    type: 'text',
    minLength: null,
    maxLength: null,
    patterns: [],
    name: 'Wildcard Token',
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
}
