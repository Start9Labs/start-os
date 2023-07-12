import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'

const ddnsOptions = Config.of({
  username: Value.text({
    name: 'Username',
    required: { default: null },
  }),
  password: Value.text({
    name: 'Password',
    required: { default: null },
    masked: true,
  }),
})

export const domainSpec = Config.of({
  hostname: Value.text({
    name: 'Hostname',
    required: { default: null },
    placeholder: 'yourdomain.com',
  }),
  provider: Value.union(
    {
      name: 'Dynamic DNS Provider',
      required: { default: 'start9' },
    },
    Variants.of({
      start9: {
        name: 'Start9',
        spec: Config.of({}),
      },
      duckdns: {
        name: 'Duck DNS',
        spec: ddnsOptions,
      },
      dyn: {
        name: 'DynDNS',
        spec: ddnsOptions,
      },
      easydns: {
        name: 'easyDNS',
        spec: ddnsOptions,
      },
      googledomains: {
        name: 'Google Domains',
        spec: ddnsOptions,
      },
      namecheap: {
        name: 'Namecheap (IPv4 only)',
        spec: ddnsOptions,
      },
      zoneedit: {
        name: 'Zoneedit',
        spec: ddnsOptions,
      },
    }),
  ),
})

export type DomainSpec = typeof domainSpec.validator._TYPE
