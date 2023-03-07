import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'

export const domainSpec = Config.of({
  provider: Value.select({
    name: 'Provider',
    required: { default: null },
    values: {
      namecheap: 'Namecheap',
      googledomains: 'Google Domains',
      duckdns: 'Duck DNS',
      changeip: 'ChangeIP',
      easydns: 'easyDNS',
      zoneedit: 'Zoneedit',
      dyn: 'DynDNS',
    },
  }),
  domain: Value.text({
    name: 'Domain Name',
    required: { default: null },
    placeholder: 'yourdomain.com',
  }),
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

export type DomainSpec = typeof domainSpec.validator._TYPE
