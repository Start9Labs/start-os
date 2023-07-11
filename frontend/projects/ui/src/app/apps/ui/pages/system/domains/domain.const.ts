import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'

const ddnsOptions = {
  username: Value.text({
    name: 'Username',
    required: { default: null },
  }),
  password: Value.text({
    name: 'Password',
    required: { default: null },
    masked: true,
  }),
}
const ipv4Option = {
  ipv4: Value.toggle({
    name: 'Enable IPv4',
    default: false,
    description:
      'Enable IPv4 if: <ol><li>Your ISP or router does not support IPv6</li><li>You want those who lack IPv6 to reach your site</li></ol> <b>Warning!</b> IPv4 addresses are closely correlated with geographic areas. If you are not using a reverse proxy, everyone will be able to determine the general location of your server on Earth.',
  }),
}

const options = Config.of(ddnsOptions)
const optionsPlus = Config.of({ ...ddnsOptions, ...ipv4Option })

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
        spec: Config.of(ipv4Option),
      },
      duckdns: {
        name: 'Duck DNS',
        spec: optionsPlus,
      },
      dyn: {
        name: 'DynDNS',
        spec: optionsPlus,
      },
      easydns: {
        name: 'easyDNS',
        spec: optionsPlus,
      },
      googledomains: {
        name: 'Google Domains',
        spec: optionsPlus,
      },
      namecheap: {
        name: 'Namecheap (IPv4 only)',
        spec: options,
      },
      zoneedit: {
        name: 'Zoneedit',
        spec: optionsPlus,
      },
    }),
  ),
})

export type DomainSpec = typeof domainSpec.validator._TYPE
