import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'

const auth = Config.of({
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

const strategyUnion = Value.union(
  {
    name: 'Networking Strategy',
    required: { default: 'router' },
  },
  Variants.of({
    router: {
      name: 'Router',
      spec: Config.of({
        ip: Value.select({
          name: 'IP Strategy',
          description: `
<h5>IPv6 Only</h5><b>Pros</b>: Ready for IPv6 Internet. Enhanced privacy, as IPv6 addresses are less correlated with geographic area
<b>Cons</b>: Your website is only accessible to people who's ISP supports IPv6
<h5>IPv6 and IPv4</h5><b>Pros</b>: Ready for IPv6 Internet. Anyone can access your website
<b>Cons</b>: IPv4 addresses are closely correlated with geographic areas
<h5>IPv4 Only</h5><b>Pros</b>: Anyone can access your website
<b>Cons</b>: IPv4 addresses are closely correlated with geographic areas
`,
          required: { default: 'ipv6' },
          values: {
            ipv6: 'IPv6 Only',
            both: 'IPv6 and IPv4',
            ipv4: 'IPv4 Only',
          },
        }),
      }),
    },
    reverseProxy: {
      name: 'Reverse Proxy',
      spec: Config.of({}),
    },
  }),
)

export const start9MeSpec = Config.of({
  strategy: strategyUnion,
})

export const customSpec = Config.of({
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
        spec: auth,
      },
      dyn: {
        name: 'DynDNS',
        spec: auth,
      },
      easydns: {
        name: 'easyDNS',
        spec: auth,
      },
      googledomains: {
        name: 'Google Domains',
        spec: auth,
      },
      namecheap: {
        name: 'Namecheap (IPv4 only)',
        spec: auth,
      },
      zoneedit: {
        name: 'Zoneedit',
        spec: auth,
      },
    }),
  ),
  strategy: strategyUnion,
})

export type Start9MeSpec = typeof start9MeSpec.validator._TYPE
export type CustomSpec = typeof customSpec.validator._TYPE
