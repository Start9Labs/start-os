import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'
import { Proxy } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'

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

function getStrategyUnion(proxies: Proxy[]) {
  const inboundProxies = proxies
    .filter(p => p.type === 'inbound-outbound')
    .reduce((prev, curr) => {
      return {
        [curr.id]: curr.name,
        ...prev,
      }
    }, {})

  return Value.union(
    {
      name: 'Networking Strategy',
      required: { default: null },
      description: `
<h5>Router</h5>Select this option if you do not mind exposing your home/business IP address to the Internet. This option is free but requires configuring router settings, which can be done automatically by StartOS if you have an OpenWRT compatible router
<h5>Proxy</h5>Select this option is you prefer to hide you home/business IP address from the Internet. This option requires running your own virtual private server (VPS) in the cloud or paying a service provider, both of which are made easy by StartOS
`,
    },
    Variants.of({
      router: {
        name: 'Router',
        spec: Config.of({
          ipStrategy: Value.select({
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
              ipv4: 'IPv4 Only',
              dualstack: 'IPv6 and IPv4',
            },
          }),
        }),
      },
      proxy: {
        name: 'Proxy',
        spec: Config.of({
          proxyStrategy: Value.union(
            {
              name: 'Select Proxy',
              required: { default: 'primary' },
              description: `
<h5>System Default</h5>The system default <i>inbound</i> proxy will be used. If you do not have an inbound proxy, no proxy will be used
<h5>Other</h5>The specific proxy you select will be used, overriding the default
`,
            },
            Variants.of({
              primary: {
                name: 'System Default',
                spec: Config.of({}),
              },
              other: {
                name: 'Other',
                spec: Config.of({
                  proxyId: Value.select({
                    name: 'Select Specific Proxy',
                    required: { default: null },
                    values: inboundProxies,
                  }),
                }),
              },
            }),
          ),
        }),
      },
    }),
  )
}

export async function getStart9MeSpec(proxies: Proxy[]) {
  return configBuilderToSpec(
    Config.of({
      strategy: getStrategyUnion(proxies),
    }),
  )
}

export async function getCustomSpec(proxies: Proxy[]) {
  return configBuilderToSpec(
    Config.of({
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
      strategy: getStrategyUnion(proxies),
    }),
  )
}
