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
      description: `<h5>Local</h5>Select this option if you do not mind exposing your home/business IP address to the Internet. This option requires configuring router settings, which StartOS can do automatically if you have an OpenWRT router
<h5>Proxy</h5>Select this option is you prefer to hide your home/business IP address from the Internet. This option requires running your own Virtual Private Server (VPS) <i>or</i> paying service provider such as Static Wire
`,
    },
    Variants.of({
      local: {
        name: 'Local',
        spec: Config.of({
          ipStrategy: Value.select({
            name: 'IP Strategy',
            description: `<h5>IPv6 Only (recommended)</h5><b>Requirements</b>:<ol><li>ISP IPv6 support</li><li>OpenWRT (recommended) or Linksys router</li></ol><b>Pros</b>: Ready for IPv6 Internet. Enhanced privacy. Run multiple clearnet servers from the same network
<b>Cons</b>: Interfaces using this domain will only be accessible to people whose ISP supports IPv6
<h5>IPv6 and IPv4</h5><b>Pros</b>: Ready for IPv6 Internet. Accessible by anyone
<b>Cons</b>: Less private, as IPv4 addresses are closely correlated with geographic areas. Cannot run multiple clearnet servers from the same network
<h5>IPv4 Only</h5><b>Pros</b>: Accessible by anyone
<b>Cons</b>: Less private, as IPv4 addresses are closely correlated with geographic areas. Cannot run multiple clearnet servers from the same network
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
              name: 'Proxy Strategy',
              required: { default: 'primary' },
              description: `<h5>Primary</h5>Use the <i>Primary Inbound</i> proxy from your proxy settings. If you do not have any inbound proxies, no proxy will be used
<h5>Other</h5>Use a specific proxy from your proxy settings
`,
            },
            Variants.of({
              primary: {
                name: 'Primary',
                spec: Config.of({}),
              },
              other: {
                name: 'Specific',
                spec: Config.of({
                  proxyId: Value.select({
                    name: 'Select Proxy',
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

export async function getStart9ToSpec(proxies: Proxy[]) {
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
          njalla: {
            name: 'Njalla',
            spec: auth,
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
          zoneedit: {
            name: 'Zoneedit',
            spec: auth,
          },
          googledomains: {
            name: 'Google Domains (IPv4 or IPv6)',
            spec: auth,
          },
          namecheap: {
            name: 'Namecheap (IPv4 only)',
            spec: auth,
          },
        }),
      ),
      strategy: getStrategyUnion(proxies),
    }),
  )
}
