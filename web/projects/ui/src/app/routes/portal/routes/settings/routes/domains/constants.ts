import { ISB } from '@start9labs/start-sdk'
import { Proxy } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

const auth = ISB.InputSpec.of({
  username: ISB.Value.text({
    name: 'Username',
    required: true,
    default: null,
  }),
  password: ISB.Value.text({
    name: 'Password',
    required: true,
    default: null,
    masked: true,
  }),
})

function getStrategyUnion(proxies: Proxy[]) {
  const inboundProxies: Record<string, string> = proxies
    .filter(p => p.type === 'inbound-outbound')
    .reduce(
      (prev, curr) => ({
        [curr.id]: curr.name,
        ...prev,
      }),
      {},
    )

  return ISB.Value.union(
    {
      name: 'Networking Strategy',
      default: 'local',
      description: `<h5>Local</h5>Select this option if you do not mind exposing your home/business IP address to the Internet. This option requires configuring router settings, which StartOS can do automatically if you have an OpenWRT router
<h5>Proxy</h5>Select this option is you prefer to hide your home/business IP address from the Internet. This option requires running your own Virtual Private Server (VPS) <i>or</i> paying service provider such as Static Wire
`,
    },
    ISB.Variants.of({
      local: {
        name: 'Local',
        spec: ISB.InputSpec.of({
          ipStrategy: ISB.Value.select({
            name: 'IP Strategy',
            description: `<h5>IPv6 Only (recommended)</h5><b>Requirements</b>:<ol><li>ISP IPv6 support</li><li>OpenWRT (recommended) or Linksys router</li></ol><b>Pros</b>: Ready for IPv6 Internet. Enhanced privacy. Run multiple clearnet servers from the same network
<b>Cons</b>: Interfaces using this domain will only be accessible to people whose ISP supports IPv6
<h5>IPv6 and IPv4</h5><b>Pros</b>: Ready for IPv6 Internet. Accessible by anyone
<b>Cons</b>: Less private, as IPv4 addresses are closely correlated with geographic areas. Cannot run multiple clearnet servers from the same network
<h5>IPv4 Only</h5><b>Pros</b>: Accessible by anyone
<b>Cons</b>: Less private, as IPv4 addresses are closely correlated with geographic areas. Cannot run multiple clearnet servers from the same network
`,
            default: 'ipv6',
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
        spec: ISB.InputSpec.of({
          proxyId: ISB.Value.select({
            name: 'Select Proxy',
            default: proxies.filter(p => p.type === 'inbound-outbound')[0].id,
            values: inboundProxies,
          }),
        }),
      },
    }),
  )
}

export function getStart9ToSpec(proxies: Proxy[]) {
  return configBuilderToSpec(
    ISB.InputSpec.of({
      strategy: getStrategyUnion(proxies),
    }),
  )
}

export function getCustomSpec(proxies: Proxy[]) {
  return configBuilderToSpec(
    ISB.InputSpec.of({
      hostname: ISB.Value.text({
        name: 'Hostname',
        required: true,
        default: null,
        placeholder: 'yourdomain.com',
      }),
      provider: ISB.Value.union(
        {
          name: 'Dynamic DNS Provider',
          default: 'start9',
        },
        ISB.Variants.of({
          start9: {
            name: 'Start9',
            spec: ISB.InputSpec.of({}),
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
