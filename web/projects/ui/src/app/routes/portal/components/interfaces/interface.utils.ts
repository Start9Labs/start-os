import { T, utils } from '@start9labs/start-sdk'
import { ConfigService } from 'src/app/services/config.service'

export abstract class AddressesService {
  abstract static: boolean
  abstract add(): Promise<void>
  abstract remove(): Promise<void>
}

export function getAddresses(
  serviceInterface: T.ServiceInterface,
  host: T.Host,
  config: ConfigService,
): {
  clearnet: (AddressDetails & { acme: string | null })[]
  local: AddressDetails[]
  tor: AddressDetails[]
} {
  const addressInfo = serviceInterface.addressInfo
  const hostnames =
    host.hostnameInfo[addressInfo.internalPort]?.filter(
      h =>
        config.isLocalhost() ||
        h.kind !== 'ip' ||
        h.hostname.kind !== 'ipv6' ||
        !h.hostname.value.startsWith('fe80::'),
    ) || []

  if (config.isLocalhost()) {
    const local = hostnames.find(
      h => h.kind === 'ip' && h.hostname.kind === 'local',
    )

    if (local) {
      hostnames.unshift({
        kind: 'ip',
        networkInterfaceId: 'lo',
        public: false,
        hostname: {
          kind: 'local',
          port: local.hostname.port,
          sslPort: local.hostname.sslPort,
          value: 'localhost',
        },
      })
    }
  }

  const clearnet: (AddressDetails & { acme: string | null })[] = []
  const local: AddressDetails[] = []
  const tor: AddressDetails[] = []

  hostnames.forEach(h => {
    const addresses = utils.addressHostToUrl(addressInfo, h)

    addresses.forEach(url => {
      if (h.kind === 'onion') {
        tor.push({
          label:
            addresses.length > 1
              ? new URL(url).protocol.replace(':', '').toUpperCase()
              : '',
          url,
        })
      } else {
        const hostnameKind = h.hostname.kind

        if (h.public) {
          clearnet.push({
            url,
            acme:
              hostnameKind == 'domain'
                ? host.domains[h.hostname.domain]?.acme || null
                : null, // @TODO Matt make sure this is handled correctly - looks like ACME settings aren't built yet anyway, but ACME settings aren't *available* for public IPs
          })
        } else {
          local.push({
            label:
              hostnameKind === 'local'
                ? 'Local'
                : `${h.networkInterfaceId} (${hostnameKind})`,
            url,
          })
        }
      }
    })
  })

  return {
    clearnet: clearnet.filter(
      (value, index, self) =>
        index === self.findIndex(t => t.url === value.url),
    ),
    local: local.filter(
      (value, index, self) =>
        index === self.findIndex(t => t.url === value.url),
    ),
    tor: tor.filter(
      (value, index, self) =>
        index === self.findIndex(t => t.url === value.url),
    ),
  }
}

export type MappedServiceInterface = T.ServiceInterface & {
  public: boolean
  addresses: {
    clearnet: AddressDetails[]
    local: AddressDetails[]
    tor: AddressDetails[]
  }
}

export type AddressDetails = {
  label?: string
  url: string
  acme?: string | null
}
