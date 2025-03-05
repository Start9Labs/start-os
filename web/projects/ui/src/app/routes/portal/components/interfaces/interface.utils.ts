import { T, utils } from '@start9labs/start-sdk'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiConfirmData } from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'

export abstract class AddressesService {
  abstract static: boolean
  abstract add(): Promise<void>
  abstract remove(): Promise<void>
}

export const REMOVE: Partial<TuiDialogOptions<TuiConfirmData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Remove clearnet address?',
    yes: 'Remove',
    no: 'Cancel',
  },
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

  let hostnames = host.hostnameInfo[addressInfo.internalPort]

  hostnames = hostnames.filter(
    h =>
      config.isLocalhost() ||
      h.kind !== 'ip' ||
      h.hostname.kind !== 'ipv6' ||
      !h.hostname.value.startsWith('fe80::'),
  )

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
          label: `Tor${
            addresses.length > 1
              ? ` (${new URL(url).protocol.replace(':', '').toUpperCase()})`
              : ''
          }`,
          url,
        })
      } else {
        const hostnameKind = h.hostname.kind

        if (hostnameKind === 'domain') {
          clearnet.push({
            label: 'Domain',
            url,
            acme: host.domains[h.hostname.domain]?.acme,
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
    clearnet,
    local: local.filter(
      (value, index, self) =>
        index === self.findIndex(t => t.url === value.url),
    ),
    tor,
  }
}

export type AddressDetails = {
  label: string
  url: string
}
