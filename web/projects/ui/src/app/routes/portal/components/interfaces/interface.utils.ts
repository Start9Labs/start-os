import { ISB, IST, T, utils } from '@start9labs/start-sdk'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiConfirmData } from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'
import { NetworkInfo } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

export abstract class AddressesService {
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

export function getClearnetSpec({
  domains,
  start9ToSubdomain,
}: NetworkInfo): Promise<IST.InputSpec> {
  const start9ToDomain = `${start9ToSubdomain?.value}.start9.to`
  const base = start9ToSubdomain ? { [start9ToDomain]: start9ToDomain } : {}

  const values = domains.reduce((prev, curr) => {
    return {
      [curr.value]: curr.value,
      ...prev,
    }
  }, base)

  return configBuilderToSpec(
    ISB.InputSpec.of({
      domain: ISB.Value.select({
        name: 'Domain',
        default: domains[0].value,
        values,
      }),
      subdomain: ISB.Value.text({
        name: 'Subdomain',
        required: false,
        default: '',
      }),
    }),
  )
}

// @TODO Aiden audit
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
    local,
    tor,
  }

  // @TODO Aiden what was going on here in 036?
  // return mappedAddresses.filter(
  //   (value, index, self) => index === self.findIndex(t => t.url === value.url),
  // )
}

function getLabel(name: string, url: string, multiple: boolean) {}

export type AddressDetails = {
  label: string
  url: string
}
