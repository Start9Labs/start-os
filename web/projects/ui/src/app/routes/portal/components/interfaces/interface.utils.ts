import { ISB, IST, T, utils } from '@start9labs/start-sdk'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiConfirmData } from '@taiga-ui/kit'
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

export type AddressDetails = {
  label?: string
  url: string
}

export function getMultihostAddresses(
  serviceInterface: T.ServiceInterface,
  host: T.Host,
): {
  clearnet: AddressDetails[]
  local: AddressDetails[]
  tor: AddressDetails[]
} {
  const addressInfo = serviceInterface.addressInfo
  const hostnamesInfo = host.hostnameInfo[addressInfo.internalPort]

  const clearnet: AddressDetails[] = []
  const local: AddressDetails[] = []
  const tor: AddressDetails[] = []

  hostnamesInfo.forEach(hostnameInfo => {
    utils.addressHostToUrl(addressInfo, hostnameInfo).forEach(url => {
      // Onion
      if (hostnameInfo.kind === 'onion') {
        tor.push({ url })
        // IP
      } else {
        // Domain
        if (hostnameInfo.hostname.kind === 'domain') {
          clearnet.push({ url })
          // Local
        } else {
          const hostnameKind = hostnameInfo.hostname.kind
          local.push({
            label:
              hostnameKind === 'local'
                ? 'Local'
                : `${hostnameInfo.networkInterfaceId} (${hostnameKind})`,
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
}
