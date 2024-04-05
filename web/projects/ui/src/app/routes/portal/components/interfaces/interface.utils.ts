import { CB, CT, T } from '@start9labs/start-sdk'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiPromptData } from '@taiga-ui/kit'
import { NetworkInfo } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'

export abstract class AddressesService {
  abstract add(): Promise<void>
  abstract remove(): Promise<void>
}

export const REMOVE: Partial<TuiDialogOptions<TuiPromptData>> = {
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
}: NetworkInfo): Promise<CT.InputSpec> {
  const start9ToDomain = `${start9ToSubdomain?.value}.start9.to`
  const base = start9ToSubdomain ? { [start9ToDomain]: start9ToDomain } : {}

  const values = domains.reduce((prev, curr) => {
    return {
      [curr.value]: curr.value,
      ...prev,
    }
  }, base)

  return configBuilderToSpec(
    CB.Config.of({
      domain: CB.Value.select({
        name: 'Domain',
        required: { default: null },
        values,
      }),
      subdomain: CB.Value.text({
        name: 'Subdomain',
        required: false,
      }),
    }),
  )
}

export type AddressDetails = {
  label?: string
  url: string
}

export function getAddresses(
  serviceInterface: T.ServiceInterfaceWithHostInfo,
): {
  clearnet: AddressDetails[]
  local: AddressDetails[]
  tor: AddressDetails[]
} {
  const host = serviceInterface.hostInfo
  const addressInfo = serviceInterface.addressInfo
  const username = addressInfo.username ? addressInfo.username + '@' : ''
  const suffix = addressInfo.suffix || ''

  const hostnames =
    host.kind === 'multi'
      ? host.hostnames
      : host.hostname
        ? [host.hostname]
        : []

  const clearnet: AddressDetails[] = []
  const local: AddressDetails[] = []
  const tor: AddressDetails[] = []

  hostnames.forEach(h => {
    let scheme = ''
    let port = ''

    if (h.hostname.sslPort) {
      port = h.hostname.sslPort === 443 ? '' : `:${h.hostname.sslPort}`
      scheme = addressInfo.bindOptions.addSsl?.scheme
        ? `${addressInfo.bindOptions.addSsl.scheme}://`
        : ''
    }

    if (h.hostname.port) {
      port = h.hostname.port === 80 ? '' : `:${h.hostname.port}`
      scheme = addressInfo.bindOptions.scheme
        ? `${addressInfo.bindOptions.scheme}://`
        : ''
    }

    if (h.kind === 'onion') {
      tor.push({
        label: h.hostname.sslPort ? 'HTTPS' : 'HTTP',
        url: toHref(scheme, username, h.hostname.value, port, suffix),
      })
    } else {
      const hostnameKind = h.hostname.kind

      if (hostnameKind === 'domain') {
        tor.push({
          url: toHref(
            scheme,
            username,
            `${h.hostname.subdomain}.${h.hostname.domain}`,
            port,
            suffix,
          ),
        })
      } else {
        local.push({
          label:
            hostnameKind === 'local'
              ? 'Local'
              : `${h.networkInterfaceId} (${hostnameKind})`,
          url: toHref(scheme, username, h.hostname.value, port, suffix),
        })
      }
    }
  })

  return {
    clearnet,
    local,
    tor,
  }
}

function toHref(
  scheme: string,
  username: string,
  hostname: string,
  port: string,
  suffix: string,
): string {
  return `${scheme}${username}${hostname}${port}${suffix}`
}
