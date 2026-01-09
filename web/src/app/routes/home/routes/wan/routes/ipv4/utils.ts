import { NonNullableFormBuilder, Validators } from '@angular/forms'
import { FormRawValue } from 'src/app/services/form.service'
import { getDnsForm } from '../../dns/utils'
import { CustomValidators } from 'src/app/utils/validators'

export const IPV4_MODES = ['dhcp', 'pppoe', 'static'] as const

export const IPV4_PPPOE_CONTROLS = ['wan', 'password', 'device'] as const
export const IPV4_STATIC_CONTROLS = ['wan', 'prefix', 'gateway'] as const

export const EXTRA = ['mask']

export const IPV4_LABELS: Record<
  | Ipv4Mode
  | (typeof IPV4_PPPOE_CONTROLS)[number]
  | (typeof IPV4_STATIC_CONTROLS)[number]
  | (typeof EXTRA)[number],
  string
> = {
  dhcp: 'DHCP',
  pppoe: 'PPPoE',
  static: 'Static',
  wan: 'WAN IP*',
  password: 'Password*',
  device: 'Device*',
  prefix: 'Subnet Prefix*',
  gateway: 'Gateway IP*',
  mask: 'Subnet Mask',
}

export type Ipv4Mode = (typeof IPV4_MODES)[number]

export function getWanIpv4Form(builder: NonNullableFormBuilder) {
  return builder.group({
    ip: builder.group({
      mode: builder.control<Ipv4Mode>('dhcp'),
      wan: builder.control('', Validators.required),
      prefix: builder.control(''),
      gateway: builder.control(''),
      password: builder.control(''),
      device: builder.control(''),
    }),
    dns: getDnsForm(builder, [CustomValidators.ipv4()]),
  })
}

export type WanIpv4Form = FormRawValue<ReturnType<typeof getWanIpv4Form>>

export function prefixFromNetmask(netmask: string | undefined): string {
  if (!netmask) return ''

  // Convert netmask to CIDR prefix
  const parts = netmask.split('.').map(Number)
  const binary = parts.map(p => p.toString(2).padStart(8, '0')).join('')
  const prefix = binary.split('1').length - 1

  return `/${prefix}`
}

export function netmaskFromPrefix(prefix: string): string {
  // Remove leading slash if present
  const prefixNum = parseInt(prefix.replace('/', ''), 10)

  if (isNaN(prefixNum) || prefixNum < 0 || prefixNum > 32) {
    return ''
  }

  // Create a 32-bit mask
  const mask = (0xffffffff << (32 - prefixNum)) >>> 0

  // Convert to dotted decimal notation
  const octet1 = (mask >>> 24) & 0xff
  const octet2 = (mask >>> 16) & 0xff
  const octet3 = (mask >>> 8) & 0xff
  const octet4 = mask & 0xff

  return `${octet1}.${octet2}.${octet3}.${octet4}`
}
