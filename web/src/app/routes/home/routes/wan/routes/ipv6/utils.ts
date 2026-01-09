import { NonNullableFormBuilder } from '@angular/forms'
import { getDnsForm } from '../../dns/utils'
import { CustomValidators } from 'src/app/utils/validators'
import { FormRawValue } from 'src/app/services/form.service'

export const IPV6_MODES = [
  'slaac',
  'dhcpv6',
  '6rd',
  'static',
  'disabled',
] as const

export const IPV6_SLAAC_CONTROLS = ['prefix'] as const
export const IPV6_SIXRD_CONTROLS = ['prefix', 'ip4', 'mask', 'border'] as const
export const IPV6_STATIC_CONTROLS = ['wan', 'prefix', 'gateway'] as const

export const IPV6_LABELS: Record<
  | Ipv6Mode
  | (typeof IPV6_SLAAC_CONTROLS)[number]
  | (typeof IPV6_SIXRD_CONTROLS)[number]
  | (typeof IPV6_STATIC_CONTROLS)[number],
  string
> = {
  slaac: 'SLAAC',
  dhcpv6: 'DHCPv6',
  '6rd': '6RD',
  static: 'Static',
  disabled: 'Disabled',
  prefix: 'IPv6 Prefix*',
  ip4: 'IPv4 Gateway IP*',
  mask: 'IPv4 Mask*',
  border: 'IPv4 Border Router IP*',
  wan: 'WAN IP*',
  gateway: 'Gateway IP*',
}

export type Ipv6Mode = (typeof IPV6_MODES)[number]

export function getWanIpv6Form(builder: NonNullableFormBuilder) {
  return builder.group({
    ip: builder.group({
      mode: builder.control<Ipv6Mode>('slaac'),
      wan: builder.control(''),
      prefix: builder.control(''),
      gateway: builder.control(''),
      ip4: builder.control(''),
      mask: builder.control(''),
      border: builder.control(''),
    }),
    dns: getDnsForm(builder, [CustomValidators.ipv4OrIpv6()]),
  })
}

export type WanIpv6Form = FormRawValue<ReturnType<typeof getWanIpv6Form>>
