import { NonNullableFormBuilder, Validators } from '@angular/forms'
import { CustomValidators } from 'src/app/utils/validators'
import { FormRawValue } from 'src/app/services/form.service'

export const IPV6_MODES = [
  'slaac',
  'dhcpv6',
  'static',
  '6rd',
  'disabled',
] as const

export const IPV6_SLAAC_CONTROLS = ['prefix'] as const
export const IPV6_DHCPV6_CONTROLS = ['prefix'] as const
export const IPV6_STATIC_CONTROLS = ['wan', 'prefix', 'gateway'] as const
export const IPV6_SIXRD_CONTROLS = ['prefix', 'ip4', 'mask', 'border'] as const

export const IPV6_LABELS: Record<
  | Ipv6Mode
  | (typeof IPV6_STATIC_CONTROLS)[number]
  | (typeof IPV6_SIXRD_CONTROLS)[number],
  string
> = {
  slaac: 'SLAAC',
  dhcpv6: 'DHCPv6',
  static: 'Static',
  '6rd': '6RD',
  disabled: 'Disabled',
  wan: 'WAN IP',
  prefix: 'IPv6 Prefix',
  gateway: 'Gateway IP',
  ip4: 'IPv4 Gateway IP',
  mask: 'IPv4 Prefix Length',
  border: 'Border Router IP',
}

export const IPV6_VALIDATION_ERRORS = {
  required: 'Required',
  ipv4: 'Enter a valid IPv4 address',
  ipv6: 'Enter a valid IPv6 address',
  prefix: ({ min, max }: { min: number; max: number }) =>
    `Enter a value between ${min} and ${max}`,
}

export type Ipv6Mode = (typeof IPV6_MODES)[number]

export function getWanIpv6Form(builder: NonNullableFormBuilder) {
  return builder.group({
    ip: builder.group({
      mode: builder.control<Ipv6Mode>('slaac'),
      // Static/SLAAC/DHCPv6 fields
      wan: builder.control('', [CustomValidators.ipv6()]),
      prefix: builder.control('', [CustomValidators.prefix(0, 128)]),
      gateway: builder.control('', [CustomValidators.ipv6()]),
      // 6RD fields
      ip4: builder.control('', [CustomValidators.ipv4()]),
      mask: builder.control('', [CustomValidators.prefix(0, 32)]),
      border: builder.control('', [CustomValidators.ipv4()]),
    }),
  })
}

export type WanIpv6Form = FormRawValue<ReturnType<typeof getWanIpv6Form>>

export function updateIpv6Validators(
  form: ReturnType<typeof getWanIpv6Form>,
  mode: Ipv6Mode,
): void {
  const ip = form.controls.ip.controls

  // Clear all mode-specific required validators
  ip.wan.clearValidators()
  ip.prefix.clearValidators()
  ip.gateway.clearValidators()
  ip.ip4.clearValidators()
  ip.mask.clearValidators()
  ip.border.clearValidators()

  // Re-add format validators
  ip.wan.addValidators([CustomValidators.ipv6()])
  ip.prefix.addValidators([CustomValidators.prefix(0, 128)])
  ip.gateway.addValidators([CustomValidators.ipv6()])
  ip.ip4.addValidators([CustomValidators.ipv4()])
  ip.mask.addValidators([CustomValidators.prefix(0, 32)])
  ip.border.addValidators([CustomValidators.ipv4()])

  // Add required validators based on mode
  if (mode === 'static') {
    ip.wan.addValidators([Validators.required])
    ip.prefix.addValidators([Validators.required])
    ip.gateway.addValidators([Validators.required])
  } else if (mode === '6rd') {
    ip.prefix.addValidators([Validators.required])
    ip.ip4.addValidators([Validators.required])
    ip.mask.addValidators([Validators.required])
    ip.border.addValidators([Validators.required])
  }

  // Update validity
  ip.wan.updateValueAndValidity()
  ip.prefix.updateValueAndValidity()
  ip.gateway.updateValueAndValidity()
  ip.ip4.updateValueAndValidity()
  ip.mask.updateValueAndValidity()
  ip.border.updateValueAndValidity()
}
