import { NonNullableFormBuilder, Validators } from '@angular/forms'
import { FormRawValue } from 'src/app/services/form.service'

export const FIRST_OCTETS = [192, 10, 172] as const

export type FirstOctet = (typeof FIRST_OCTETS)[number]

// Second octet is determined by first octet (standard private ranges)
export const SECOND_OCTET_MAP: Record<FirstOctet, number> = {
  192: 168, // 192.168.x.x (Class C private)
  10: 0, // 10.0.x.x (Class A private)
  172: 16, // 172.16.x.x (Class B private)
}

export const LAN_IPV4_VALIDATION_ERRORS = {
  required: 'Required',
  min: ({ min }: { min: number }) => `Minimum value is ${min}`,
  max: ({ max }: { max: number }) => `Maximum value is ${max}`,
}

export function getSecondOctet(first: FirstOctet): number {
  return SECOND_OCTET_MAP[first]
}

export function getLanIpv4Form(builder: NonNullableFormBuilder) {
  return builder.group({
    ip: builder.group({
      firstOctet: builder.control<FirstOctet>(192),
      // TODO @matt - remove routerOctet? The 4th octet in ip.ts was changed
      // from configurable to a static "1" — it's always 1 by convention, and
      // making it configurable would complicate DHCP ranges and break user
      // expectations. That leaves routerOctet (3rd octet) as the only
      // editable part of the gateway IP here, but it duplicates the Admin
      // Security Profile's subnet field which sets the same value. It likely
      // makes sense to remove this form entirely and let the Admin Profile
      // be the single source of truth for the 3rd octet.
      routerOctet: builder.control(1, [
        Validators.required,
        Validators.min(0),
        Validators.max(254),
      ]),
    }),
  })
}

export type LanIpv4Form = FormRawValue<ReturnType<typeof getLanIpv4Form>>

export function buildNetworkBlock(ip: LanIpv4Form['ip']): string {
  const second = getSecondOctet(ip.firstOctet)
  return `${ip.firstOctet}.${second}.0.0/16`
}

export function buildRouterIp(ip: LanIpv4Form['ip']): string {
  const second = getSecondOctet(ip.firstOctet)
  return `${ip.firstOctet}.${second}.${ip.routerOctet}.1`
}

export function parseIpToForm(ipAddr: string): LanIpv4Form {
  const [first, , third] = ipAddr.split('.').map(Number)

  return {
    ip: {
      firstOctet: (FIRST_OCTETS.includes(first as FirstOctet)
        ? first
        : 192) as FirstOctet,
      routerOctet: third ?? 1,
    },
  }
}
