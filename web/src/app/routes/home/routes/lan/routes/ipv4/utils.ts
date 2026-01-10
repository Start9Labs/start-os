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
      thirdOctet: builder.control(0, [
        Validators.required,
        Validators.min(0),
        Validators.max(255),
      ]),
      routerOctet: builder.control(1, [
        Validators.required,
        Validators.min(1),
        Validators.max(254),
      ]),
    }),
  })
}

export type LanIpv4Form = FormRawValue<ReturnType<typeof getLanIpv4Form>>

export function buildFullIp(ip: LanIpv4Form['ip']): string {
  const second = getSecondOctet(ip.firstOctet)
  return `${ip.firstOctet}.${second}.${ip.thirdOctet}.${ip.routerOctet}`
}

export function parseIpToForm(ipAddr: string): LanIpv4Form {
  const [first, , third, fourth] = ipAddr.split('.').map(Number)

  return {
    ip: {
      firstOctet: (FIRST_OCTETS.includes(first as FirstOctet)
        ? first
        : 192) as FirstOctet,
      thirdOctet: third ?? 0,
      routerOctet: fourth ?? 1,
    },
  }
}
