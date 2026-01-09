import { NonNullableFormBuilder, ValidatorFn } from '@angular/forms'
import { NetworkInterfaceSection } from 'src/app/services/api/types'
import { FormRawValue } from 'src/app/services/form.service'

export const DNS_MODES = ['isp', 'custom'] as const

export type DnsMode = (typeof DNS_MODES)[number]

export const LABELS: Record<DnsMode, string> = {
  isp: 'Get from ISP',
  custom: 'Custom',
}

export function getDnsForm(
  builder: NonNullableFormBuilder,
  validators: ValidatorFn[],
) {
  return builder.group({
    mode: builder.control<DnsMode>('isp'),
    custom1: builder.control('', validators),
    custom2: builder.control('', validators),
    custom1Tls: builder.control(false),
    custom2Tls: builder.control(false),
  })
}

export type DnsForm = FormRawValue<ReturnType<typeof getDnsForm>>

export function parseDnsServer(server: string): { ip: string; tls: boolean } {
  if (server.includes('@853') || server.includes('#853')) {
    return {
      ip: server.split(/[@#]/)[0],
      tls: true,
    }
  }
  return {
    ip: server,
    tls: false,
  }
}

export function parseDnsFromInterface(iface: NetworkInterfaceSection): DnsForm {
  const dnsServers = iface.lists.dns || []
  const peerdns = iface.options.peerdns

  if (peerdns === '0' && dnsServers.length > 0) {
    const server1 = parseDnsServer(dnsServers[0])
    const server2 = dnsServers[1]
      ? parseDnsServer(dnsServers[1])
      : { ip: '', tls: false }

    return {
      mode: 'custom',
      custom1: server1.ip,
      custom2: server2.ip,
      custom1Tls: server1.tls,
      custom2Tls: server2.tls,
    }
  }

  return {
    mode: 'isp',
    custom1: '',
    custom2: '',
    custom1Tls: false,
    custom2Tls: false,
  }
}

export function applyDnsToInterface(
  dns: DnsForm,
  iface: NetworkInterfaceSection,
): void {
  if (dns.mode === 'isp') {
    iface.options.peerdns = '1'
    iface.lists.dns = []
  } else if (dns.mode === 'custom') {
    iface.options.peerdns = '0'
    const servers: string[] = []
    if (dns.custom1) {
      servers.push(dns.custom1Tls ? `${dns.custom1}@853` : dns.custom1)
    }
    if (dns.custom2) {
      servers.push(dns.custom2Tls ? `${dns.custom2}@853` : dns.custom2)
    }
    iface.lists.dns = servers
  }
}
