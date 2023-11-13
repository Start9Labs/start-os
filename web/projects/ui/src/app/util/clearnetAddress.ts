import { DomainInfo } from '../services/patch-db/data-model'

export function getClearnetAddress(
  protocol: string,
  domainInfo: DomainInfo | null,
  path = '',
) {
  if (!domainInfo) return ''
  const subdomain = domainInfo.subdomain ? `${domainInfo.subdomain}.` : ''
  return `${protocol}://${subdomain}${domainInfo.domain}${path}`
}
