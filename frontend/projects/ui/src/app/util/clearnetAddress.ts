import { Clearnet } from '../services/patch-db/data-model'

export function getClearnetAddress(protocol: string, clearnet: Clearnet) {
  return `${protocol}://${clearnet?.subdomain}.${clearnet?.domain}`
}
