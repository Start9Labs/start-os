export function toAcmeName(url: ACME_URL | string | null): ACME_Name | string {
  return (
    Object.entries(knownACME).find(([_, val]) => val === url)?.[0] ||
    url ||
    'System CA'
  )
}

export function toAcmeUrl(name: ACME_Name | string): ACME_URL | string {
  return knownACME[name as ACME_Name] || name
}

export const knownACME = {
  'Let\'s Encrypt': 'https://acme-v02.api.letsencrypt.org/directory',
  'Let\'s Encrypt (Staging)':
    'https://acme-staging-v02.api.letsencrypt.org/directory',
}

export type ACME_Name = keyof typeof knownACME

export type ACME_URL = (typeof knownACME)[ACME_Name]
