export function toAcmeName(url: string | null): string | 'System CA' {
  return knownACME.find(acme => acme.url === url)?.name || url || 'System CA'
}

export function toAcmeUrl(name: string): string {
  return knownACME.find(acme => acme.name === name)?.url || name
}

export const knownACME: {
  name: string
  url: string
}[] = [
  {
    name: `Let's Encrypt`,
    url: 'https://acme-v02.api.letsencrypt.org/directory',
  },
  {
    name: `Let's Encrypt (Staging)`,
    url: 'https://acme-staging-v02.api.letsencrypt.org/directory',
  },
]
