export function toAuthorityName(url: string | null): string | 'Local Root CA' {
  return (
    knownAuthorities.find(ca => ca.url === url)?.name || url || 'Local Root CA'
  )
}

export function toAuthorityUrl(name: string): string {
  return knownAuthorities.find(ca => ca.name === name)?.url || name
}

export const knownAuthorities = [
  {
    name: `Let's Encrypt`,
    url: 'https://acme-v02.api.letsencrypt.org/directory',
  },
  {
    name: `Let's Encrypt (Staging)`,
    url: 'https://acme-staging-v02.api.letsencrypt.org/directory',
  },
] as const
