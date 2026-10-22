export const OS_UPDATE_SIGNERS = [
  `-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAqPjOeMD2CB3UEASoICgfJlfrO5ygjOSBCCNRyDBs75A=
-----END PUBLIC KEY-----
`,
  `-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEASv7fB+rs6FRL+pk7KXaoXONNn26dRkwMB9LXUqmXXLM=
-----END PUBLIC KEY-----
`,
  // Ed25519 primary key from apt/start9.gpg.
  `-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAJdDg62o3oHIh4lXECn9gqORe+cCBbH/67MWjMxIMzDg=
-----END PUBLIC KEY-----
`,
]

const normalizeKey = (key: string) => key.replace(/\s/g, '')
const trustedSigners = new Set(OS_UPDATE_SIGNERS.map(normalizeKey))

export function trustedOsUpdateSigners(signers: string[]): string[] {
  return signers.filter(signer => trustedSigners.has(normalizeKey(signer)))
}
