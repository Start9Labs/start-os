export const OS_UPDATE_SIGNERS = [
  `-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAgpcI3hOExv1njCt+zJoYBartdyk4SKiqSIy3+9tcpOI=
-----END PUBLIC KEY-----
`,
  `-----BEGIN PUBLIC KEY-----
MCowBQYDK2VwAyEAfeubYt/ozIfZspUZdmu2Fa6sK2psMnHVGxuVolo8u40=
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
