export function isIOSSafari(): boolean {
  const ua = navigator.userAgent
  return (
    /iPad|iPhone|iPod/.test(ua) ||
    (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1)
  )
}

export const ROOT_CA_DOWNLOAD_HREF = isIOSSafari()
  ? '/static/local-root-ca.mobileconfig'
  : '/static/local-root-ca.crt'
