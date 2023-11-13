export async function copyToClipboard(str: string): Promise<boolean> {
  if (window.isSecureContext) {
    return navigator.clipboard
      .writeText(str)
      .then(() => true)
      .catch(() => false)
  }

  const el = document.createElement('textarea')
  el.value = str
  el.setAttribute('readonly', '')
  el.style.position = 'absolute'
  el.style.left = '-9999px'
  document.body.appendChild(el)
  el.select()
  const didCopy = document.execCommand('copy')
  document.body.removeChild(el)
  return didCopy
}
