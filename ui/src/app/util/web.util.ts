export async function copyToClipboard (str: string): Promise<boolean> {
  if (window.isSecureContext) {
    return navigator.clipboard.writeText(str)
      .then(() => {
        return true
      })
      .catch(err => {
        return false
      })
  } else {
    const el = document.createElement('textarea')
    el.value = str
    el.setAttribute('readonly', '')
    el.style.position = 'absolute'
    el.style.left = '-9999px'
    document.body.appendChild(el)
    el.select()
    const copy = document.execCommand('copy')
    document.body.removeChild(el)
    return copy
  }
}
