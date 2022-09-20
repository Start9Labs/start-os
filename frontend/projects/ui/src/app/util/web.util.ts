export function strip(html: string) {
  let doc = new DOMParser().parseFromString(html, 'text/html')
  return doc.body.textContent || ''
}

export function getUrlDomain(url: string): string | null {
  const matches = url.match(/^https?\:\/\/([^\/:?#]+)(?:[\/:?#]|$)/i)
  return matches && matches[1]
}
